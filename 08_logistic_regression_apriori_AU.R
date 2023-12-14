#---
#**Aim:** 
#*"Perform Bayesian logistic regression analysis exploring the relations between 
#*the activation intensity of automatically detected action units (AUs) and 
#*manually coded facial expression categories. Single-model inference is followed 
#*by model averaging for parameter selection."
#---

# Libraries ----
#install.packages("igraph", type = "binary") # install binary if the installation for package 'brms' fails
library(BAS) # Bayesian parameter selection through model averaging
library(brms)  # fitting Bayesian multilevel logistic model
library(tidybayes) # for analysis of posterior draws of a Bayesian model
library(ROCR) # fitting ROC curves
library(corrplot) # correlation plots
library(dplyr) # data wrangling & manipulation
library(readxl) # reading excel data
library(magrittr) # For operators (data manipulation/programming flow)
library(ggplot2) # plotting
library(caret) # confusion matrices

rstan::rstan_options(auto_write = TRUE) # for package 'brms'

# Read data ----

# A function checking whether the automatically analyzed data with or without
# smoothing are being used, based on which to input and output paths are specified. 
if (!exists("analysis_type")){
  analysis_type <- "smooth"
} else if (!analysis_type %in% c("smooth", "raw")) {
  stop("Illegal value for analysis type. Use 'smooth' or 'raw'.")
}

# Based on the dataset, specify input and output paths. 
bfr_data_path <- paste0('output/', analysis_type, '/BFR_OBXT_valid_data.rds')

## Prepare data ----

## Read in, recode, and center variables for the model predicting manually coded 
# facial expression categories from the intensity of activation in apriori 
# Action Unit combinations.Make one primary dataframe, from which data are subset
# for specific analyses.
data_mlr <- readr::read_rds(bfr_data_path) %>%
  # Filter out events that are manually coded as "Not Visible". Missing data 
  # resulting from the automatic analysis with BFR9 were already filtered out 
  # in the ROC analysis script. 
  dplyr::filter(FE_category != "Not Visible") %>%
  dplyr::mutate(manual_valence = factor(FE_category, 
                                        levels = c('Negative', 'Neutral', 'Positive'), 
                                        ordered = TRUE), 
                # Create a new manual valence variable with factors: Positive vs. Negative/Neutral. 
                manual_valence_pos_rest = recode(FE_category, 
                                                 Negative = 'Rest', Neutral = 'Rest'),
                manual_valence_pos_rest = factor(manual_valence_pos_rest, 
                                                 levels = c('Rest', 'Positive'), 
                                                 ordered = TRUE),
                # Convert participant ID to factor.
                participant_id = factor(participant_id)) %>%
  dplyr::mutate(manual_valence_binary = dplyr::if_else(manual_valence_pos_rest == "Positive", 1, 0), 
                # Coarse scale of mouth opening - sum up all AU's involved in mouth opening
                AU_mouth_opening = (AU25 + AU26 + AU27)) %>%
  dplyr::select(manual_valence, FE_category, 
                manual_valence_pos_rest, age,
                AU12, AU6, AU25, AU26, AU27,
                AU17, AU20, AU3_4, AU7, AU_mouth_opening,
                participant_id, participant_code) %>%
  # Center the AU variables to the grand mean (and not the infant's mean).
  dplyr::mutate(AU6_AU7 = (AU6 + AU7)/2,
                dplyr::across(dplyr::starts_with('AU'), ~ .x - mean(.x)),
         age_8 = dplyr::if_else(age == 4, 0, 1),
         age_8 = factor(age_8, levels = c(0, 1), 
                        labels = c(4, 8), 
                        ordered = TRUE)) %>% 
  dplyr::select(-age) 

# Check whether FE_category is successfully recoded into manual_valence.
dplyr::count(data_mlr, manual_valence)
dplyr::count(data_mlr, FE_category)
dplyr::count(data_mlr, manual_valence_pos_rest)
dplyr::count(data_mlr, AU_mouth_opening)

# Check whether there is any missing data.
data_mlr %>% dplyr::summarise(dplyr::across(dplyr::everything(), 
                                            ~ sum(is.na(.x))))

# Plot the distributions per age of the ordered AU_mouth_opening variable.
data_mlr %>%
  ggplot2::ggplot(aes(x = AU_mouth_opening, fill = age_8)) +
  ggplot2::geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity', stat = "bin") +
  ggplot2::scale_fill_manual(values = c("#69b3a2", "#404080"), name = "Age") +
  hrbrthemes::theme_ipsum() +
  ggplot2::theme(
    axis.title.x = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14, hjust = 0.5)) +
  ggplot2::xlab("Sum Value of AU25, AU26, and AU27 [Coarse Scale of Mouth Opening], Grand-Mean Centered") + 
  ggplot2::ylab("Count Time Samples (15Hz)") + 
  ggplot2::labs(fill = "")

# Write to rds. 
saveRDS(data_mlr, file = paste0("output/", analysis_type, "/mlr_analysis/brf9_mlr_data.rds"))

# Write to csv. 
readr::write_csv2(data_mlr, 
                  file = paste0("output/", analysis_type, "/mlr_analysis/brf9_mlr_data.csv"), 
                  na = "NA")

# Prepare data for comparing negative vs. neutral manually coded facial expressions. 
data_mlr_neg <- data_mlr %>% 
  dplyr::filter(manual_valence != 'Positive') %>%
  dplyr::select(manual_valence, AU17, AU20, AU3_4, AU6, AU_mouth_opening) %>%
  # Assign inverse class weights
  dplyr::mutate(class_weights = dplyr::case_when(
    manual_valence == "Negative" ~ 92, 
    manual_valence == "Neutral" ~ 8))

# Compute class weights. 
# Negative = 21676/275449 = 0.08
# I.e., the penalty for incorrectly classifying negative as neutral is 92 times as
# severe as the other way around. 
data_mlr_neg %>% dplyr::count(manual_valence)

# Check inverse class weights assignment.
data_mlr_neg %>% dplyr::count(class_weights)

# Prepare data for comparing positive vs. other manually coded facial expressions. 
data_mlr <- data_mlr %>% dplyr::select(manual_valence_pos_rest,
                         AU12, AU6, AU25, AU26, AU27,
                         AU17, AU20, AU3_4, AU7, AU_mouth_opening,
                         participant_id, participant_code) %>%
            # Assign inverse class weights
            dplyr::mutate(class_weights = dplyr::case_when(
              manual_valence_pos_rest == "Positive" ~ 55, 
              manual_valence_pos_rest == "Rest" ~ 45))

# Compute class weights. 
# Positive = 133207/297125 = 0.45
data_mlr %>% dplyr::count(manual_valence_pos_rest)

# Check inverse class weights assignment.
data_mlr %>% dplyr::count(class_weights)

## Mouth opening intensity ----

# Alternative ways to create an ordered factor of mouth opening representing 
# activation intensity using 1) 20% Quantiles, or 2) FACS intensity cutoffs from:
# Ekman, P., Friesen, W. V., & Hager, J. C. (2002). Facial Action Coding System: 
# Facial action coding system: the manual: on CD-ROM. Research Nexus.

# FACS intenstity cutoffs:
# Not active - [0.00 - 0.100]
#  A - [0.100 - 0.217]
#  B - [0.217 - 0.334]
#  C - [0.334 - 0.622]
#  D - [0.622 - 0.910]
#  E - [0.910 - 1.000]

# Mouth Opening composite usign 20% Qunatiles:

# # Get the quantile values partitioning the Action Units intensities for mouth opening in 20% segments. 
# mouth_opening <- data_mlr %>% 
#   summarize(AU25_q = stats::quantile(AU25, probs = seq(0, 1, 1/5), type = 7, names = FALSE), 
#             AU26_q = stats::quantile(AU26, probs = seq(0, 1, 1/5), type = 7, names = FALSE),
#             AU27_q = stats::quantile(AU27, probs = seq(0, 1, 1/5), type = 7, names = FALSE)) 
# 
# # Create an ordered factor "AU_mouth_opening" depending on the quantile in which activation in AU25, AU26 and AU27 falls.  
# data_mlr <- data_mlr %>%
#   dplyr::mutate(AU_mouth_opening = dplyr::case_when(((AU25 > AU26) & (AU25 > AU27)) & (AU25 < mouth_opening$AU25_q[2]) ~ 1, 
#                                                     ((AU25 > AU26) & (AU25 > AU27)) & (AU25 >= mouth_opening$AU25_q[2]) & (AU25 < mouth_opening$AU25_q[3]) ~ 2, 
#                                                     ((AU25 > AU26) & (AU25 > AU27)) & (AU25 >= mouth_opening$AU25_q[3]) & (AU25 < mouth_opening$AU25_q[4]) ~ 3, 
#                                                     ((AU25 > AU26) & (AU25 > AU27)) & (AU25 >= mouth_opening$AU25_q[4]) & (AU25 < mouth_opening$AU25_q[5]) ~ 4, 
#                                                     ((AU25 > AU26) & (AU25 > AU27)) & (AU25 >= mouth_opening$AU25_q[5]) & (AU25 <= mouth_opening$AU25_q[6]) ~ 5, 
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 < mouth_opening$AU26_q[2]) ~ 6, 
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 >= mouth_opening$AU26_q[2]) & (AU26 < mouth_opening$AU26_q[3]) ~ 7,
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 >= mouth_opening$AU26_q[3]) & (AU26 < mouth_opening$AU26_q[4]) ~ 8, 
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 >= mouth_opening$AU26_q[4]) & (AU26 < mouth_opening$AU26_q[5]) ~ 9, 
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 >= mouth_opening$AU26_q[5]) & (AU26 <= mouth_opening$AU26_q[6]) ~ 10, 
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 < mouth_opening$AU27_q[2]) ~ 11, 
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 >= mouth_opening$AU27_q[2]) & (AU27 < mouth_opening$AU27_q[3]) ~ 12,
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 >= mouth_opening$AU27_q[3]) & (AU27 < mouth_opening$AU27_q[4]) ~ 13, 
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 >= mouth_opening$AU27_q[4]) & (AU27 < mouth_opening$AU27_q[5]) ~ 14, 
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 >= mouth_opening$AU27_q[5]) & (AU27 <= mouth_opening$AU27_q[6]) ~ 15),
#                 AU_mouth_opening = factor(AU_mouth_opening, levels = seq(0, 15, 1), 
#                                           ordered = TRUE))

# FACS intensity cutoffs.
# Create an ordered factor "AU_mouth_opening" based on FACS intensity cutoffs.  
# data_mlr <- data_mlr %>%
#   dplyr::mutate(AU_mouth_opening = dplyr::case_when(((AU25 > AU26) & (AU25 > AU27)) & (AU25 < 0.217) ~ 1, 
#                                                     ((AU25 > AU26) & (AU25 > AU27)) & (AU25 >= 0.217) & (AU25 < 0.334) ~ 2, 
#                                                     ((AU25 > AU26) & (AU25 > AU27)) & (AU25 >= 0.334) & (AU25 < 0.622) ~ 3, 
#                                                     ((AU25 > AU26) & (AU25 > AU27)) & (AU25 >= 0.622) & (AU25 < 0.910) ~ 4, 
#                                                     ((AU25 > AU26) & (AU25 > AU27)) & (AU25 >= 0.910) & (AU25 <= 1) ~ 5, 
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 < 0.217) ~ 6, 
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 >= 0.217) & (AU26 < 0.334) ~ 7,
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 >= 0.334) & (AU26 < 0.622) ~ 8, 
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 >= 0.622) & (AU26 < 0.910) ~ 9, 
#                                                     ((AU26 >= AU25) & (AU26 > AU27)) & (AU26 >= 0.910) & (AU26 <= 1) ~ 10, 
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 < 0.217) ~ 11, 
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 >= 0.217) & (AU27 < 0.334) ~ 12,
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 >= 0.334) & (AU27 < 0.622) ~ 13, 
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 >= 0.622) & (AU27 < 0.910) ~ 14, 
#                                                     ((AU27 >= AU25) & (AU27 >= AU26)) & (AU27 >= 0.910) & (AU27 <= 1) ~ 15),
#                 AU_mouth_opening = factor(AU_mouth_opening, levels = seq(0, 15, 1), 
#                                           ordered = TRUE))

## Correlation matrix ----
# Make sure that the correlations among model predictors are not too high 
# AU12 and AU6 correlate at about .5; AU6 & AU12*AU6 correlate at about .6
AU_corr <- data_mlr %>% 
  dplyr::mutate(AU12_AU6_int = (AU12*AU6), 
         # Compute composites & interaction effects for positive facial expressions.
         AU12_mouth_opening_int = (AU12*AU_mouth_opening),
         # Compute composites & interaction effects for negative facial expressions.
         AU17_AU3_4_int = AU17*AU3_4,
         AU20_AU3_4_int = AU20*AU3_4,
         AU20_AU6_int = (AU20*AU6),
         AU20_mouth_opening_int = (AU20*AU_mouth_opening)) %>%
  dplyr::select(dplyr::starts_with('AU')) 

# Correlations positive AU's
AU_corr_pos <- AU_corr %>% 
  dplyr::select(AU12, AU6, AU7, AU25, AU26, AU27, AU_mouth_opening, 
         AU12_AU6_int, AU12_mouth_opening_int) %>%
  stats::cor(.)

# Compute sig. level
pval <- psych::corr.test(AU_corr, adjust = "none")$p

corrplot::corrplot(AU_corr_pos, method = "number", type = "upper", 
                   p.mat = pval, 
                   sig.level = 0, insig = c("p-value"))

# Save correlation matrix to .csv.
readr::write_csv2(tibble::as_tibble(AU_corr_pos["corrPos"["corr"]], 
                                    tibble::as_tibble_col(AU_corr_pos["corrPos"["yName"]])), 
                  file = paste0("output/", analysis_type, "/mlr_analysis/AU_pos_correlations.csv"), 
                  na = "NA")

# Correlations negative AU's
AU_corr_neg <- AU_corr %>% 
  dplyr::select(AU17, AU20, AU3_4, 
                AU6, AU7, AU6_AU7,
                AU25, AU26, AU27, AU_mouth_opening,
                AU17_AU3_4_int, AU20_AU3_4_int, 
                AU20_AU6_int, AU20_mouth_opening_int) %>%
  stats::cor(.) 

cor.test(AU_corr$AU3_4, AU_corr$AU17_AU3_4_int, alternative = "two.sided", method = "pearson")

corrplot::corrplot(AU_corr_neg, method = "number", type = "upper", sig.level = 0.05)

# Save correlation matrix to .csv.
readr::write_csv2(tibble::as_tibble(AU_corr_pos["corrNeg"["corr"]], 
                                    tibble::as_tibble_col(AU_corr_pos["corrNeg"["yName"]])), 
                  file = paste0("output/", analysis_type, "/mlr_analysis/AU_neg_correlations.csv"), 
                  na = "NA")

# Bayesian Logistic Regression: Single-Model Inference ----

## Model settings ----
nlogical_cores <- parallel::detectCores(logical = FALSE) - 2
chains <- nlogical_cores * 2
warmups_per_chain <- 500
iterations_post_warmup <- 6000 
iterations <- ceiling((iterations_post_warmup/chains) + warmups_per_chain)
max_tree_depth <- 20

## Model formula's ----

# Random-intercept-only model
ri_form <- stats::formula(manual_valence_pos_rest ~ (1|participant_id))

# Check which model parameters can have priors. 
ri_model_priors <- brms::get_prior(stats::formula(ri_form),
                                   data = data_mlr, 
                                   family = binomial(link = "logit"))

# Positive Affect: Positive apriori AU's main effects without random intercept.
# AU equation with mouth opening (AU25, AU26, and AU27) composite.
AU_main_pos_form <- stats::formula(manual_valence_pos_rest | resp_weights(class_weights) ~ AU12 + 
                                    AU6 + AU_mouth_opening)

# Check which model parameters can have priors. 
AU_main_pos_priors <- brms::get_prior(stats::formula(AU_main_pos_form),
                                     data = data_mlr, 
                                     family = binomial(link = "logit"))

# Positive Affect: Positive apriori AU's equation without random intercept.
# AU equation with mouth opening (AU25, AU26, and AU27) composite.
AU_int_pos_form <- stats::formula(manual_valence_pos_rest | resp_weights(class_weights) ~ AU12 + 
                                AU6 + AU_mouth_opening + 
                                AU12*AU6 + AU12*AU_mouth_opening)

# Check which model parameters can have priors. 
AU_int_pos_priors <- brms::get_prior(stats::formula(AU_int_pos_form),
                                 data = data_mlr, 
                                 family = binomial(link = "logit"))

# Negative Affect: Negative apriori AU's main effects without random intercept.
# AU equation with mouth opening (AU25, AU26, and AU27) composite.
AU_main_neg_form <- stats::formula(manual_valence | resp_weights(class_weights) ~ AU17 + AU20 + 
                                     AU3_4 + AU6 + AU_mouth_opening)

# Check which model parameters can have priors. 
AU_main_neg_priors <- brms::get_prior(stats::formula(AU_main_neg_form),
                                      data = data_mlr_neg, 
                                      family = binomial(link = "logit"))

# Negative Affect: Negative apriori AU's equation without random intercept.
# AU equation with mouth opening (AU25, AU26, and AU27) composite.
AU_int_neg_form <- stats::formula(manual_valence | resp_weights(class_weights) ~ AU17 + AU20 + 
                                    AU3_4 + AU6 + AU_mouth_opening +
                                    AU17*AU3_4 + AU20*AU3_4 + 
                                    AU20*AU6 + AU20*AU_mouth_opening)

# Check which model parameters can have priors. 
AU_int_neg_priors <- brms::get_prior(stats::formula(AU_int_neg_form),
                                     data = data_mlr_neg, 
                                     family = binomial(link = "logit"))

# Negative Affect without AU17 and AU3+4 interactions. 
AU_int_neg_sig_form <- stats::formula(manual_valence | resp_weights(class_weights) ~ AU20 + 
                                    AU3_4 + AU6 + AU_mouth_opening +
                                    AU20*AU6 + AU20*AU_mouth_opening)

# Check which model parameters can have priors. 
AU_int_neg_sig_priors <- brms::get_prior(stats::formula(AU_int_neg_sig_form),
                                     data = data_mlr_neg, 
                                     family = binomial(link = "logit"))

## Model testing ----

# Random intercept model.
ri_fit <- brms::brm(formula = ri_form,
                          data = data_mlr,
                          # choose Bernoulli instead of categorical family for binary classification.
                          # specify a reference category if > 2 categories for classification.
                          family = brms::bernoulli(link = "logit"),
                          warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
                          iter = iterations, # n iterations per chain
                          chains = chains, # fits n Markov Chains
                          init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
                          cores = chains, # runs on n parallel cores
                          prior = ri_model_priors,
                          control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
                          refresh = 1,
                          seed = 16,
                          file = paste0("output/", analysis_type, "/mlr_analysis/ri_fit.rds"))

# Read model summary.
readr::read_rds(file = paste0("output/", analysis_type, "/mlr_analysis/ri_fit.rds"))

# Random intercept model fails to converge.
ri_fit_summary <- readr::read_rds(paste0('output/', analysis_type, '/mlr_analysis/ri_fit.rds'))

# Positive AU model with main effects without random intercept.
AU_main_pos_fit <- brms::brm(formula = AU_main_pos_form,
                            data = data_mlr,
                            # choose Bernoulli instead of categorical family for binary classification.
                            # specify a reference category if > 2 categories for classification. 
                            family = brms::bernoulli(link = "logit"),
                            warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
                            iter = iterations, # n iterations per chain
                            chains = chains, # fits n Markov Chains 
                            init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
                            cores = chains, # runs on n parallel cores
                            # a beta prior favors models that are neither too simple, nor too complex
                            # prior = prior(normal(0,5), class = "b"),
                            prior = AU_main_pos_priors,
                            control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
                            refresh = 1,
                            seed = 16,
                            file = paste0("output/", analysis_type, "/mlr_analysis/AU_main_pos_fit.rds"))

# Read model summary.
AU_main_pos_fit_summary <- readr::read_rds(paste0('output/', analysis_type, '/mlr_analysis/AU_main_pos_fit.rds'))

# Save fixed part of the model to .csv.
write.csv2(tibble::tibble(AU_main_pos_fit_summary[["fixed"]]),    
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_main_pos_fit_summary.csv"), 
           row.names = TRUE)

# Positive AU model with interaction effects without random intercept.
AU_int_pos_fit <- brms::brm(formula = AU_int_pos_form,
                        data = data_mlr,
                        # choose Bernoulli instead of categorical family for binary classification.
                        # specify a reference category if > 2 categories for classification. 
                        family = brms::bernoulli(link = "logit"),
                        warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
                        iter = iterations, # n iterations per chain
                        chains = chains, # fits n Markov Chains 
                        init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
                        cores = chains, # runs on n parallel cores
                        # a beta prior favors models that are neither too simple, nor too complex
                        # prior = prior(normal(0,5), class = "b"),
                        prior = AU_int_pos_priors,
                        control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
                        refresh = 1,
                        seed = 16,
                        file = paste0("output/", analysis_type, "/mlr_analysis/AU_int_pos_weighted_fit.rds"))

# Read model summary.
AU_int_pos_fit_summary <- readr::read_rds(paste0('output/', analysis_type, '/mlr_analysis/AU_int_pos_weighted_fit.rds'))

# Save fixed part of the model to .csv.
write.csv2(tibble::tibble(AU_int_pos_fit_summary[["fixed"]]),    
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_int_pos_fit_summary.csv"), 
           row.names = TRUE)

# Negative AU model with main effects without random intercept.
AU_main_neg_fit <- brms::brm(formula = AU_main_neg_form,
                             data = data_mlr_neg,
                             # choose Bernoulli instead of categorical family for binary classification.
                             # specify a reference category if > 2 categories for classification. 
                             family = brms::bernoulli(link = "logit"),
                             warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
                             iter = iterations, # n iterations per chain
                             chains = chains, # fits n Markov Chains 
                             init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
                             cores = chains, # runs on n parallel cores
                             # a beta prior favors models that are neither too simple, nor too complex
                             # prior = prior(normal(0,5), class = "b"),
                             prior = AU_main_neg_priors,
                             control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
                             refresh = 1,
                             seed = 16,
                             file = paste0("output/", analysis_type, "/mlr_analysis/AU_main_neg_fit.rds"))

# Read model summary.
AU_main_neg_fit_summary <- readr::read_rds(paste0('output/', analysis_type, '/mlr_analysis/AU_main_neg_fit.rds'))

# Save fixed part of the model to .csv.
write.csv2(tibble::tibble(AU_main_neg_fit_summary[["fixed"]]),    
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_main_neg_fit_summary.csv"), 
           row.names = TRUE)

# Negative AU model with interaction effects without random intercept.
AU_int_neg_fit <- brms::brm(formula = AU_int_neg_form,
                            data = data_mlr_neg,
                            # choose Bernoulli instead of categorical family for binary classification.
                            # specify a reference category if > 2 categories for classification. 
                            family = brms::bernoulli(link = "logit"),
                            warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
                            iter = iterations, # n iterations per chain
                            chains = chains, # fits n Markov Chains 
                            init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
                            cores = chains, # runs on n parallel cores
                            # a beta prior favors models that are neither too simple, nor too complex
                            # prior = prior(normal(0,5), class = "b"),
                            prior = AU_int_neg_priors,
                            control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
                            refresh = 1,
                            seed = 16,
                            file = paste0("output/", analysis_type, "/mlr_analysis/AU_int_neg_fit.rds"))

# Read model summary.
AU_int_neg_fit_summary <- readr::read_rds(paste0('output/', analysis_type, '/mlr_analysis/AU_int_neg_fit.rds'))

# Save fixed part of the model to .csv.
write.csv2(tibble::tibble(AU_int_neg_fit_summary[["fixed"]]),    
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_int_neg_fit_summary.csv"), 
           row.names = TRUE)

# Negative AU model with interaction effects without AU17.
AU_int_neg_sig_fit <- brms::brm(formula = AU_int_neg_sig_form,
                                data = data_mlr_neg,
                                # choose Bernoulli instead of categorical family for binary classification.
                                # specify a reference category if > 2 categories for classification. 
                                family = brms::bernoulli(link = "logit"),
                                warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
                                iter = iterations, # n iterations per chain
                                chains = chains, # fits n Markov Chains 
                                init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
                                cores = chains, # runs on n parallel cores
                                # a beta prior favors models that are neither too simple, nor too complex
                                # prior = prior(normal(0,5), class = "b"),
                                prior = AU_int_neg_sig_priors,
                                control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
                                refresh = 1,
                                seed = 16,
                                file = paste0("output/", analysis_type, "/mlr_analysis/AU_int_neg_sig_fit.rds"))

# Save fixed part of the model to .csv.
write.csv2(tibble::tibble(AU_int_neg_sig_fit_summary[["fixed"]]),    
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_int_neg_sig_fit_summary.csv"), 
           row.names = TRUE)

# Read model summary.
AU_int_neg_sig_fit_summary <- readr::read_rds(paste0('output/', analysis_type, '/mlr_analysis/AU_int_neg_sig_fit.rds'))

## Predicted probabilities ----

# Select model: "AU_int_neg_sig_fit" for negative AU model; AU_int_pos_fit for positive AU model.
AU_model <- AU_int_neg_sig_fit
AU_model <- AU_int_pos_fit

# Select dataset: 'data_mlr' for positive AU model or 'data_mlr_neg' for negative AU model. 
data_pred <- data_mlr %>%
  mutate(manual_valence = forcats::fct_drop(manual_valence_pos_rest))

# data_pred <- data_mlr_neg %>%
#   mutate(manual_valence = forcats::fct_drop(manual_valence))

# Derive predicted values for the whole dataset incrementally, starting at row 1.
sample_start <- 1
# Get predictions in increments of 10000 rows.
sample_step <- 10000
# Fix seed so that the results are reproducible - only required for the predicted values.
#set.seed(100)
# Create a matrix for storing the predictions.
predictions <- matrix(nrow = 0, ncol = 4)
# Loop across the dataset to get predicted values.
while (sample_start < nrow(data_pred)){
  print(sample_start)
  subset <- filter(data_pred, row_number() >= sample_start & row_number() < (sample_start + sample_step))
  predicted_val <- fitted(AU_model, 
                           summary = TRUE,
                           newdata = subset)
  predictions <- rbind(predictions, predicted_val)
  sample_start <- sample_start + sample_step
}
nrow(predictions) == nrow(data_pred)

# Store positive model prediction & write to csv.
data_pred_pos <- data_pred %>% 
  dplyr::mutate(
    pred = predictions[,1],
    pred_error = predictions[,2],
    pred_str = if_else(pred >= 0.5, 'Positive', 'Rest'), 
    manual_valence = manual_valence_pos_rest) %>%
  dplyr::select(-manual_valence_pos_rest)
  
write.csv2(data_pred_pos, 
           file = paste0("output/", analysis_type, "/mlr_analysis/data_pred_pos.csv"),
           row.names = TRUE)

# Store negative model predictions & write to .csv.
data_pred_neg <- data_pred %>% 
  dplyr::mutate(
    pred = predictions[,1],
    pred_error = predictions[,2],
    pred_str = if_else(pred >= 0.5, 'Neutral', 'Negative'))

write.csv2(data_pred_neg, 
           file = paste0("output/", analysis_type, "/mlr_analysis/data_pred_weighted_neg.csv"),
           row.names = TRUE)

# Specify model for calculating F1 scores.
data_pred <- data_pred_pos
data_pred <- data_pred_neg


# Create function: agreement_metrics, which computes agreement metrics (positive agreement = F1, negative agreement)
# for a given dataset and a ROC contrast index to compute
agreement_metrics <- function(roc_data, roc_index){
  # Get ROC metrics 
  roc_coords <- pROC::coords(roc_data$rocs[[roc_index]], "best", 
                             ret = c("threshold", "recall", "specificity", "precision", 
                                     "tp", "tn", "fp", "fn"))
  # Store and return negative agreement, positive agreement, and F1 to a dataframe
  df <- data.frame(pos_agreement = (2 * roc_coords$tp)/(2 * roc_coords$tp + roc_coords$fp + roc_coords$fn),
                   # the alternative f1 formula can be included to check whether F1 = PA, 
                   #f1_score = (2 * (roc_coords$precision * roc_coords$recall))/(roc_coords$precision + roc_coords$recall),
                   neg_agreement = (2 * roc_coords$tn)/(2 * roc_coords$tn + roc_coords$fp + roc_coords$fn)
  )
  return(list(roc_coords, df))
}

# Plot predictions.
hist(data_pred$pred)

data_pred$manual_valence[1]
data_pred$pred[1]
predictions[1,1]
data_pred$pred[length(data_pred$pred)]
predictions[length(data_pred$pred),1]

# Compute confusion matrix.
count_obs_pred <- dplyr::count(data_pred, manual_valence, pred_str)

# Specify each cell of the confusion matrix: Positive AU model
data_pred <- data_pred %>% dplyr::mutate(
  conf_mat = dplyr::case_when(
    manual_valence == "Positive" & pred_str == "Positive" ~ "tp", 
    manual_valence == "Rest" & pred_str == "Rest" ~ "tn",
    manual_valence == "Rest" & pred_str == "Positive" ~ "fp",
    manual_valence == "Positive" & pred_str == "Rest" ~ "fn"))

# Specify each cell of the confusion matrix: Negative AU model
data_pred <- data_pred %>% dplyr::mutate(
    conf_mat = dplyr::case_when(
      manual_valence == "Negative" & pred_str == "Negative" ~ "tp", 
      manual_valence == "Neutral" & pred_str == "Neutral" ~ "tn",
      manual_valence == "Neutral" & pred_str == "Negative" ~ "fp",
      manual_valence == "Negative" & pred_str == "Neutral" ~ "fn"))

# Get counts for each cell of the confusion matrix.
conf_mat <- dplyr::count(data_pred, conf_mat) 
fn <- conf_mat[conf_mat == 'fn', 'n']
fp <- conf_mat[conf_mat == 'fp', 'n']
tp <- conf_mat[conf_mat == 'tp', 'n']
tn <- conf_mat[conf_mat == 'tn', 'n']

# Compute F1 scores per observed facial expression class
# precision: 1) .79 for positive vs. rest; 2) .28 for negative vs. neutral 
tp / (tp + fp)

# recall = sensitivity: 1) .57 for rest vs. positive; 2) .61 for negative vs. neutral
tp / (tp + fn)

# specificity: 1) 0.93; 2) .88 for negative vs. neutral
tn / (fp + tn)

# F1 score formula = (2 * (precision * recall))/(precision + recall)

# PA (F1 score) for positive vs. negative/neutral = 0.66
(2 * ((tp / (tp + fp)) * (tp / (tp + fn)))) / (tp/(tp + fp) + (tp / (tp + fn)))

# PA formula from manuscript: Agrees
(2 * tp) / ((2* tp) + fp + fn)

# NA (Opposite F1) score for negative/neutral vs. positive = 0.88
(2 * ((tn / (tn + fn)) * (tn / (tn + fp))))/((tn / (tn + fn)) + (tn / (tn + fp)))

# NA formula from manuscript: Agrees 
(2 * tn) / ((2 * tn) + fp + fn)

# PA (F1 score) for negative vs neutral = 0.38
(2 * ((tp / (tp + fp)) * (tp / (tp + fn)))) / (tp/(tp + fp) + (tp / (tp + fn)))

# PA formula from manuscript: Agrees
(2 * tp) / ((2* tp) + fp + fn)

# NA (Opposite F1 score) for neutral vs negative = 0.91
(2 * ((tn / (tn + fn)) * (tn / (tn + fp))))/((tn / (tn + fn)) + (tn / (tn + fp)))

# NA formula from manuscript: Agrees 
(2 * tn) / ((2 * tn) + fp + fn)

## Plot model results ----

# Set which model will be plotted: 'ri_fit_summary', 'AU_main_fit_summary', 'AU_int_fit_summary'
model_fit <- AU_int_neg_fit

# Check whether there is evidence of the model not converging.
# The two chains mix well for all of the parameters >> no evidence of non-convergence.
brms::mcmc_plot(model_fit, type = "trace") + 
  ggplot2::theme(
    axis.title.x = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 14, hjust = 0.5)) 

# Check whether there are indications of strong autocorrelation that would bias the estimates. 
# All parameters quickly diminish to about 0 >> no evidence of strong autocorrelation. 
brms::mcmc_plot(model_fit, 
         type = "acf_bar")

# Plot the densities of the parameter estimates. Wider credible intervals indicate 
# greater uncertainty in the estimate of the parameter coefficient's value. 
brms::mcmc_plot(model_fit, type = "intervals")

# Visualize the point estimates and the respective uncertainty intervals.
# The dark blue line in each density represents the point estimate, whereas
# the light-blue area indicates the 95% credibility intervals. The credibility
# intervals of meaningful parameters do not contain zero and their densities 
# have a very narrow shape.
brms::mcmc_plot(model_fit, 
         type = "areas",
         prob = 0.95)

# Plot the densities of the exponentiated parameter estimates.
brms::mcmc_plot(model_fit, 
         type = "intervals",
         prob = 0.95,
         transformations = "exp") +
  geom_vline(xintercept = 1, color = "grey") + 
  xlim(0, 110)

# Plot main and interaction effects using conditional means.
conditional_effects <- brms::conditional_effects(model_fit, plot = FALSE, resolution = 600)
brms_theme <- ggplot2::theme(axis.title.x = element_text(size = 14, hjust = 0.5),
                             axis.text.x = element_text(size = 12),
                             axis.title.y = element_text(size = 14, hjust = 0.5), 
                             axis.text.y = element_text(size = 12), 
                             plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# Save plots (set plot in conditional_effects_plots = FALSE to save; set = TRUE to plot)
conditional_effects_plots <- plot(conditional_effects, theme = brms_theme, plot = FALSE)

# Change axis / legend titles and save plots.
for (p in conditional_effects_plots) {
  plot_id <- p$labels$x
  # Change axis titles
  lab_y <- p$labels$y
  lab_x <- p$labels$x
  new_lab_y <- 'Manual Valence'
  #new_leg <- 'Mouth Opening'
  #new_lab_x <- 'Mouth Opening'
  #new_lab_x <- stringr::str_to_title(stringr::str_replace_all(lab_x, '_', ' '))
  p <- p + 
    ggplot2::ylab(new_lab_y) 
    #xlab(new_lab_x) +
   #ggplot2::guides(color = ggplot2::guide_legend(title = new_leg), fill = ggplot2::guide_legend(title = new_leg))
  # Save plots
  ggplot2::ggsave(p,
                   file = paste0("rplots/", analysis_type, "/", plot_id, ".jpg"))
 }

# Exponentiate the estimates to interpret their values.
AU_fit_par <- round(exp(fixef(model_fit)[,-2]), 2)

# Save table with the exponentiated parameters to .csv.
write.csv2(model_fit, 
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_int_fit_exp_par.csv"),
           row.names = TRUE)

# Compute mean and range for AU12 (centered to the grand mean).
data_mlr %>% 
  dplyr::summarise(AU12_range = round(range(AU12, na.rm = TRUE), 2), 
            AU12_mean = round(mean(AU12, na.rm = TRUE), 2))

# Plot the effect of AU12 (centered).
model_fit %>%
  tidybayes::spread_draws(b_Intercept, b_AU12) %>%
  dplyr::mutate(AU12 = list(seq(from = -0.24, to = 0.71, by = 0.01))) %>% #the observed value range of AU12
  tidyr::unnest(AU12) %>%
  dplyr::mutate(pred = exp(b_Intercept + b_AU12*AU12)/(1+exp(b_Intercept + b_AU12*AU12))) %>%
  dplyr::group_by(AU12) %>%
  dplyr::summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = AU12, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.2) +
  ylab("Predicted Probability: Positive (1) vs. Rest (0)") + 
  xlim(-0.5, 1) + 
  ylim(0, 1)

# Compute mean and range for AU6 (centered to the grand mean).
data_mlr %>% 
  dplyr::summarise(AU6_range = round(range(AU6, na.rm = TRUE), 2), 
            AU6_mean = round(mean(AU6, na.rm = TRUE), 2))

# Plot the effect of AU6 (centered).
model_fit %>%
  tidybayes::spread_draws(b_Intercept, b_AU6) %>%
  dplyr::mutate(AU6 = list(seq(from = -0.20, to = 0.75, by = 0.01))) %>% #the observed value range 
  tidyr::unnest(AU6) %>%
  dplyr::mutate(pred = exp(b_Intercept + b_AU6*AU6)/(1+exp(b_Intercept + b_AU6*AU6))) %>%
  dplyr::group_by(AU6) %>%
  dplyr::summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = AU6, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.2) +
  ylab("Predicted Probability: Positive (1) vs. Rest (0) ") + 
  xlim(-0.5, 1) + 
  ylim(0, 1)

# Compute mean and range for AU25 (centered to the grand mean).
data_mlr %>% 
  dplyr::summarise(AU25_range = round(range(AU25, na.rm = TRUE), 2), 
            AU25_mean = round(mean(AU25, na.rm = TRUE), 2))

# Plot the effect of AU25 (centered).
model_fit %>%
  tidybayes::spread_draws(b_Intercept, b_AU25) %>%
  dplyr::mutate(AU25 = list(seq(from = -0.32,to = 0.63, by = 0.01))) %>% #the observed value range 
  tidyr::unnest(AU25) %>%
  dplyr::mutate(pred = exp(b_Intercept + b_AU25*AU25)/(1+exp(b_Intercept + b_AU25*AU25))) %>%
  dplyr::group_by(AU25) %>%
  dplyr::summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = AU25, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.2) +
  ylab("Predicted Probability: Positive (1) vs. Rest (0) ") + 
  xlim(-0.5, 1) + 
  ylim(0, 1)

# Compute mean and range for AU27 (centered to the grand mean).
data_mlr %>% 
  dplyr::summarise(AU27_range = round(range(AU27, na.rm = TRUE), 2), 
            AU27_mean = round(mean(AU27, na.rm = TRUE), 2))

# Plot the effect of AU27 (centered).
model_fit %>%
  tidybayes::spread_draws(b_Intercept, b_AU27) %>%
  dplyr::mutate(AU27 = list(seq(from = -0.03,to = 0.91, by = 0.01))) %>% #the observed value range 
  tidyr::unnest(AU27) %>%
  dplyr::mutate(pred = exp(b_Intercept + b_AU27*AU27)/(1+exp(b_Intercept + b_AU27*AU27))) %>%
  dplyr::group_by(AU27) %>%
  dplyr::summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = AU27, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.2) +
  ylab("Predicted Probability: Positive (1) vs. Rest (0) ") + 
  xlim(-0.5, 1) +
  ylim(0, 1)

# Compute mean and range for AU20 (centered to the grand mean).
data_mlr %>% 
  dplyr::summarise(AU20_range = round(range(AU20, na.rm = TRUE), 2), 
            AU20_mean = round(mean(AU20, na.rm = TRUE), 2))

# Plot the effect of AU20 (centered).
model_fit %>%
  tidybayes::spread_draws(b_Intercept, b_AU20) %>%
  dplyr::mutate(AU20 = list(seq(from = -0.05,to = 0.90, by = 0.01))) %>% #the observed value range 
  tidyr::unnest(AU20) %>%
  dplyr::mutate(pred = exp(b_Intercept + b_AU20*AU20)/(1+exp(b_Intercept + b_AU20*AU20))) %>%
  dplyr::group_by(AU20) %>%
  dplyr::summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = AU20, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.2) +
  ylab("Predicted Probability: Positive (1) vs. Rest (0) ") + 
  xlim(-0.5, 1) + 
  ylim(0, 1)

# Compute mean and range for AU17 (centered to the grand mean).
data_mlr %>% 
  dplyr::summarise(AU17_range = round(range(AU17, na.rm = TRUE), 2), 
            AU17_mean = round(mean(AU17, na.rm = TRUE), 2))

# Plot the effect of AU17 (centered)
# AU17 shows the opposite effect than expected.
model_fit %>%
  tidybayes::spread_draws(b_Intercept, b_AU17) %>%
  dplyr::mutate(AU17 = list(seq(from = -0.05,to = 0.90, by = 0.01))) %>% #the observed value range 
  tidyr::unnest(AU17) %>%
  dplyr::mutate(pred = exp(b_Intercept + b_AU17*AU17)/(1+exp(b_Intercept + b_AU17*AU17))) %>%
  dplyr::group_by(AU17) %>%
  dplyr::summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = AU17, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.2) +
  ylab("Predicted Probability: Positive (1) vs. Rest (0) ") + 
  xlim(-0.5, 1) + 
  ylim(0, 1)

# Compute mean and range for AU3_4 (centered to the grand mean).
data_mlr %>% 
  dplyr::summarise(AU3_4_range = round(range(AU3_4, na.rm = TRUE), 2), 
            AU3_4_mean = round(mean(AU3_4, na.rm = TRUE), 2))

# Plot the effect of AU3_4 (centered).
model_fit %>%
  tidybayes::spread_draws(b_Intercept, b_AU3_4) %>%
  dplyr::mutate(AU3_4 = list(seq(from = -0.02,to = 0.93, by = 0.01))) %>% #the observed value range 
  tidyr::unnest(AU3_4) %>%
  dplyr::mutate(pred = exp(b_Intercept + b_AU3_4*AU3_4)/(1+exp(b_Intercept + b_AU3_4*AU3_4))) %>%
  dplyr::group_by(AU3_4) %>%
  dplyr::summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = AU3_4, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha = 0.2) +
  ylab("Predicted Probability: Positive (1) vs. Rest (0) ") + 
  xlim(-0.5, 1) + 
  ylim(0, 1)

# Bayesian Model Averaging ----

# AU distributions
hist(data_mlr$AU6)
hist(data_mlr$AU12)
hist(data_mlr$AU25)
hist(data_mlr$AU26)
hist(data_mlr$AU27)
hist(data_mlr$AU_mouth_opening)
hist(data_mlr$AU3_4)
hist(data_mlr$AU17)
hist(data_mlr$AU20)

## Main effects: AU model with mouth opening composite ----
par_select_AU_main <- BAS::bas.glm(formula = AU_main_form,
                                   data = data_mlr,
                                   # Specify a reference category if n > 2 categories for classification. 
                                   # <5 % of the data were manually coded as Negative; therefore, Negative and Neutral are pooled.
                                   family = binomial(link = "logit"),
                                   # force.heredity = FALSE when main effects are modeled. 
                                   force.heredity = FALSE,
                                   initprobs = "Uniform",
                                   #betaprior = "JZS", 
                                   #betaprior = hyper.g.n(),
                                   method = "MCMC+BAS",
                                   n.models =  20,
                                   bestmodel = NULL,
                                   # Choose Bernoulli instead of categorical family for binary classification.
                                   modelprior = Bernoulli(probs = 0.5))

## Main effects: Positive AU model ----
par_select_pos_AU_main <- BAS::bas.glm(formula = AU_main_pos_form,
                                   data = data_mlr,
                                   # Specify a reference category if n > 2 categories for classification. 
                                   # <5 % of the data were manually coded as Negative; therefore, Negative and Neutral are pooled.
                                   family = binomial(link = "logit"),
                                   # force.heredity = FALSE when main effects are modeled. 
                                   force.heredity = FALSE,
                                   initprobs = "Uniform",
                                   #betaprior = "JZS", 
                                   #betaprior = hyper.g.n(),
                                   method = "MCMC+BAS",
                                   n.models =  20,
                                   bestmodel = NULL,
                                   # Choose Bernoulli instead of categorical family for binary classification.
                                   modelprior = Bernoulli(probs = 0.5))

## Main effects: Negative AU model ----
par_select_neg_AU_main <- BAS::bas.glm(formula = AU_main_neg_form,
                                       data = data_mlr_neg,
                                       # Specify a reference category if n > 2 categories for classification. 
                                       # <5 % of the data were manually coded as Negative; therefore, Negative and Neutral are pooled.
                                       family = binomial(link = "logit"),
                                       # force.heredity = FALSE when main effects are modeled. 
                                       force.heredity = FALSE,
                                       initprobs = "Uniform",
                                       #betaprior = "JZS", 
                                       #betaprior = hyper.g.n(),
                                       method = "MCMC+BAS",
                                       n.models =  20,
                                       bestmodel = NULL,
                                       # Choose Bernoulli instead of categorical family for binary classification.
                                       modelprior = Bernoulli(probs = 0.5))

## Interaction effects: AU model with mouth opening composite ----
par_select_AU_int <- BAS::bas.glm(formula = AU_int_form,
                                   data = data_mlr,
                                   # Specify a reference category if n > 2 categories for classification. 
                                   # <5 % of the data were manually coded as Negative; therefore, Negative and Neutral are pooled.
                                   family = binomial(link = "logit"),
                                   # force.heredity = TRUE when interaction effects are modeled. 
                                   force.heredity = TRUE,
                                   initprobs = "Uniform",
                                   #betaprior = "JZS", 
                                   #betaprior = hyper.g.n(),
                                   method = "MCMC+BAS",
                                   n.models =  20,
                                   bestmodel = NULL,
                                   # Choose Bernoulli instead of categorical family for binary classification.
                                   modelprior = Bernoulli(probs = 0.5))

## Interaction effects: Positive AU model  ----
par_select_pos_AU_int <- BAS::bas.glm(formula = AU_int_pos_form,
                                  data = data_mlr,
                                  # Specify a reference category if n > 2 categories for classification. 
                                  # <5 % of the data were manually coded as Negative; therefore, Negative and Neutral are pooled.
                                  family = binomial(link = "logit"),
                                  # force.heredity = TRUE when interaction effects are modeled. 
                                  force.heredity = TRUE,
                                  initprobs = "Uniform",
                                  #betaprior = "JZS", 
                                  #betaprior = hyper.g.n(),
                                  method = "MCMC+BAS",
                                  n.models =  20,
                                  bestmodel = NULL,
                                  # Choose Bernoulli instead of categorical family for binary classification.
                                  modelprior = Bernoulli(probs = 0.5))

## Interaction effects: Negative AU model  ----
par_select_neg_AU_int <- BAS::bas.glm(formula = AU_int_neg_form,
                                      data = data_mlr_neg,
                                      # Specify a reference category if n > 2 categories for classification. 
                                      # <5 % of the data were manually coded as Negative; therefore, Negative and Neutral are pooled.
                                      family = binomial(link = "logit"),
                                      # force.heredity = TRUE when interaction effects are modeled. 
                                      force.heredity = TRUE,
                                      initprobs = "Uniform",
                                      #betaprior = "JZS", 
                                      #betaprior = hyper.g.n(),
                                      method = "MCMC+BAS",
                                      n.models =  20,
                                      bestmodel = NULL,
                                      # Choose Bernoulli instead of categorical family for binary classification.
                                      modelprior = Bernoulli(probs = 0.5))


## Robustness check ----

# Update prior for main/interaction effects models
#BAS::update(par_select_AU_main, newprior = , aplpha = )

## Plot model results ----

# Specify the model results to plot: par_select_AU_main or par_select_AU_int.

par_select_AU_model <- par_select_neg_AU_int

# Determine which are the best models.
# bas.image throws an error whenever less best models are requested for plotting
# than the n successfully fit (with posterior distribution > 0)
# Report issue for bas.image on the bas developer's github 
# bas.image code: https://github.com/merliseclyde/BAS/blob/f92fc3e0e1860b171906392bef9fde6cad48a642/R/image.R

# Best models
best <- order(-par_select_AU_model$postprobs)[1:20]
# "Which" contains variables per model. Get variables of best model.
selected_vars <- par_select_AU_model$which[[best[1]]]
# Select variables.
par_select_AU_model$namesx[selected_vars]

# Model inclusion probs.
par_select_AU_model$probne0

# Model summary.
plot(par_select_AU_model)

# You cannot compare models with postprobs = 0.
number_of_valid_models <- sum(par_select_AU_model$postprobs > 0)
image(par_select_AU_model, top.models = min(20, number_of_valid_models))

# Log and prob (both default true) need to be false to be able to plot even if postprobs = 0.
# Do not compare models with postprobs = 0.
image(par_select_AU_model, top.models = 5, log = FALSE, prob = FALSE, rotate = FALSE) 

# Model summary.
par_selection_sum <- summary(par_select_AU_model)

# Parameter coefficients summary.
par_selection_coef <- coef(par_select_AU_model)

# Only for method MCMC:
#confint(par_selection_coef)

# Create a table with parameter coefficients.
par_selection_coef <- tibble::tibble(par_selection_coef$namesx, 
                                     par_selection_coef$postmean, 
                                     par_selection_coef$postsd, 
                                     par_selection_coef$probne0)

# Save model comparison and parameter coefficients summaries. 
write.csv2(par_selection_sum, 
           file = paste0("output/", analysis_type, "/mlr_analysis/model_comparison_BFR9.csv"))
write.csv2(par_selection_coef, 
           file = paste0("output/", analysis_type, "/mlr_analysis/par_selection_BFR9.csv"))

# Posterior probabilities. 
postprob <- par_select_AU_model$postprobs
par_select_AU_model$postprobs