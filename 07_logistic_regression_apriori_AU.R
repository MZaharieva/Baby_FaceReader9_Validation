#---
#**Aim:** 
#*"Perform Bayesian logistic regression analysis exploring the relations between 
#*the activation intensity of automatically detected action units (AUs) and 
#*manually coded facial expression categories. Single-model inference is followed 
#*by model averaging for parameter selection."
#---

# Libraries ----
#install.packages("igraph", type = "binary") # for package 'brms', also install binary in case installation fails otherwise.
library(BAS) # Bayesian parameter selection through model averaging
library(brms)  # fitting Bayesian multilevel logistic model
library(tidybayes) # for analysis of posterior draws of a Bayesian model
library(ROCR) # fitting ROC curves 
library(corrplot) # correlation plots
library(MVN) 
library(dplyr) # data wrangling & manipulation
library(readxl) # reading excel data
library(magrittr) # For operators (data manipulation/programming flow)
library(ggplot2) # plotting

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

## Read in, recode, and center variables for FE model
# Make one primary dataframe, from which data is subset for specific analyses.
# Dyadic dataset of infants at 4 and 8 months analyzed with the smoothing function in BFR 9. 
data_mlr <- readr::read_rds(bfr_data_path) %>%
  # All errors from the automatic analysis should have been already filtered out in the previous script: ROC analysis. 
  # Filter the relevant data, excluding events that are manually coded as "Not codable".
  dplyr::filter(FE_category != "Not Visible") %>%
  dplyr::mutate(manual_valence = factor(FE_category, 
                                        levels = c('Negative', 'Neutral', 'Positive'), 
                                        ordered = TRUE), 
         manual_valence_pos_rest = recode(FE_category, 
                                          Negative = 'Rest', Neutral = 'Rest'),
         manual_valence_pos_rest = factor(manual_valence_pos_rest, 
                                          levels = c('Rest', 'Positive'), 
                                          ordered = TRUE),
         participant_id = factor(participant_id)) %>%
  dplyr::mutate(manual_valence_binary = dplyr::if_else(manual_valence_pos_rest == "Positive", 1, 0)) %>%
  dplyr::select(manual_valence, FE_category, 
                manual_valence_pos_rest, age,
                AU12, AU6, AU25, AU26, AU27,
                AU17, AU20, AU3_4, AU7,
                participant_id, participant_code) %>%
  # Centering the AU variables to the grand mean (and not the infant's mean)
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

# Check whether there is any missing data.
data_mlr %>% dplyr::summarise(dplyr::across(dplyr::everything(), 
                                            ~ sum(is.na(.x))))

# Write to rds. 
#saveRDS(data_mlr, file = paste0("output/", analysis_type, "/mlr_analysis/brf9_mlr_data.rds"))

# Write to csv. 
#readr::write_csv2(data_mlr, 
#                  file = paste0("output/", analysis_type, "/mlr_analysis/brf9_mlr_data.csv"), 
#                  na = "NA")

## Correlation matrix ----
# Make sure that the correlations among model predictors are not too high 
# AU12 and AU6 correlate at about .5; AU6 & AU12*AU6 correlate at about .6
AU_corr <- data_mlr %>% 
  dplyr::mutate(AU12_AU6_int = (AU12*AU6), 
         # Compute composites & interaction effects for positive facial expressions
         AU12_AU25_int = (AU12*AU25),
         AU12_AU26_int = (AU12*AU26),
         AU12_AU27_int = (AU12*AU27),
         # Compute composites & interaction effects for negative facial expressions
         AU17_AU3_4_int = AU17*AU3_4,
         AU20_AU3_4_int = AU20*AU3_4,
         AU20_AU6_int = (AU20*AU6),
         AU20_AU25_int = (AU20*AU25),
         AU20_AU26_int = (AU20*AU26),
         AU20_AU27_int = (AU20*AU27)) %>%
  dplyr::select(dplyr::starts_with('AU')) 

# Correlations positive AU's
AU_corr_pos <- AU_corr %>% 
  dplyr::select(AU12, AU6, AU25, AU26, AU27, 
         AU12_AU6_int, AU12_AU25_int, AU12_AU26_int, AU12_AU27_int) %>%
  stats::cor(.)

corrplot::corrplot(AU_corr_pos, method = "number", type = "upper")

# Save correlation matrix to .csv.
readr::write_csv2(tibble::as_tibble(AU_corr_pos["corrPos"["corr"]], 
                                    tibble::as_tibble_col(AU_corr_pos["corrPos"["yName"]])), 
                  file = paste0("output/", analysis_type, "/mlr_analysis/AU_pos_correlations.csv"), 
                  na = "NA")

# Correlations negative AU's
AU_corr_neg <- AU_corr %>% 
  dplyr::select(AU20, AU17, AU3_4, AU20_AU3_4_int, 
                AU17_AU3_4_int, AU6, AU7, AU6_AU7, 
                AU20_AU6_int, AU25, AU26, AU27, 
                AU20_AU25_int, AU20_AU26_int, 
                AU20_AU27_int) %>%
  stats::cor(.)

corrplot::corrplot(AU_corr_neg, method = "number", type = "upper")

# Save correlation matrix to .csv.
readr::write_csv2(tibble::as_tibble(AU_corr_pos["corrNeg"["corr"]], 
                                    tibble::as_tibble_col(AU_corr_pos["corrNeg"["yName"]])), 
                  file = paste0("output/", analysis_type, "/mlr_analysis/AU_neg_correlations.csv"), 
                  na = "NA")

# Bayesian Logistic Regression: Single-Model Inference ----

## Model settings ----
nlogical_cores <- parallel::detectCores(logical=FALSE) - 2
chains <- nlogical_cores * 2
warmups_per_chain <- 500
iterations_post_warmup <- 6000 
iterations <- ceiling((iterations_post_warmup/chains) + warmups_per_chain)
max_tree_depth <- 20

## Model formula's ----

# Random-intercept model
ri_form <- stats::formula(manual_valence_pos_rest ~ (1|participant_id))

# Saturated model without random intercept
full_form <- stats::formula(manual_valence_pos_rest ~ age_8 + 
                              AU12 + AU6 + AU25 + AU26 + AU27 + 
                              AU12*AU6 + AU12*AU25 + AU12*AU26 + AU12*AU27 +
                              AU17 + AU20 + AU3_4 + 
                              AU17*AU3_4 + AU20*AU3_4 + AU20*AU6 + 
                              AU20*AU25 + AU20*AU26 + AU20*AU27)

# Full AU equation without random intercept
AU_form <- stats::formula(manual_valence_pos_rest ~ AU12 + 
                            AU6 + AU25 + AU26 + AU27 + AU12*AU6 + 
                            AU12*AU25 + AU12*AU26 + AU12*AU27 +
                            AU17 + AU20 + AU3_4 + AU17*AU3_4 + 
                            AU20*AU3_4 + AU20*AU6 + AU20*AU25 + 
                            AU20*AU26 + AU20*AU27)

# Positive AUs without random intercept
AU_pos_form <- stats::formula(manual_valence_pos_rest ~ age_8 + AU12 + 
                                AU6 + AU25 + AU26 + AU27 + AU12*AU6 + 
                                AU12*AU25 + AU12*AU26 + AU12*AU27)

# Negative AUs without random intercept
AU_neg_form <- stats::formula(manual_valence_pos_rest ~ age_8 + 
                                AU6 + AU25 + AU26 + AU27 +
                                AU17 + AU20 + AU3_4 + AU17*AU3_4 + 
                                AU20*AU3_4 + AU20*AU6 + AU20*AU25 + 
                                AU20*AU26 + AU20*AU27)
## Model testing ----

# Random intercept model
# ri_fit <- brms::brm(formula = ri_form,
#                           data = data_mlr,
#                           # choose Bernoulli instead of categorical family for binary classification.
#                           # specify a reference category if > 2 categories for classification. 
#                           family = brms::bernoulli(link = "logit"),
#                           warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
#                           iter = iterations, # n iterations per chain
#                           chains = chains, # fits n Markov Chains 
#                           init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
#                           cores = chains, # runs on n parallel cores
#                           #prior = set_prior(normal(0,5), class = "b"),
#                           control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
#                           refresh = 1,
#                           seed = 16,
#                           file = paste0("output/", analysis_type, "/mlr_analysis/ri_fit.rds"))

# Read model summary
readr::read_rds(file = paste0("output/", analysis_type, "/mlr_analysis/ri_fit.rds"))

# Random intercept model fails to converge
#summary(ri_fit)

# Apriori AU model without random intercept
# AU_fit <- brms::brm(formula = AU_form,
#                           data = data_mlr,
#                           # choose Bernoulli instead of categorical family for binary classification.
#                           # specify a reference category if > 2 categories for classification. 
#                           family = brms::bernoulli(link = "logit"),
#                           warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
#                           iter = iterations, # n iterations per chain
#                           chains = chains, # fits n Markov Chains 
#                           init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
#                           cores = chains, # runs on n parallel cores
#                           prior = prior(normal(0,5), class = "b"),
#                           control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
#                           refresh = 1,
#                           seed = 16,
#                           file = paste0("output/", analysis_type, "/mlr_analysis/AU_fit.rds"))

# Read model summary
AU_fit_summary <- readr::read_rds(paste0('output/', analysis_type, '/mlr_analysis/AU_fit.rds'))

# Save fixed part of the model to .csv
write.csv2(tibble::tibble(AU_fit_summary[["fixed"]]), 
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_fit.csv"), 
                         row.names = TRUE)

# Positive AU's
AU_pos_fit <- brms::brm(formula = AU_pos_form,
                           data = data_mlr,
                           # choose Bernoulli instead of categorical family for binary classification.
                           # specify a reference category if > 2 categories for classification. 
                           family = brms::bernoulli(link = "logit"),
                           warmup = warmups_per_chain, # specifies the burn-in period (i.e. number of iterations that should be discarded)
                           iter = iterations, # n iterations per chain
                           chains = chains, # fits n Markov Chains 
                           init = "0", # starts at 0; can set this as the maximum likelihood parameter estimates.
                           cores = chains, # runs on n parallel cores
                           prior = prior(normal(0,5), class = "b"),
                           control = list(max_treedepth = max_tree_depth, adapt_delta = 0.9),
                           refresh = 1,
                           seed = 16,
                           file = paste0("output/", analysis_type, "/mlr_analysis/AU_pos_fit.rds"))

# Read model summary
pos_fit_summary <- readr::read_rds(paste0('output/', analysis_type, '/mlr_analysis/AU_pos_fit.rds'))

# Save fixed part of the model to .csv
write.csv2(tibble::tibble(pos_fit_summary[["fixed"]]),    
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_pos_fit.csv"), 
           row.names = TRUE)

## Plot model fit ----

# Check whether there is evidence of the model not converging.
# The two chains mix well for all of the parameters and therefore, we can conclude no evidence of non-convergence.
brms::mcmc_plot(AU_fit, type = "trace")

# Check whether there are indications of strong autocorrelation that would bias the estimates. 
# All parameters quickly diminish to about 0 >> no evidence of strong autocorrelation. 
brms::mcmc_plot(AU_fit, 
         type = "acf_bar")

# # Plot the densities of these parameter estimates.
# Note that the interpretation of the parameter estimates is linked to the odds rather than probabilities. The definition of odds is: P(event occurring)/P(event not occurring). 
# Make sure to understand how to interpret the parameters: 
# In this analysis, assuming everything else stays the same, being a boy increases the odds of repeating a grade by 54%, in comparison to being a girl; having preschool education lowers the odds of repeating a grade by (1 - 0.54)% = 46%, in comparison to not having preschool education, assuming everything else stays constant. The baseline odds (indicated by the intercept term) of repeating a grade, namely if you're a girl with no previous schooling, is about 17%.
# Does not work properly. 
brms::mcmc_plot(AU_fit, type = "intervals")

# Visualize the point estimates and the respective uncertainty intervals.
# The dark blue line in each density represents the point estimate, whereas
# the light-blue area indicates the 95% credibility intervals. The credibility
# intervals of meaningful do not contain zero and their densities have a very narrow shape.
brms::mcmc_plot(AU_fit, 
         type = "areas",
         prob = 0.95)

# Exponentiate the estimates to interpret their values.
AU_fit_par <- exp(fixef(AU_fit)[,-2])

# Plot the densities of these parameter estimates.
# Note that the interpretation of the parameter estimates is linked to the odds rather than probabilities. The definition of odds is: P(event occurring)/P(event not occurring). 
# Make sure to understand how to interpret the parameters: 
# In this analysis, assuming everything else stays the same, being a boy increases the odds of repeating a grade by 54%, in comparison to being a girl; having preschool education lowers the odds of repeating a grade by (1 - 0.54)% = 46%, in comparison to not having preschool education, assuming everything else stays constant. The baseline odds (indicated by the intercept term) of repeating a grade, namely if you're a girl with no previous schooling, is about 17%.
# Does not work properly. 
brms::mcmc_plot(AU_fit, 
         type = "intervals",
         prob = 0.95,
         transformations = "exp") +
  geom_vline(xintercept = 1, color = "grey") + 
  xlim(0, 100)

# Compute mean and range for AU12 (centered to the grand mean)
data_mlr %>% 
  dplyr::summarise(AU12_range = round(range(AU12, na.rm = TRUE), 2), 
            AU12_mean = round(mean(AU12, na.rm = TRUE), 2))

# Plot the effect of AU12 (centered)
AU_fit %>%
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

# Compute mean and range for AU6 (centered to the grand mean)
data_mlr %>% 
  dplyr::summarise(AU6_range = round(range(AU6, na.rm = TRUE), 2), 
            AU6_mean = round(mean(AU6, na.rm = TRUE), 2))

# Plot the effect of AU6 (centered)
AU_fit %>%
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

# Compute mean and range for AU25 (centered to the grand mean)
data_mlr %>% 
  dplyr::summarise(AU25_range = round(range(AU25, na.rm = TRUE), 2), 
            AU25_mean = round(mean(AU25, na.rm = TRUE), 2))

# Plot the effect of AU25 (centered)
AU_fit %>%
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

# Compute mean and range for AU27 (centered to the grand mean)
data_mlr %>% 
  dplyr::summarise(AU27_range = round(range(AU27, na.rm = TRUE), 2), 
            AU27_mean = round(mean(AU27, na.rm = TRUE), 2))

# Plot the effect of AU27 (centered)
# AU27 and AU20 may be swapped - show the opposite effects as expected 
AU_fit %>%
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

# Compute mean and range for AU20 (centered to the grand mean)
data_mlr %>% 
  dplyr::summarise(AU20_range = round(range(AU20, na.rm = TRUE), 2), 
            AU20_mean = round(mean(AU20, na.rm = TRUE), 2))

# Plot the effect of AU20 (centered)
AU_fit %>%
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

# Compute mean and range for AU17 (centered to the grand mean)
data_mlr %>% 
  dplyr::summarise(AU17_range = round(range(AU17, na.rm = TRUE), 2), 
            AU17_mean = round(mean(AU17, na.rm = TRUE), 2))

# Plot the effect of AU17 (centered)
# AU17 shows the opposite effect than expected
AU_fit %>%
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

# Compute mean and range for AU3_4 (centered to the grand mean)
data_mlr %>% 
  dplyr::summarise(AU3_4_range = round(range(AU3_4, na.rm = TRUE), 2), 
            AU3_4_mean = round(mean(AU3_4, na.rm = TRUE), 2))

# Plot the effect of AU3_4 (centered)
AU_fit %>%
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

# Plot interaction effects
brms::conditional_effects(AU_fit)

# Exponentiate the estimates to interpret their values.
pos_fit_par <- exp(fixef(AU_fit)[,-2])

# Save table with the exponentiated parameters to .csv.
write.csv2(AU_fit_par, 
           file = paste0("output/", analysis_type, "/mlr_analysis/AU_fit_exp_par.csv"),
                         row.names = TRUE)

# Bayesian Model Averaging ----

## Run model with positive AU's (apriori) ----
par_selection_pos <- BAS::bas.glm(formula = manual_valence_pos_rest ~ AU12 + AU6
                                  + AU25 + AU26 + AU27 + AU12*AU6 + AU12*AU25
                                  + AU12*AU26 + AU12*AU27,
                                  data = data_mlr,
                                  # Specify a reference category if n > 2 categories for classification. 
                                  # <5 % of the data were manually coded as Negative; therefore, Negative and Neutral are pooled.
                                  family = binomial(link = "logit"),
                                  # force.heredity = true when interaction effects are modeled. 
                                  force.heredity = TRUE,
                                  initprobs = "Uniform",
                                  #betaprior = "JZS", 
                                  #betaprior = hyper.g.n(),
                                  method = "BAS",
                                  n.models =  20,
                                  bestmodel = NULL,
                                  # Choose Bernoulli instead of categorical family for binary classification.
                                  modelprior = Bernoulli(probs = 0.5))

# Summary statistics 
hist(data_mlr$AU6)
hist(data_mlr$AU12)
hist(data_mlr$AU25)
hist(data_mlr$AU26)
hist(data_mlr$AU27)

# bas.image throws an error whenever less best models are requested for plotting than the n succesfully fit (with posterior distribution > 0)
# Report issue for bas.image on the bas developer's github 
# bas.image code: https://github.com/merliseclyde/BAS/blob/f92fc3e0e1860b171906392bef9fde6cad48a642/R/image.R
# You can see the R2 per model
par_selection_pos$R2
# Best models
best <- order(-par_selection_pos$postprobs)[1:20]
# "Which" contains variables per model. Get variables of best model 
selected_vars <- par_selection_pos$which[[best[1]]]
# Select vars.
par_selection_pos$namesx[selected_vars]

# Model inclusion probs
par_selection_pos$probne0

# Model summary
plot(par_selection_pos)

# You cannot compare models with postprobs = 0.
number_of_valid_models <- sum(par_selection_pos$postprobs > 0)
BAS::image(par_selection_pos, top.models = min(20, number_of_valid_models))

# Log and prob (both default true) need to be false to be able to plot even if postprobs = 0
BAS::image(par_selection_pos, top.models = 10, log = FALSE, prob = FALSE, rotate = FALSE)

# Model & parameter coefficients summaries.
par_selection_sum <- summary(par_selection_pos)
par_selection_coef <- coef(par_selection_pos)

# Only for method MCMC
#confint(par_selection_coef)

# Create a table with parameter coefficients. 
par_selection_coef <- tibble::tibble(par_selection_coef$namesx,
                                     par_selection_coef$postmean,
                                     par_selection_coef$postsd, 
                                     par_selection_coef$probne0)

# Save model comparison summary. 
write.csv2(par_selection_sum, 
           file = paste0("output/", analysis_type, "/mlr_analysis/model_comparison_pos_BFR9.csv"))
           
#Save parameter coefficients table. 
write.csv2(par_selection_coef, 
           file = paste0("output/", analysis_type, "/mlr_analysis/par_selection_pos_BFR9.csv"))

# Posterior probabilities
postprob <- par_selection_pos$postprobs
par_selection_pos$postprobs

## Run model full model (apriori) ----
par_selection_full <- BAS::bas.glm(formula = full_form,
                                  data = data_mlr,
                                  # Specify a reference category if n > 2 categories for classification. 
                                  # <5 % of the data were manually coded as Negative; therefore, Negative and Neutral are pooled.
                                  family = binomial(link = "logit"),
                                  # force.heredity = true when interaction effects are modeled. 
                                  force.heredity = TRUE,
                                  initprobs = "Uniform",
                                  #betaprior = "JZS", 
                                  #betaprior = hyper.g.n(),
                                  method = "BAS",
                                  n.models =  20,
                                  bestmodel = NULL,
                                  # Choose Bernoulli instead of categorical family for binary classification.
                                  modelprior = Bernoulli(probs = 0.5))


# bas.image throws an error whenever less best models are requested for plotting than the n succesfully fit (with posterior distribution > 0)
# Report issue for bas.image on the bas developer's github 
# bas.image code: https://github.com/merliseclyde/BAS/blob/f92fc3e0e1860b171906392bef9fde6cad48a642/R/image.R
# You can see the R2 per model
par_selection_full$R2
# Best models
best <- order(-par_selection_full$postprobs)[1:20]
# "Which" contains variables per model. Get variables of best model 
selected_vars <- par_selection_full$which[[best[1]]]
# Select vars.
par_selection_full$namesx[selected_vars]

# Model inclusion probs.
par_selection_full$probne0

# Model summary.
plot(par_selection_full)

# You cannot compare models with postprobs = 0.
number_of_valid_models <- sum(par_selection_full$postprobs > 0)
image(par_selection_full, top.models = min(20, number_of_valid_models))

# Log and prob (both default true) need to be false to be able to plot even if postprobs = 0.
image(par_selection_full, top.models = 5, log = FALSE, prob = FALSE, rotate = FALSE)

# Model summary.
par_selection_sum <- summary(par_selection_full)

# Parameter coefficients summary.
par_selection_coef <- coef(par_selection_full)
# Only for MCMC
#confint(par_selection_coef)

# Create a table with parameter coefficients.
par_selection_coef <- tibble::tibble(par_selection_coef$namesx, 
                                     par_selection_coef$postmean, 
                                     par_selection_coef$postsd, 
                                     par_selection_coef$probne0)

# Save model comparison and parameter coefficients summaries. 
write.csv2(par_selection_sum, 
           file = paste0("output/", analysis_type, "/mlr_analysis/model_comparison_full_BFR9.csv"))
write.csv2(par_selection_coef, 
           file = paste0("output/", analysis_type, "/mlr_analysis/par_selection_full_BFR9.csv"))

# Posterior probabilities. 
postprob <- par_selection_full$postprobs
par_selection_full$postprobs


