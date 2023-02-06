#---
#**Aim:** 
#*"1) Perform ROC analysis to assess the classification accuracy with which 
#*automatically detected global emotional valence predicts manually coded
#*facial expression categories overall and per infant age. 
#*2) Compute manual-manual inter-rater reliability and agreement. 
#*3) Perform ROC analysis to assess the classification accuracy achieved by two 
#*manual coders on 15% of the data that was double-coded."
#*
#**Overview of basic terms in ROC analysis:**
# https://towardsdatascience.com/understanding-auc-roc-curve-68b2303cc9c5
#---

# Libraries ----
library(pROC) # multi-class ROC
library(ROCR) # binary ROC 
library(irr) # inter-rater reliability and agreement
library(splitstackshape) # splitting dataset
library(dplyr) # data wrangling and manipulation
library(readxl) # read in excel data 
library(broom) # nesting statistical analyses 
library(effectsize) # computing effect sizes
library(ggplot2) # plotting
library(magrittr) # assignment pipe operator

# Read data ----
# Dyadic interaction dataset of infants at 4 and 8 months.

# A function checking whether the automatically analyzed data with or without
# smoothing are being used, based on which to input and output paths are specified. 
if (!exists("analysis_type")){
  analysis_type <- "smooth"
} else if (!analysis_type %in% c("smooth", "raw")) {
  stop("Illegal value for analysis type. Use smooth or raw.")
}

# Based on the dataset ("smooth" or "raw"), specify the input and output paths. 
bfr_data_path <- paste0('output/', analysis_type, '/BFR_OBXT_valid_data.rds')

# Read .rds data file.
data <- readr::read_rds(bfr_data_path) %>%
  #All errors are filtered out in a previous step.
  dplyr::select(-error)

# Count the number of rows per facial expression category: FE_category.
data %>% 
  dplyr::count(FE_category, sort = TRUE) 

# Automatic-Manual Valence ----

## Data preparation ----
# Filter the relevant data, excluding events that are manually coded as "Not Visible".
data_ROC <- data %>% 
  dplyr::filter(FE_category != "Not Visible") %>%
  dplyr::mutate(
    manual_valence = base::factor(FE_category, 
                            levels = c('Negative', 'Neutral', 'Positive'),
                            ordered = TRUE), 
    automatic_valence = valence) 

# Count the recoded data, mainly check that all categories are found.
dplyr::count(data_ROC, manual_valence)

# Count the number of unique participant ID's.
base::nrow(dplyr::count(data_ROC, participant_id))

# Count the number of unique video ID's.
data_ROC %>% 
  dplyr::count(participant_id, age, int_partner) %>% 
  base::nrow(.)

# Count N participants who exhibit < 2 manually coded facial expression categories 
# Out of 289 videos, 3 infants showed only one facial expression category during 
# three interactions with an unfamiliar adult - two videos from the 4-month wave 
# and one from the 8-month wave. 
data_ROC %>% 
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  dplyr::summarize(n_categories = dplyr::n_distinct(manual_valence)) %>%
  dplyr::filter(n_categories < 2)

# We decided to not drop these participants from the ROC analysis because they 
# still yield valid facial expression data.
# data_ROC <- data_ROC %>%
#   dplyr::filter(!(participant_code %in% c("025_8E_dyadic_MG_LE", 
                                          #"034_4E_Dyadic_LvD_JT_NS_MvS", 
                                          #"057_4E_Dyadic_LvD_LvD")))

## Multi-class ROC ----
# RQ: To what extent can we distinguish between negative < neutral < positive 
# based on Automatic Valence? 
roc_all <- pROC::multiclass.roc(response = data_ROC$manual_valence, 
                                predictor = data_ROC$automatic_valence)
base::print(roc_all$auc)

# Fit the multi-class ROC curves. 
# Use print.thres = TRUE, print.thres.best.method = "youden" to display 
# the specificity & sensitivity thresholds.
pROC::plot.roc(roc_all$rocs[[1]], print.auc = TRUE, print.auc.y = .8, 
               print.auc.x = .35, xlim = c(1, 0), ylim = c(0, 1), 
               print.thres = FALSE, print.thres.best.method = "youden",
               main = "Specificity vs. Sensitivity of the Automatic Global Emotional Valence Measurement")
pROC::plot.roc(roc_all$rocs[[2]], add = TRUE, print.auc = TRUE, 
               xlim = c(1, 0), ylim = c(0, 1), print.auc.y = .3, 
               print.auc.x = .9, col = "#EB9A63", 
               print.thres = FALSE, print.thres.best.method = "youden")
pROC::plot.roc(roc_all$rocs[[3]], add = TRUE, print.auc = TRUE, 
               xlim = c(1, 0), ylim = c(0, 1), print.auc.y = .75, 
               print.auc.x = .6, col = "#23979E", 
               print.thres = FALSE, print.thres.best.method = "youden")
legend("bottomright", legend = c(
  paste0(roc_all$rocs[[1]]$levels, collapse = " vs. "),
  paste0(roc_all$rocs[[2]]$levels, collapse = " vs. "),
  paste0(roc_all$rocs[[3]]$levels, collapse = " vs. ")),
  col = c(par("fg"), "#EB9A63", "#23979E"), lwd = 2)

# Save ROC plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/ROC_AV_Multi.jpg"), 
                 plot = ggplot2::last_plot(), width = 20, height = 10, 
                 units = "cm", dpi = 600)

# Plot Confidence Intervals 
pROC::ci.auc(roc_all$rocs[[1]]) # negative vs. neutral
pROC::ci.auc(roc_all$rocs[[2]]) # negative vs. positive 
pROC::ci.auc(roc_all$rocs[[3]]) # neutral vs. positive 

## Compute F1 Scores ----
# Classification accuracy score for unbalanced groups accounting for false positive and false negative rates
# Report together with AUC

# Get precision and recall values at the best threshold value: negative vs. neutral
f1_neg_neut <- pROC::coords(roc_all$rocs[[1]], "best", 
                            ret = c("threshold", "precision", "recall")) 
# Print F1
(2 * (f1_neg_neut$precision * f1_neg_neut$recall))/(f1_neg_neut$precision + f1_neg_neut$recall)

# Get precision and recall values at the best threshold value: negative vs. positive
f1_neg_pos <- pROC::coords(roc_all$rocs[[2]], "best", 
                           ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neg_pos$precision * f1_neg_pos$recall))/(f1_neg_pos$precision + f1_neg_pos$recall)

# Get precision and recall values at the best threshold value: neutral vs. positive
f1_neut_pos <- pROC::coords(roc_all$rocs[[3]], "best", 
                            ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neut_pos$precision * f1_neut_pos$recall))/(f1_neut_pos$precision + f1_neut_pos$recall)

## Video-level ROCs ----

# Fit an ROC per video, for all observations during which the infant displayed 
# more than 2 manually coded FE categories (required for the analysis).
roc_ind <- data_ROC %>% 
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 2) %>%
  dplyr::mutate(auc = base::as.numeric(pROC::multiclass.roc(response = manual_valence, 
                                                            predictor = automatic_valence)$auc)) %>%
  dplyr::ungroup()

# Summarize per video: the AUC, mean and SD video quality, number rows analyzed.
roc_ind <- roc_ind %>% 
  dplyr::group_by(participant_id, age, int_partner_join) %>%
  dplyr::summarize(auc = first(auc), 
            video_quality_mean = mean(video_quality), 
            video_quality_sd = sd(video_quality),
            n = dplyr::n(), .groups = 'drop') 

# Save datafile.
readr::write_csv2(roc_ind, 
                  file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_ind.csv"))

# Print mean & SD AUC per video.
roc_ind %>%
  dplyr::summarize(mean_auc = mean(auc), sd_auc = sd(auc)) %>%
  View(., title = "Mean and SD Video-level AUC")

## Multi-class ROC: Age * Automatic Valence ----

# Data preparation 

# Subset the 4-month-olds' data. 
data_ROC_4 <- data_ROC %>%
  #dplyr::filter(!(participant_code %in% c("025_8E_dyadic_MG_LE", 
  #                                        "034_4E_Dyadic_LvD_JT_NS_MvS", 
  #                                        "057_4E_Dyadic_LvD_LvD"))) %>%
  dplyr::filter(age == 4)

data_ROC_4 %>%
  # Count the number of unique video ID's.
  dplyr::count(participant_id, age, int_partner) %>%
  base::nrow(.)

# Fit multi-class ROC 4 months: Automatic Valence 
roc_all_4 <- pROC::multiclass.roc(response = data_ROC_4$manual_valence, 
                                  predictor = data_ROC_4$automatic_valence)
print(roc_all_4$auc)

# Subset the 8-month-olds' data. 
data_ROC_8 <- data_ROC %>%
  #dplyr::filter(!(participant_code %in% c("025_8E_dyadic_MG_LE",
  #                                        "034_4E_Dyadic_LvD_JT_NS_MvS", 
  #                                        "057_4E_Dyadic_LvD_LvD"))) %>%
  dplyr::filter(age == 8)

# Count the number of unique video ID's.
data_ROC_8 %>%
  dplyr::count(participant_id, age, int_partner) %>%
  base::nrow(.) 

# Fit multi-class ROC 8 months: Automatic Valence.
roc_all_8 <- pROC::multiclass.roc(response = data_ROC_8$manual_valence, 
                                  predictor = data_ROC_8$automatic_valence)
print(roc_all_8$auc)

# Plot multi-ROC per age and facial expression pair.
pROC::plot.roc(roc_all_4$rocs[[1]], print.auc = TRUE, 
               print.auc.y = .8, print.auc.x = .35, 
               main = "Specificity vs. Sensitivity of the Automatic Global Emotional Valence Measurement Per Age", 
               xlim = c(1, 0), ylim = c(0,1))
pROC::plot.roc(roc_all_4$rocs[[2]], add = TRUE, print.auc = TRUE, 
               print.auc.y = .3, print.auc.x = .9, col = "#EB9A63")
pROC::plot.roc(roc_all_4$rocs[[3]], add = TRUE, print.auc = TRUE, 
               print.auc.y = .75, print.auc.x = .6, col = "#23979E")
legend("bottomright", legend = c(
  paste0(roc_all_4$rocs[[1]]$levels, collapse = "-"),
  paste0(roc_all_4$rocs[[2]]$levels, collapse = "-"),
  paste0(roc_all_4$rocs[[3]]$levels, collapse = "-")),
  col = c(par("fg"), "#EB9A63", "#23979E"), lwd = 2)
pROC::plot.roc(roc_all_8$rocs[[1]], add = TRUE, print.auc = TRUE,
               print.auc.y = .85, print.auc.x = .35,
               xlim = c(1, 0), ylim = c(0,1))
pROC::plot.roc(roc_all_8$rocs[[2]], add = TRUE, print.auc = TRUE,
               print.auc.y = .35, print.auc.x = .9, col = "#EB9A63")
pROC::plot.roc(roc_all_8$rocs[[3]], add = TRUE, print.auc = TRUE,
               print.auc.y = .79, print.auc.x = .6, col = "#23979E")
legend("bottomright", legend = c(
  paste0(roc_all_8$rocs[[1]]$levels, collapse = "-"),
  paste0(roc_all_8$rocs[[2]]$levels, collapse = "-"),
  paste0(roc_all_8$rocs[[3]]$levels, collapse = "-")),
  col = c(par("fg"), "#EB9A63", "#23979E"), lwd = 2)

## Compute F1 Scores per Age ----
# Compute F1 - 4 Months
# Get precision and recall values at the best threshold value: negative vs. neutral
f1_neg_neut_4 <- pROC::coords(roc_all_4$rocs[[1]], "best", 
                              ret = c("threshold", "precision", "recall")) 
# Print F1
(2 * (f1_neg_neut_4$precision * f1_neg_neut_4$recall))/(f1_neg_neut_4$precision + f1_neg_neut_4$recall)

# Get precision and recall values at the best threshold value: negative vs. positive
f1_neg_pos_4 <- pROC::coords(roc_all_4$rocs[[2]], "best", 
                             ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neg_pos_4$precision * f1_neg_pos_4$recall))/(f1_neg_pos_4$precision + f1_neg_pos_4$recall)

# Get precision and recall values at the best threshold value: neutral vs. positive
f1_neut_pos_4 <- pROC::coords(roc_all_4$rocs[[3]], "best", 
                              ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neut_pos_4$precision * f1_neut_pos_4$recall))/(f1_neut_pos_4$precision + f1_neut_pos_4$recall)

# Compute F1 - 8 Months
# Get precision and recall values at the best threshold value: negative vs. neutral
f1_neg_neut_8 <- pROC::coords(roc_all_8$rocs[[1]], "best", 
                              ret = c("threshold", "precision", "recall")) 
# Print F1
(2 * (f1_neg_neut_8$precision * f1_neg_neut_8$recall))/(f1_neg_neut_8$precision + f1_neg_neut_8$recall)

# Get precision and recall values at the best threshold value: negative vs. positive
f1_neg_pos_8 <- pROC::coords(roc_all_8$rocs[[2]], "best", 
                             ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neg_pos_8$precision * f1_neg_pos_8$recall))/(f1_neg_pos_8$precision + f1_neg_pos_8$recall)

# Get precision and recall values at the best threshold value: neutral vs. positive
f1_neut_pos_8 <- pROC::coords(roc_all_8$rocs[[3]], "best", 
                              ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neut_pos_8$precision * f1_neut_pos_8$recall))/(f1_neut_pos_8$precision + f1_neut_pos_8$recall)

# Individual AU's-Manual Valence ----
# To what extent can we distinguish between negative < neutral < positive based on AU12/AU20/AU3+4? 

## Multi-class ROC ----

# Select which AU to predict manual valence: AU12, AU20, AU3+4
roc_all_AU <- pROC::multiclass.roc(response = data_ROC$manual_valence, 
                                   predictor = data_ROC$AU12)
              # pROC::multiclass.roc(response = data_ROC$manual_valence, 
              #                       predictor = data_ROC$AU12)
              # pROC::multiclass.roc(response = data_ROC$manual_valence, 
              #                      predictor = data_ROC$AU20)

print(roc_all_AU$auc)
# Use print.thres = "best" to display the specificipy & sensitity thresholds.
pROC::plot.roc(roc_all_AU$rocs[[1]], 
               print.auc = TRUE, print.auc.y = .8, 
               print.auc.x = .7, print.thres = FALSE, 
               print.thres.best.method = "youden",
               main = "Specificity vs. Sensitivity Using Automatically Detected AU12", 
               xlim = c(1, 0), ylim = c(0,1))
pROC::plot.roc(roc_all_AU$rocs[[2]], add = TRUE, 
               print.auc = TRUE, print.auc.y = .3, 
               print.auc.x = .75, col = "#EB9A63", 
               print.thres = FALSE, print.thres.best.method = "youden")
pROC::plot.roc(roc_all_AU$rocs[[3]], add = TRUE, 
               print.auc = TRUE, print.auc.y = .65, 
               print.auc.x = .55, col = "#23979E", 
               print.thres = FALSE, print.thres.best.method = "youden")
legend("bottomright", legend = c(
  paste0(roc_all_AU$rocs[[1]]$levels, collapse = " vs. "),
  paste0(roc_all_AU$rocs[[2]]$levels, collapse = " vs. "),
  paste0(roc_all_AU$rocs[[3]]$levels, collapse = " vs. ")),
  col = c(par("fg"), "#EB9A63", "#23979E"), lwd = 2)

## Multi-class ROC: Age * AU's ----

# Multi-class ROC 4 months: Individual AU's
# Select which AU to predict manual valence: AU12, AU20, AU3+4
roc_all_4_AU <- pROC::multiclass.roc(response = data_ROC_4$manual_valence,
                                     predictor = data_ROC_4$AU3_4)
                # pROC::multiclass.roc(response = data_ROC_4$manual_valence,
                #                      predictor = data_ROC_4$AU12)
                # pROC::multiclass.roc(response = data_ROC_4$manual_valence,
                #                      predictor = data_ROC_4$AU20)

# Print multi-class AUC.
print(roc_all_4_AU$auc)

# Multi-class ROC 8 months: AU12.
roc_all_8_AU <- pROC::multiclass.roc(response = data_ROC_8$manual_valence, 
                                       predictor = data_ROC_8$AU3_4)
                  # pROC::multiclass.roc(response = data_ROC_8$manual_valence,
                  #                      predictor = data_ROC_8$AU20)
                  # pROC::multiclass.roc(response = data_ROC_8$manual_valence,
                  #                      predictor = data_ROC_8$AU3_4)

# Print multi-class AUC.
print(roc_all_8_AU$auc)

# Plot multi-class ROC per age and facial expression pair.
pROC::plot.roc(roc_all_4_AU$rocs[[1]], print.auc = TRUE, 
               print.auc.y = .5, print.auc.x = .35, 
               main = "Specificity vs. Sensitivity of Automatically Detected AU20 Per Age", 
               xlim = c(1, 0), ylim = c(0,1),
               print.thres = FALSE, print.thres.best.method = "youden")
pROC::plot.roc(roc_all_4_AU$rocs[[2]], add = TRUE, print.auc = TRUE, 
               print.auc.y = .3, print.auc.x = .9, col = "#EB9A63", 
               print.thres = FALSE, print.thres.best.method = "youden")
pROC::plot.roc(roc_all_4_AU$rocs[[3]], add = TRUE, print.auc = TRUE, 
               print.auc.y = .75, print.auc.x = .6, col = "#23979E", 
               print.thres = FALSE, print.thres.best.method = "youden")
legend("bottomright", legend = c(
  paste0(roc_all_4_AU$rocs[[1]]$levels, collapse = "-"),
  paste0(roc_all_4_AU$rocs[[2]]$levels, collapse = "-"),
  paste0(roc_all_4_AU$rocs[[3]]$levels, collapse = "-")),
  col = c(par("fg"), "#EB9A63", "#23979E"), lwd = 2)
pROC::plot.roc(roc_all_8_AU$rocs[[1]], add = TRUE, print.auc = TRUE,
               print.auc.y = .85, print.auc.x = .35,
               xlim = c(1, 0), ylim = c(0,1), 
               print.thres = FALSE, print.thres.best.method = "youden")
pROC::plot.roc(roc_all_8_AU$rocs[[2]], add = TRUE, print.auc = TRUE,
               print.auc.y = .35, print.auc.x = .9, col = "#EB9A63", 
               print.thres = FALSE, print.thres.best.method = "youden")
pROC::plot.roc(roc_all_8_AU$rocs[[3]], add = TRUE, print.auc = TRUE,
               print.auc.y = .79, print.auc.x = .6, col = "#23979E", 
               print.thres = T, print.thres.best.method = "youden")
legend("bottomright", legend = c(
  paste0(roc_all_8$rocs[[1]]$levels, collapse = " vs. "),
  paste0(roc_all_8$rocs[[2]]$levels, collapse = " vs. "),
  paste0(roc_all_8$rocs[[3]]$levels, collapse = " vs. ")),
  col = c(par("fg"), "#EB9A63", "#23979E"), lwd = 2)

# Plot Confidence Intervals: 4 Months 
pROC::ci.auc(roc_all_4_AU$rocs[[1]]) # negative vs. neutral
pROC::ci.auc(roc_all_4_AU$rocs[[2]]) # negative vs. positive 
pROC::ci.auc(roc_all_4_AU$rocs[[3]]) # neutral vs. positive 

# Plot Confidence Intervals: 8 Months 
pROC::ci.auc(roc_all_8_AU$rocs[[1]]) # negative vs. neutral
pROC::ci.auc(roc_all_8_AU$rocs[[2]]) # negative vs. positive 
pROC::ci.auc(roc_all_8_AU$rocs[[3]]) # neutral vs. positive 

## Compute F1 Scores per Age ----

# Compute F1 for individual AU's Overall 
print(roc_all_AU)
# Get precision and recall values at the best threshold value: negative vs. neutral
f1_neg_neut_AU <- pROC::coords(roc_all_AU$rocs[[1]], "best", 
                               ret = c("threshold", "precision", "recall")) 
# Print F1
(2 * (f1_neg_neut_AU$precision * f1_neg_neut_AU$recall))/(f1_neg_neut_AU$precision + f1_neg_neut_AU$recall)

# Get precision and recall values at the best threshold value: negative vs. positive
f1_neg_pos_AU <- pROC::coords(roc_all_AU$rocs[[2]], "best",
                              ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neg_pos_AU$precision * f1_neg_pos_AU$recall))/(f1_neg_pos_AU$precision + f1_neg_pos_AU$recall)

# Get precision and recall values at the best threshold value: neutral vs. positive
f1_neut_pos_AU <- pROC::coords(roc_all_AU$rocs[[3]], "best",
                               ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neut_pos_AU$precision * f1_neut_pos_AU$recall))/(f1_neut_pos_AU$precision + f1_neut_pos_AU$recall)

# Compute F1 for individual AU's: positive vs. negative/neutral
print(roc_all_4_AU)
# Compute F1 for individual AU's at 4 Months
# Get precision and recall values at the best threshold value: negative vs. neutral
f1_neg_neut_4_AU <- pROC::coords(roc_all_4_AU$rocs[[1]], "best", 
                                 ret = c("threshold", "precision", "recall")) 
# Print F1
(2 * (f1_neg_neut_4_AU$precision * f1_neg_neut_4_AU$recall))/(f1_neg_neut_4_AU$precision + f1_neg_neut_4_AU$recall)

# Get precision and recall values at the best threshold value: negative vs. positive
f1_neg_pos_4_AU <- pROC::coords(roc_all_4_AU$rocs[[2]], "best",
                                ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neg_pos_4_AU$precision * f1_neg_pos_4_AU$recall))/(f1_neg_pos_4_AU$precision + f1_neg_pos_4_AU$recall)

# Get precision and recall values at the best threshold value: neutral vs. positive
f1_neut_pos_4_AU <- pROC::coords(roc_all_4_AU$rocs[[3]], "best",
                                   ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neut_pos_4_AU$precision * f1_neut_pos_4_AU$recall))/(f1_neut_pos_4_AU$precision + f1_neut_pos_4_AU$recall)


# Compute F1 for individual AU's at 8 Months
print(roc_all_8_AU)
# Get precision and recall values at the best threshold value: negative vs. neutral
f1_neg_neut_8_AU <- pROC::coords(roc_all_8_AU$rocs[[1]], "best",
                                 ret = c("threshold", "precision", "recall")) 
# Print F1
(2 * (f1_neg_neut_8_AU$precision * f1_neg_neut_8_AU$recall))/(f1_neg_neut_8_AU$precision + f1_neg_neut_8_AU$recall)

# Get precision and recall values at the best threshold value: negative vs. positive
f1_neg_pos_8_AU <- pROC::coords(roc_all_8_AU$rocs[[2]], "best",
                                ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neg_pos_8_AU$precision * f1_neg_pos_8_AU$recall))/(f1_neg_pos_8_AU$precision + f1_neg_pos_8_AU$recall)

# Get precision and recall values at the best threshold value: neutral vs. positive
f1_neut_pos_8_AU <- pROC::coords(roc_all_8_AU$rocs[[3]], "best",
                                   ret = c("threshold", "precision", "recall"))
# Print F1
(2 * (f1_neut_pos_8_AU$precision * f1_neut_pos_8_AU$recall))/(f1_neut_pos_8_AU$precision + f1_neut_pos_8_AU$recall)

## Video-level ROCs: Individual AU's * Age  ----

# Perform ROC on pp who display more than 1 manually coded FE categories (required for the analysis).
data_ROC_AU <- data_ROC %>% 
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 2) %>%
  # Select which AU should predict manual valence:
  dplyr::mutate(auc = as.numeric(pROC::multiclass.roc(response = manual_valence, 
                                                      predictor = AU12)$auc)) %>%
  # dplyr::mutate(auc = as.numeric(pROC::multiclass.roc(response = manual_valence, 
  #                                                     predictor = AU20)$auc)) %>%
  # dplyr::mutate(auc = as.numeric(pROC::multiclass.roc(response = manual_valence, 
  #                                                     predictor = AU3_4)$auc)) %>%
  dplyr::ungroup()

# Summarize per video: the AUC, mean and SD video quality, number rows analyzed.
data_ROC_AU <- data_ROC_AU %>% 
  dplyr::group_by(participant_id, age, int_partner_join) %>%
  dplyr::summarize(auc = dplyr::first(auc), 
                   video_quality_mean = mean(video_quality), 
                   video_quality_sd = sd(video_quality),
                   n = dplyr::n(), .groups = 'drop') 

# Save datafile.
readr::write_csv2(data_ROC_AU, 
                  file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_AU12.csv"))
                  #file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_AU20.csv"))
                  #file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_AU3_4.csv"))

# Print mean & SD AUC per video.
data_ROC_AU %>%
  dplyr::summarize(mean_auc = mean(auc), sd_auc = sd(auc)) %>%
  View(., title = "Mean & SD AUC per video")

# Binary ROC Analysis ----

## ROC: Positive vs. Negative/Neutral: Automatic Valence ----

# Prepare the dataset: Filter out data that was manually coded as "Not Visible" &
# recode the levels of the manual_valence variable. 
data_ROC_pos <- data %>% 
  dplyr::filter(FE_category != "Not Visible") %>%
  # Recode FE_category into "positive" vs "rest"
  dplyr::mutate(manual_valence = droplevels(recode_factor(FE_category, 
                                                          `Negative` = "Negative/Neutral", 
                                                          `Neutral` = "Negative/Neutral", 
                                                          `Positive` = "Positive")), 
                automatic_valence = valence) 

# Nest the data within video
f1_pos_rest <- data_ROC_pos %>%
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  # Filter out infants who do not display both manually coded facial expressions 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 1)
# Perform a binary ROC
f1_pos_rest <-  pROC::multiclass.roc(response = f1_pos_rest$manual_valence,
                          predictor = f1_pos_rest$automatic_valence,
                          direction = ">", 
                          levels = c("Positive", "Negative/Neutral"))

# Plot the binary ROC curve for positive vs rest and the curve for negative vs. neutral
pROC::plot.roc(f1_pos_rest$rocs[[1]], print.auc = TRUE, 
               print.auc.y = .75, print.auc.x = .6, col = "#23979E", 
               xlim = c(1, 0), ylim = c(0, 1), 
               print.thres = FALSE, print.thres.best.method = "youden",
               main = "Specificity vs. Sensitivity of the Automatic Global Emotional Valence Measurement")
pROC::plot.roc(roc_all$rocs[[1]], add = TRUE, print.auc = TRUE, 
               print.auc.y = .3, print.auc.x = .6, col = "#EB9A63", 
               xlim = c(1, 0), ylim = c(0, 1),
               print.thres = FALSE, print.thres.best.method = "youden")
legend("bottomright", 
       legend = c(
         paste0(f1_pos_rest$rocs[[1]]$levels, collapse = " vs. "),
         paste0(roc_all$rocs[[1]]$levels, collapse = " vs. ")),
       col = c("#23979E", "#EB9A63"), lwd = 2)

# Get precision and recall values at the best threshold value: positive vs. rest
f1_pos_rest <- pROC::coords(f1_pos_rest$rocs[[1]], "best", 
               ret = c("threshold", "precision", "recall", "specificity"))

# Print F1
(2 * (f1_pos_rest$precision * f1_pos_rest$recall))/(f1_pos_rest$precision + f1_pos_rest$recall)

# Perform ROC on pp who display more than 1 manually coded FE categories.
data_ROC_pos <- data_ROC_pos %>% 
  # Nest the data within video
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  # Filter out infants who do not display both manually coded facial expressions 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 1) %>%
  dplyr::mutate(auc = as.numeric(pROC::multiclass.roc(response = manual_valence,
                                                      predictor = automatic_valence,
                                                      direction = "<")$auc)) %>%
  dplyr::ungroup()

# Save the dataset as .rds file.
saveRDS(data_ROC_pos, 
        file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_pos.rds"))

# Summarize per video: the AUC, mean and SD video quality, number rows analyzed.
data_ROC_pos <- data_ROC_pos %>% 
  dplyr::group_by(participant_id, age, int_partner_join) %>%
  dplyr::summarize(auc = dplyr::first(auc), 
            video_quality_mean = mean(video_quality), 
            video_quality_sd = sd(video_quality),
            n = dplyr::n(), .groups = 'drop') 

# Print mean & SD AUC per video.
data_ROC_pos %>%
  dplyr::summarize(mean_auc = mean(auc), sd_auc = sd(auc)) %>%
  View(., title = "Mean & SD AUC per video")


## ROC: Positive vs. Negative/Nuetral AU12 ----

# Prepare the dataset: Filter out data that was manually coded as "Not Visible" &
# recode the levels of the manual_valence variable. 
data_ROC_pos_AU12 <- data %>% 
  dplyr::filter(FE_category != "Not Visible") %>%
  # Recode FE_category into "positive" vs "rest"
  dplyr::mutate(manual_valence = droplevels(recode_factor(FE_category, 
                                                          `Negative` = "Negative/Neutral", 
                                                          `Neutral` = "Negative/Neutral", 
                                                          `Positive` = "Positive"))) 

# Nest the data within video
f1_pos_rest_AU12 <- data_ROC_pos_AU12 %>%
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  # Filter out infants who do not display both manually coded facial expressions 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 1)
# Perform a binary ROC
f1_pos_rest_AU12 <-  pROC::multiclass.roc(response = f1_pos_rest_AU12$manual_valence,
                                     predictor = f1_pos_rest_AU12$AU12,
                                     direction = ">", 
                                     levels = c("Positive", "Negative/Neutral"))

# Plot the binary ROC curve for positive vs rest and the curve for negative vs. neutral
pROC::plot.roc(f1_pos_rest_AU12$rocs[[1]], print.auc = TRUE, 
               print.auc.y = .75, print.auc.x = .6, col = "#23979E", 
               xlim = c(1, 0), ylim = c(0, 1), 
               print.thres = FALSE, print.thres.best.method = "youden",
               main = "Specificity vs. Sensitivity of the Automatic AU12 Measurement")
pROC::plot.roc(roc_all_AU$rocs[[1]], add = TRUE,
               print.auc = TRUE, print.auc.y = .3,
               print.auc.x = .9, col = "#EB9A63",
               print.thres = FALSE, print.thres.best.method = "youden")
legend("bottomright", 
       legend = c(
         paste0(f1_pos_rest_AU12$rocs[[1]]$levels, collapse = " vs. "),
         paste0(roc_all_AU$rocs[[1]]$levels, collapse = " vs. ")),
       col = c("#23979E", "#EB9A63"), lwd = 2)

# Get precision and recall values at the best threshold value: positive vs. rest
f1_pos_rest_AU12 <- pROC::coords(f1_pos_rest_AU12$rocs[[1]], "best", 
                            ret = c("threshold", "precision", "recall", "specificity"))

# Print F1
(2 * (f1_pos_rest_AU12$precision * f1_pos_rest_AU12$recall))/(f1_pos_rest_AU12$precision + f1_pos_rest_AU12$recall)

## ROC: Negative vs. Neutral/Positive: Automatic Valence ----

# Prepare the dataset: Filter out data that was manually coded as "Not Visible" &
# recode the levels of the manual_valence variable. 
data_ROC_neg <- data %>% 
  dplyr::filter(FE_category != "Not Visible") %>%
  # Recode FE_category into "positive" vs "rest"
  dplyr::mutate(manual_valence = droplevels(recode_factor(FE_category, 
                                                          `Negative` = "Negative", 
                                                          `Neutral` = "Neutral/Positive", 
                                                          `Positive` = "Neutral/Positive")), 
                automatic_valence = valence) 

# Nest the data within video
f1_neg_rest <- data_ROC_neg %>%
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  # Filter out infants who do not display both manually coded facial expressions 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 1)
# Perform a binary ROC
f1_neg_rest <-  pROC::multiclass.roc(response = f1_neg_rest$manual_valence,
                                     predictor = f1_neg_rest$automatic_valence,
                                     direction = "<", 
                                     levels = c("Negative", "Neutral/Positive"))


# Get precision and recall values at the best threshold value: positive vs. rest
f1_neg_rest <- pROC::coords(f1_neg_rest$rocs[[1]], "best", 
                            ret = c("threshold", "precision", "recall", "specificity"))

# Print F1
(2 * (f1_neg_rest$precision * f1_neg_rest$recall))/(f1_neg_rest$precision + f1_neg_rest$recall)

# Perform ROC on pp who display more than 1 manually coded FE categories.
data_ROC_neg <- data_ROC_neg %>% 
  # Nest the data within video
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  # Filter out infants who do not display both manually coded facial expressions 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 1) %>%
  dplyr::mutate(auc = as.numeric(pROC::multiclass.roc(response = manual_valence,
                                                      predictor = automatic_valence,
                                                      direction = "<")$auc)) %>%
  dplyr::ungroup()

## ROC: Negative vs. Neutral ----

# Prepare the dataset: Filter out data that was manually coded as "Not Visible" &
# recode the levels of the manual_valence variable. 
data_ROC_neg <- data %>% 
  dplyr::filter(FE_category != "Not Visible" & FE_category != "Positive") %>%
  # negative vs rest
  dplyr::mutate(manual_valence = base::droplevels(dplyr::recode_factor(
                                                    FE_category, 
                                                   `Negative` = "Negative", 
                                                   `Neutral` = "Neutral")), 
                automatic_valence = valence)

# Nest the data within video
f1_neg_neut <- data_ROC_neg %>%
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  # Filter out infants who do not display both manually coded facial expressions 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 1)
# Perform a binary ROC
f1_neg_neut <-  pROC::multiclass.roc(response = f1_neg_neut$manual_valence,
                                     predictor = f1_neg_neut$automatic_valence,
                                     direction = "<", 
                                     levels = c("Negative", "Neutral"))

# Plot the binary ROC curves for positive vs rest and negative vs rest
pROC::plot.roc(f1_pos_rest$rocs[[1]], print.auc = TRUE, 
               print.auc.y = .75, print.auc.x = .6, col = "#23979E", 
               xlim = c(1, 0), ylim = c(0, 1), 
               print.thres = FALSE, print.thres.best.method = "youden",
               main = "Specificity vs. Sensitivity of the Automatic Global Emotional Valence Measurement")
pROC::plot.roc(f1_neg_neut$rocs[[1]], add = TRUE, print.auc = TRUE, 
               print.auc.y = .3, print.auc.x = .6, col = "#EB9A63", 
               xlim = c(1, 0), ylim = c(0, 1),
               print.thres = FALSE, print.thres.best.method = "youden")
legend("bottomright", 
       legend = c(
         paste0(f1_pos_rest$rocs[[1]]$levels, collapse = "-"),
         paste0(f1_neg_neut$rocs[[1]]$levels, collapse = "-")),
       col = c("#23979E", "#EB9A63"), lwd = 2)

# Get precision and recall values at the best threshold value: positive vs. rest
f1_neg_neut <- pROC::coords(f1_neg_neut$rocs[[1]], "best", 
                            ret = c("threshold", "precision", "recall", "specificity"))

# Print F1
(2 * (f1_neg_neut$precision * f1_neg_neut$recall))/(f1_neg_neut$precision + f1_neg_neut$recall)

# Perform ROC on pp who display more than 1 manually coded FE categories.
data_ROC_neg  <- data_ROC_neg %>% 
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 1) %>%
  dplyr::mutate(auc = as.numeric(pROC::multiclass.roc(response = manual_valence, 
                                                      predictor = automatic_valence, 
                                                      direction = "<")$auc)) %>%
  dplyr::ungroup()

# Save the dataset as .rds file.
saveRDS(data_ROC_neg, 
        file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_neg.rds"))

# Summarize per video: the AUC, mean and SD video quality, number rows analyzed.
data_ROC_neg <- data_ROC_neg %>% 
  dplyr::group_by(participant_id, age, int_partner_join) %>%
  dplyr::summarize(auc = dplyr::first(auc), 
            video_quality_mean = mean(video_quality), 
            video_quality_sd = sd(video_quality),
            n = dplyr::n(), .groups = 'drop') 

# Print mean & SD AUC per video.
data_ROC_neg %>%
  dplyr::summarize(mean_auc = mean(auc), sd_auc = sd(auc)) %>%
  View(., title = "Mean & SD AUC per video")

## ROC: Negative vs. Neutral AU20/AU3+4 ----

# Prepare the dataset: Filter out data that was manually coded as "Not Visible" &
# recode the levels of the manual_valence variable. 
data_ROC_neg_AU <- data %>% 
  dplyr::filter(FE_category != "Not Visible") %>%
  # Recode FE_category into "positive" vs "rest"
  dplyr::mutate(manual_valence = droplevels(recode_factor(FE_category, 
                                                          `Negative` = "Negative", 
                                                          `Neutral` = "Neutral", 
                                                          `Positive` = "Negative"))) 

# Nest the data within video
f1_neg_neut_AU <- data_ROC_neg_AU %>%
  dplyr::group_by(participant_id, age, int_partner_join) %>% 
  # Filter out infants who do not display both manually coded facial expressions 
  dplyr::filter(dplyr::n_distinct(manual_valence) > 1)
# Perform a binary ROC
f1_neg_neut_AU <- pROC::multiclass.roc(response = f1_neg_neut_AU$manual_valence,
                                        predictor = f1_neg_neut_AU$AU20,
                                        direction = ">", 
                                        levels = c("Negative", "Neutral", "Positive"))

# Plot the binary ROC curve for positive vs rest and the curve for negative vs. neutral
pROC::plot.roc(f1_neg_neut_AU$rocs[[1]], print.auc = TRUE, 
               print.auc.y = .8, print.auc.x = .6, 
               xlim = c(1, 0), ylim = c(0, 1), col = "#23979E",
               print.thres = TRUE, print.thres.best.method = "youden",
               main = "Specificity vs. Sensitivity of the Automatic AU20 Measurement")
pROC::plot.roc(f1_neg_neut_AU$rocs[[2]], add = TRUE, 
               print.auc = TRUE, print.auc.y = .3, 
               print.auc.x = .9, col = "#EB9A63", 
               print.thres = TRUE, print.thres.best.method = "youden")
pROC::plot.roc(f1_neg_neut_AU$rocs[[3]], add = TRUE, 
               print.auc = TRUE, print.auc.y = .75, 
               print.auc.x = .6, col = "#23979E", 
               print.thres = TRUE, print.thres.best.method = "youden")
legend("bottomright", 
       legend = c(
       paste0(f1_neg_neut_AU$rocs[[1]]$levels, collapse = " vs. "),
       paste0(f1_neg_neut_AU$rocs[[2]]$levels, collapse = " vs. "), 
       paste0(f1_neg_neut_AU$rocs[[3]]$levels, collapse = " vs. ")),
       col = c(par("fg"), "#EB9A63", "#23979E"), lwd = 2)

# Get precision and recall values at the best threshold value: negative vs. neutral
f1_neg_neut_AU <- pROC::coords(f1_neg_neut_AU$rocs[[1]], "best", 
                               ret = c("threshold", "precision", "recall", "specificity"))

# Print F1
(2 * (f1_neg_neut_AU$precision * f1_neg_neut_AU$recall))/(f1_neg_neut_AU$precision + f1_neg_neut_AU$recall)

# Age, Interaction Partner, Face Model Fit Quality ----

## Positive vs. Rest ----
# AUC for 4 vs 8MO.  
boxplot(auc ~ age, data = data_ROC_pos)

# AUC across interaction partners.
boxplot(auc ~ int_partner_join, data = data_ROC_pos)

# Age to predict video quality.
boxplot(video_quality_mean ~ age, data = data_ROC_pos)

# Use age, interaction partner & video quality to predict AUC.
data_ROC_pos$age <- base::as.factor(data_ROC_pos$age)
data_ROC_pos$int_partner_join <- base::as.factor(data_ROC_pos$int_partner_join)
anova_pos <- stats::lm(auc ~ age + int_partner_join + video_quality_mean, data = data_ROC_pos)
base::summary(anova_pos)

# Compute effect size: eta squared. 
effectsize::eta_squared(anova_pos, partial = TRUE, ci = 0.95)

# Recode age and interaction partner as dummy variables. 
data_ROC_pos_long <- data_ROC_pos %>%
  dplyr::mutate(int_partner_mother = dplyr::recode_factor(int_partner_join, 
                                                          `1` = 1, 
                                                          `0` = 0, 
                                                          `2` = 0), 
         int_partner_father = dplyr::recode_factor(int_partner_join, 
                                                   `1` = 0, 
                                                   `0` = 1, 
                                                   `2` = 0), 
         age = dplyr::recode_factor(age, 
                                    `4`= 0, 
                                    `8` = 1)) 

# Save data file.
readr::write_csv2(data_ROC_pos_long, 
                  file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_pos_long.csv"), 
                  na = "NA")

# Recode from long to wide data format.
data_ROC_pos_wide <- data_ROC_pos %>%
  dplyr::mutate(int_partner_join = dplyr::recode_factor(int_partner_join, 
                                                        `1` = "Mother",
                                                        `0` = "Father",
                                                        `2` = "Unfamiliar Adult")) %>% 
  tidyr::pivot_wider(., 
                     id_cols = c(participant_id),
                     names_from = c(age, int_partner_join),
                     values_from = c(auc, video_quality_mean, video_quality_sd, n))

# Save data file.
readr::write_csv2(data_ROC_pos_wide, 
                  file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_pos_wide.csv"), 
                  na = "NA")

## Negative vs. Rest ----

# AUC 4MO vs 8MO.
boxplot(auc ~ age, data = data_ROC_neg)

# AUC across interaction partners.
boxplot(auc ~ int_partner_join, data = data_ROC_neg)

# Age to predict video quality - negative ROC data.
boxplot(video_quality_mean ~ age, data = data_ROC_neg)

# Age to predict video quality.
boxplot(video_quality ~ age, data = data_ROC)

# Use age, interaction partner & video quality to predict AUC.
data_ROC_neg$age <- base::as.factor(data_ROC_neg$age)
data_ROC_neg$int_partner_join <- base::as.factor(data_ROC_neg$int_partner_join)
anova_neg <- stats::lm(auc ~ age + int_partner_join + video_quality_mean, data = data_ROC_neg)
summary(anova_neg)

# Recode age and interaction partner as dummy variables. 
data_ROC_neg_long <- data_ROC_neg %>%
  dplyr::mutate(int_partner_mother = dplyr::recode_factor(int_partner_join, 
                                                          `1` = 1, 
                                                          `0` = 0,
                                                          `2` = 0), 
         int_partner_father = dplyr::recode_factor(int_partner_join,
                                                   `1` = 0, 
                                                   `0` = 1, 
                                                   `2` = 0), 
         age = dplyr::recode_factor(age,
                                    `4`= 0,
                                    `8` = 1)) 

# Save data file.
readr::write_csv2(data_ROC_neg_long, 
                  file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_neg_long.csv"), 
                  na = "NA")

# Recode from long to wide data format.
data_ROC_neg_wide <- data_ROC_neg %>%
  mutate(int_partner_join = dplyr::recode_factor(int_partner_join, 
                                          `1` = "Mother",
                                          `0` = "Father",
                                          `2` = "Unfamiliar Adult")) %>% 
  tidyr::pivot_wider(., 
                     id_cols = c(participant_id),
                     names_from = c(age, int_partner_join),
                     values_from = c(auc, video_quality_mean, video_quality_sd, n))

# Save data file. 
readr::write_csv2(data_ROC_neg_wide, 
                  file = paste0("output/", analysis_type, "/roc_analysis/data_ROC_neg_wide.csv"), 
                  na = "NA")

# Manual-Manual Valence ----

## Read & restructure data ----
# Specify the directory to the dataset containing a subset of the manually coded 
# dataset that has been double-coded for computing manual-manual inter-rater 
# reliability and ROC curves.  
obxt_data_path <- 'data/OBXT_data/OBXT_0408MO_dyadic_reliability.xlsx'

# Read in the data file with a high guess_max - slows down loading time but
# does not require to specify each column type.
OBXT_irr_data_base <- readxl::read_xlsx(obxt_data_path, guess_max = 500000, progress = TRUE)

# Order and rename variables.
OBXT_irr_data <- OBXT_irr_data_base %>% 
  dplyr::filter(`Behavior...14` != "Not Visible") %>%
  dplyr::mutate(participant_id = base::as.numeric(stringr::str_match(Observation, '^(\\d+)')[,2]), 
         age = as.numeric(stringr::str_match(Observation, '^\\d+_(\\d)')[,2]), 
         int_partner = stringr::str_match(Observation, '\\d+_\\d(.)_')[,2], 
         observer = stringr::str_match(Observation, '_([a-zA-Z\\d]*)$')[,2], 
         participant_code = Observation, 
         FE_category = base::factor(`Behavior...14`, ordered = TRUE, 
                              levels = c(levels = "Negative", "Neut", "Positive"),
                              labels = c("negative", "neutral", "positive")),
         ts = `Time_Relative_sf`) %>%
  dplyr::select(participant_code, participant_id, age, int_partner, observer, ts, FE_category)

# Overview of coders per video
overview_irr <- OBXT_irr_data %>% 
  dplyr::count(participant_id, age, int_partner, observer) 

# Overview of coders per participant
OBXT_irr_data %>% 
  dplyr::count(participant_id, observer) %>%
  dplyr::count(participant_id, sort = TRUE) %>%
  dplyr::arrange(n) %>%
  View()

# Create a variable to store first and second coders
OBXT_irr_data <- OBXT_irr_data %>% 
  dplyr::group_by(participant_id, age, int_partner) %>%
  dplyr::mutate(observer_id = dplyr::if_else(observer == base::min(observer), 1, 2)) %>%
  dplyr::ungroup()

#  Check for missing data
OBXT_irr_data %>%
  dplyr::filter(is.na(int_partner))

# Overview of coders per participant
overview_irr <- OBXT_irr_data %>% 
  dplyr::count(participant_id, age, int_partner, observer_id) 

OBXT_irr_data %>%
  dplyr::count(participant_id, int_partner, ts) %>%
  dplyr::filter(n != 2) %>%
  View()

# Long to wide format
OBXT_irr_data_wide <- OBXT_irr_data %>%
  tidyr::pivot_wider(., 
                     id_cols = c(participant_id, age, int_partner, ts),
                     names_from = c(observer_id),
                     names_prefix = "coder_",
                     values_from = c(FE_category))

OBXT_irr_data_wide %>% dplyr::summarize_all(~ sum(is.na(.x)))
OBXT_irr_data_wide %>% dplyr::count(coder_1, coder_2) %>%
  View()
OBXT_irr_data_wide %>% dplyr::count(participant_id, coder_1, coder_2) %>%
  View()

## Inter-rater reliability & agreement ----

## Subset the two columns containing the rater judgments for the 4-month wave.
OBXT_irr_kappa_4 <- OBXT_irr_data_wide %>%
  dplyr::filter(age == 4) %>%
  dplyr::select(coder_1, coder_2)

# Compute Cohen's kappa
irr::kappa2(OBXT_irr_kappa_4, weight = "unweighted", sort.levels = FALSE)

# Compute percentage agreement 
irr::agree(OBXT_irr_kappa_4, tolerance = 0)

## Positive at 4 months
## Subset the two columns containing the rater judgments for the 4-month wave.
OBXT_irr_kappa_4_pos <- OBXT_irr_data_wide %>%
  dplyr::filter(age == 4) %>%
  dplyr::select(coder_1, coder_2)

# Compute Cohen's kappa
irr::kappa2(OBXT_irr_kappa_4_pos, weight = "unweighted", sort.levels = FALSE)

# Compute percentage agreement 
irr::agree(OBXT_irr_kappa_4_pos, tolerance = 0)

## Subset the two columns containing the rater judgments for the 8-month wave.
OBXT_irr_kappa_8 <- OBXT_irr_data_wide %>%
  filter(age == 8) %>%
  dplyr::select(coder_1, coder_2)

# Compute Cohen's kappa
irr::kappa2(OBXT_irr_kappa_8, weight = "unweighted", sort.levels = FALSE)

# Compute percentage agreement
irr::agree(OBXT_irr_kappa_8, tolerance = 0)

## Multi-class ROC ----

# Perform the multi-class ROC & plot it. 
# To what extent can we distinguish between 
# negative < neutral  < positive based on Automatic Valence? 
roc_mm <- pROC::multiclass.roc(response = OBXT_irr_data_wide$coder_1, 
                               predictor = OBXT_irr_data_wide$coder_2)
print(roc_mm$auc)
# Use print.thres = "best" to display the specificity / sensitity thresholds furthest from chance. 
pROC::plot.roc(roc_mm$rocs[[1]], print.auc = TRUE, 
               print.auc.y = .8, print.auc.x = .35,
               main = "Specificity vs. Sensitivity of the Automatic Facial Expression Valence Classifier", 
               xlim = c(1, 0), ylim = c(0,1))
pROC::plot.roc(roc_mm$rocs[[2]], add = TRUE, print.auc = TRUE, 
               print.auc.y = .3, print.auc.x = .9, col = "#EB9A63")
pROC::plot.roc(roc_mm$rocs[[3]], add = TRUE, print.auc = TRUE, 
               print.auc.y = .75, print.auc.x = .6, col = "#23979E")
legend("bottomright", legend = c(
  paste0(roc_mm$rocs[[1]]$levels, collapse = "-"),
  paste0(roc_mm$rocs[[2]]$levels, collapse = "-"),
  paste0(roc_mm$rocs[[3]]$levels, collapse = "-")),
  col = c(par("fg"), "#EB9A63", "#23979E"), lwd = 2)

# Video-level ROCs
roc_manual_manual  <- OBXT_irr_data_wide %>% 
  dplyr::group_by(participant_id, age, int_partner) %>% 
  dplyr::mutate(auc = as.numeric(pROC::multiclass.roc(response = coder_1, 
                                                      predictor = coder_2, 
                                                      direction = "<")$auc)) %>%
  dplyr::ungroup()

# Summarize per video: the AUC, number rows analyzed.
roc_manual_manual <- roc_manual_manual %>% 
  dplyr::group_by(participant_id, age, int_partner) %>% 
  dplyr::summarize(auc = dplyr::first(auc), 
            auc_sd = sd(auc),
            n = dplyr::n(), .groups = 'drop') 

# ROC Power ----

## Positive vs Negative/Neurtal ----
# rocobj <- pROC::roc(response = data_ROC_pos$manual_valence,
#                                predictor = data_ROC_pos$automatic_valence, 
#                                direction = "<")
# # Number of cases and controls
# ncases <- 133207
# ncontrols <- 282904

## Negative vs. Positive/Neurtal ----
# rocobj <- pROC::roc(response = data_ROC_neg$manual_valence,
#                     predictor = data_ROC_neg$automatic_valence, 
#                     direction = "<")
# # Number of cases and controls
# ncases <- 238370
# ncontrols <- 21676

# Compute power based on the following AU intensities: 
# 0 - 0.100 = not active
# 0.100 - 0.217 = A
# 0.217 - 0.334 = B
# 0.334 - 0.622 = C
# 0.622 - 0.910 = D
# 0.910 - 1.000 = E

## AU12 ----
ncases <- data_ROC %>% dplyr::filter(AU12 > .1) %>% dplyr::count() %>% as.numeric()
  
ncontrols <- data_ROC %>% dplyr::filter(AU12 <= .1) %>% dplyr::count() %>% as.numeric()

## AU20 ----
ncases <- data_ROC %>% dplyr::filter(AU20 > .1) %>% dplyr::count() %>% as.numeric()

ncontrols <- data_ROC %>% dplyr::filter(AU20 <= .1) %>% dplyr::count() %>% as.numeric()

## AU3+4 ----
ncases <- data_ROC %>% dplyr::filter(AU3_4 > .1) %>% dplyr::count() %>% as.numeric()

ncontrols <- data_ROC %>% dplyr::filter(AU3_4 <= .1) %>% dplyr::count() %>% as.numeric()

# Determine power for one ROC curve from the count cases and controls for an AUC of .80.
power.roc.test(ncases = ncases, ncontrols = ncontrols, auc = 0.8)

# Determine ncases & ncontrols
# kappa is the ratio of controls to cases
power.roc.test(auc = .7, sig.level = 0.0011, power = .99, kappa = ncontrols/ncases)

# ROC: Hypothesis data (used in pre-registration) ----
## Read data ----
# Dataset created for script- and hypothesis-building only. 
data_eg <- readxl::read_xlsx("data/heatmap_data.xlsx", sheet = "unbiased")

# Expand rows to dis-aggregate counts
data_long <- data_eg %>% 
  dplyr::rename_all(tolower) %>% 
  splitstackshape::expandRows("freq")

## ROC analysis ----
# Plot a ROC function depicting the trade-off between True and False Positive Rate 
# per facial expression category. 
par(mfrow = c(2, 2))  # Set up a 2 x 2 plotting space
for (outcome in c('Not codable', 'Negative',  'Neutral', 'Positive')){
  # Reduce the automatic valence variable to a binary outcome.
  data_long_plot <- data_long %>% dplyr::mutate(human_binary = dplyr::if_else(human == outcome, 1, 0),
                                         baby_facereader_binary = dplyr::if_else(baby_facereader == outcome, 1, 0)) 
  predictions <- data_long_plot$baby_facereader_binary
  labels <- data_long_plot$human_binary 
  pred <- ROCR::prediction(predictions, labels)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  ROCR::plot(perf, colorize = FALSE, main = outcome)
  abline(a = 0, b = 1, lty = 2, lwd = 3, col = "red")
}

# Plot the automatic vs. manual categories against each other. 
data_long_plot %>% 
  dplyr::count(human_binary, baby_facereader_binary)

# Plot a ROC function depicting the trade-off between True and False Positive Rate 
# for all facial expression categories on the same plot space. 
par(mfrow = c(1, 1))
for (outcome in c('Not codable', 'Negative',  'Neutral', 'Positive')){
  add = ifelse(outcome == 'Not codable', FALSE, TRUE)
  data_long_plot <- data_long %>% dplyr::mutate(human_binary = dplyr::if_else(human == outcome, 1, 0),
                                         baby_facereader_binary = dplyr::if_else(baby_facereader == outcome, 1, 0)) 
  predictions <- data_long_plot$baby_facereader_binary
  labels <- data_long_plot$human_binary 
  pred <- ROCR::prediction(predictions, labels)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  ROCR::plot(perf, colorize = FALSE, add = add)
}
abline(a = 0, b = 1, lty = 2, lwd = 3, col = "red")

# Perform a multi-class ROC analysis. 
data_long <- data_long %>% 
  dplyr::mutate(
  human = base::factor(human, levels = c('Not codable', 
                                         'Negative',  
                                         'Neutral', 
                                         'Positive'), 
                       ordered = TRUE), 
  baby_facereader = base::factor(baby_facereader, levels = c('Not codable',
                                                             'Negative',
                                                             'Neutral',
                                                             'Positive'),
                           ordered = TRUE))  

# Derive the Area Under the Curve (AUC). 
output <- pROC::multiclass.roc(data_long$human, data_long$baby_facereader)
print(output$auc)
