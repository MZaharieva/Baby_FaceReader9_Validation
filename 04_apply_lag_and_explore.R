#---
#**Aim:** 
# "1) Apply the desired lag when syncing the automatic and manual data streams. 
# *2) Perform a final clean-up of the synchronized dataset. 3) Plot and generate 
# *descriptive statistics of automatically detected global emotional valence,  
# *the proportion of missing data, and face model fit quality." 
#---

# Libraries ----
library(dplyr) # data wrangling and manipulation
library(lubridate) # time stampls
library(ggplot2) # data visualization 
library(ggridges) # data visualization 
library(plotly) # interactive plots
library(tibble) # data tables 
library(readr) # reading data files
library(magrittr) # assignment pipe operator

# Read data ----

# A function checking whether the automatically analyzed data with or without
# smoothing are being used, based on which to input and output paths are specified. 
if (!exists("analysis_type")){
  analysis_type <- "smooth"
} else if (!analysis_type %in% c("smooth", "raw")) {
  stop("Illegal value for analysis type. Use smooth or raw.")
}

# Based on the dataset ("smooth" or "raw"), specify input and output paths. 
# Path to individual data files analyzed with(out) temporal smoothing.
bfr_data_path <- paste0('output/', analysis_type,'/BFR_OBXT_data.rds')
# Path to datafile with head rotation variables.
#bfr_data_path <- paste0('output/', analysis_type,'/BFR_OBXT_data_head_rotations.rds')

# With smoothing
#BFR_OBXT_valid_data <- readRDS(file = "output/BFR_OBXT_data_smoothing.rds")

BFR_OBXT_valid_data <- readRDS(bfr_data_path)

# Use results from lag_analysis to correct BFR_OBXT_valid_data. 
# Use BFR instead of the time_st_seconds_join.
# MANUALLY select the desired n parameter.
desired_lag <- 0

if (desired_lag != 0){
  BFR_OBXT_valid_data <- BFR_OBXT_valid_data %>%
    # Arrange to assure that the lag works.
    dplyr::arrange(participant_id, analysis_id, BFR) %>%
    # Lag per group.
    dplyr::group_by(participant_id, analysis_id)  %>%
    # lag() or lead() by 1 (or more); for the BFR9 dyadic data: 0
    # Does not work  if lag = 0
    { if (desired_lag < 0) {
      dplyr::mutate(., FE_category = dplyr::lead(FE_category, n = abs(desired_lag))) %>%
        dplyr::filter(., dplyr::row_number() <= (nrow(.) + desired_lag))
    } else { 
      dplyr::mutate(., FE_category = dplyr::lag(FE_category, n = desired_lag)) %>%
        dplyr::filter(., dplyr::row_number() > desired_lag)}
    } %>%
    dplyr::ungroup() 
}


## Organize & recode variables
BFR_OBXT_valid_data <- BFR_OBXT_valid_data %>%
  # Recode FE_category as an ordered factor
  dplyr::mutate(FE_category = ordered(FE_category, 
                               levels = c("not_codable", "negative", "neutral", "positive"), 
                               labels = c("Not Visible", "Negative", "Neutral", "Positive"))) %>%
  #dplyr::mutate(head_rotation = (yaw + pitch)/2) %>%
  dplyr::select(participant_id, int_partner, age, 
                sex, time_st, time_rel_sf, video_quality,
                valence, arousal, FE_category, error, dplyr::starts_with("AU"), 
                #pitch, yaw, roll, head_rotation,
                dplyr::everything()) %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("AU"), as.numeric))

# Save data file
saveRDS(BFR_OBXT_valid_data, file = paste0("output/", analysis_type, "/BFR_OBXT_valid_data.rds"))

# # Save data file with head rotation
# saveRDS(BFR_OBXT_valid_data, file = paste0("output/", analysis_type, "/BFR_OBXT_valid_data_head_rotation.rds"))

# Descriptives ----

## Automatic Valence Descriptives per Manually Coded FE category ----

# Compute the mean, SD and range BFR Valence values for facial expressions
# coded as positive, negative, neutral and Not Visible in OBXT. 
FE_valence_ranges_overall <- BFR_OBXT_valid_data %>%
  #dplyr::group_by(FE_category) %>%
  dplyr::group_by(FE_category, age) %>%
  dplyr::summarise(dplyr::across(c(valence, arousal, video_quality, 
                                   # head rotation variables
                                   #pitch, yaw, roll, head_rotation,
                                   # positive & shared AU's
                                   AU12, AU6, AU25, AU26, AU27,
                                   # negative AU's
                                   AU3_4, AU7, AU17, AU20,
                                   # Other AUs for eye area
                                   AU1, AU2, AU5, AU43, AU9, 
                                   # Other AUs for mouth area
                                   AU10, AU14, AU15, AU18, AU23, AU24), 
                                 list(mean = ~ mean(.x, na.rm = TRUE), 
                                      sd = ~ sd(.x, na.rm = TRUE), 
                                      min = ~ min(.x, na.rm = TRUE), 
                                      max = ~ max(.x, na.rm = TRUE)), 
                                 .names = "{.col}_{.fn}"))

# Descriptives for Table 6: Automatic valence per facial expression category, age and overall
BFR_OBXT_valence_summary <- BFR_OBXT_valid_data %>%
  dplyr::filter(FE_category != "Not visible") %>%
  #dplyr::group_by(., FE_category) %>%
  dplyr::group_by(., age, FE_category) %>%
  dplyr::summarise(across(c(valence), 
                          list(mean = ~ mean(.x, na.rm = TRUE), 
                               sd = ~ sd(.x, na.rm = TRUE), 
                               min = ~ min(.x, na.rm = TRUE), 
                               max = ~ max(.x, na.rm = TRUE)), 
                          .names = "{.col}_{.fn}"))

# Decsriptives for Table S2: Mean and Standard Deviations of the Action Unit Intensities 
# per Manually Coded Facial Expression Category and Age
FE_AU_ranges <- BFR_OBXT_valid_data %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  dplyr::group_by(FE_category, age) %>%
  dplyr::summarise(dplyr::across(c(# positive & shared AU's
                                   AU12, AU6, AU25, AU26, AU27,
                                   # negative AU's
                                   AU3_4, AU7, AU17, AU20), 
                                 list(mean = ~ mean(.x, na.rm = TRUE), 
                                      sd = ~ sd(.x, na.rm = TRUE)), 
                                 .names = "{.col}_{.fn}"))

# Generate descriptives in a wide format 
FE_valence_ranges_descr_age <- FE_valence_ranges_overall %>%
  tidyr::pivot_wider(., 
                     id_cols = c(FE_category),
                     names_from = c(age),
                     values_from = c(valence_mean, valence_sd, video_quality_mean, video_quality_sd, starts_with("AU"))) %>%
  ungroup()

# Save descriptives files. 
readr::write_excel_csv2(FE_valence_ranges_overall, 
                        file = paste0("output/", analysis_type, "/FE_valence_ranges_overall.csv"), 
                        na = "NA")

readr::write_excel_csv2(FE_valence_ranges_descr_age, 
                        file = paste0("output/", analysis_type, "/FE_valence_ranges_descr_age.csv"), 
                        na = "NA")

readr::write_excel_csv2(FE_AU_ranges, 
                        file = paste0("output/", analysis_type, "/FE_AU_ranges.csv"), 
                        na = "NA")

# FE categories per participant.
BFR_OBXT_valid_data %>%
  # group by participant index & FE category 
  dplyr::group_by(participant_id, FE_category) %>%
  dplyr::summarise(
    dplyr::across(c(valence, arousal, video_quality), 
                  list(mean = mean, sd = sd, min = min, max = max), 
                  .names = "{.col}_{.fn}")) %>%
  dplyr::arrange(FE_category, participant_id) %>%
  tibble::view(., title = "FE categories per participant")


## Misclassifications & Extreme Values ----

# Inspect cases for which positive FE that take on negative Valence values
BFR_OBXT_valid_data %>%
  dplyr::filter(FE_category == 'Positive' & valence < 0) %>%
  tibble::view(., title = "Display positive FE that take on negative Valence values")

# Check for extreme values 
BFR_OBXT_valid_data %>%
  dplyr::filter(FE_category %in% c('Positive', 'Neutral', 'Negative')) %>%
  dplyr::group_by(participant_id, analysis_id, FE_category) %>%
  dplyr::summarise(valence_min = min(valence),
                   valence_max = max(valence)) %>%
  tibble::view(., title = "Check for extreme values")

# Plot Automatic Valence Distributions ----

# Overall distribution of automatic valence.
hist(BFR_OBXT_valid_data$valence)

## Manually Coded Facial Expression Category ----

# Automatic valence distribution per manually coded facial expression category.
BFR_OBXT_valid_data %>% 
  dplyr::filter(FE_category != "Not Visible") %>% 
  ggplot2::ggplot(., aes(x = valence, y = FE_category, fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(scale = 0.95, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  theme_ridges() + 
  scale_fill_viridis_c(name = "Automatic Valence", option = "D") +
  labs(title = 'Automatic Valence Distribution per Manually Coded Facial Expression Category', 
       x = 'Automatic Valence',  y = 'Manually Coded Facial Expression Category') 

## Age ----

# Figure 4: Automatic Valence Distributions per Manually Coded Facial 
# Expression Category at 4 and at 8 Months.
automatic_valence_age <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months", 
                                           `8` = "at 8 Months"),
                'Automatic Valence' = factor(paste(FE_category, age), 
                                             levels = c("Positive at 4 Months",
                                                        "Positive at 8 Months",
                                                        "Neutral at 4 Months",
                                                        "Neutral at 8 Months",
                                                        "Negative at 4 Months",
                                                        "Negative at 8 Months"))) %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(y = FE_category)) + 
  ggridges::geom_density_ridges(aes(x = valence, fill = `Automatic Valence`),
                                scale = 1.5, 
                                quantile_lines = TRUE, 
                                quantiles = 2, 
                                panel_scaling = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Automatic Valence", 
                       option = "D", alpha = 0.7, 
                       direction = -1) +
  ggplot2::labs(title = 'Automatic Valence Distribution per Manually Coded Facial Expression Category', 
       x = 'Automatic Valence',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-1, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
        strip.text.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
        axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
        axis.text.y = element_text(vjust = 0.25),
        plot.margin = unit(c(1, 1, 1, 1), "cm"), 
        strip.background = element_rect(colour = "black",
                                        fill = "white")) 

# Save plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/automatic_valence_age.jpg"), 
       automatic_valence_age, width = 25, height = 20, units = "cm", dpi = 600)

## Interaction Partner * Age ----

# Figure S2: Automatic Valence Distribution per Age and Interaction partner.
# Plot focusing on differences between the automatic valence distributions 
# across interaction partners. 
automatic_valence_int_partner_age <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "4 Months", 
                                           `8` = "8 Months"),
         int_partner = factor(int_partner, 
                              levels = c(1, 2, 3), 
                              labels = c("with Father", 
                                         "with Mother", 
                                         "with Unfamiliar Adult")),
         'Automatic Valence' = factor(paste(FE_category, int_partner), 
                                      levels = c("Positive with Mother",
                                                 "Positive with Father",
                                                 "Positive with Unfamiliar Adult",
                                                 "Neutral with Mother",
                                                 "Neutral with Father",
                                                 "Neutral with Unfamiliar Adult",
                                                 "Negative with Mother",
                                                 "Negative with Father",
                                                 "Negative with Unfamiliar Adult"))) %>%
  # Use to color code by interaction partner:
  # dplyr::mutate(age = dplyr::recode_factor(age, 
  #                                          `4` = "4 Months", 
  #                                          `8` = "8 Months"),
  #        int_partner = factor(int_partner, levels = c(1, 2, 3), 
  #                             labels = c("with Father", 
  #                                        "with Mother", 
  #                                        "with Unfamiliar Adult")),
  #        'Automatic Valence' = factor(paste(FE_category, int_partner), 
  #                                           levels = c("Positive with Mother",
  #                                                      "Neutral with Mother",
#                                                        "Negative with Mother",
#                                                        "Positive with Father",
#                                                        "Neutral with Father",
#                                                        "Negative with Father",
#                                                        "Positive with Unfamiliar Adult",
#                                                        "Neutral with Unfamiliar Adult",
#                                                        "Negative with Unfamiliar Adult"))) %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(y = FE_category)) + 
  ggridges::geom_density_ridges(aes(x = valence, fill = `Automatic Valence`),
                                scale = 1.5, 
                                quantile_lines = TRUE, 
                                quantiles = 2, 
                                panel_scaling = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Automatic Valence", 
                       option = "D", alpha = 0.5, 
                       direction = -1) +
  ggplot2::labs(title = 'Automatic Valence Distribution per Age and Interaction Partner', 
       x = 'Automatic Valence',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-1, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
        strip.text.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
        axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
        axis.text.y = element_text(vjust = 0.25),
        plot.margin = unit(c(1, 1, 1, 1), "cm"), 
        strip.background = element_rect(colour="black",
                                        fill="white")) + 
  ggplot2::facet_wrap(~age, scales = "free_x", strip.position = "bottom")

# Save plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/automatic_valence_int_partner_age.jpg"), 
       automatic_valence_int_partner_age, width = 25, height = 20, units = "cm", dpi = 600)

# Automatic Valence Distributions per Manually Coded Facial Expression Category, 
# Age & Interaction Partner.
# Plot focusing on the differences between the distributions of automatic valence 
# across longitudinal measurement waves. 
automatic_valence_int_partner <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                'Automatic Valence' = factor(paste(FE_category, age), 
                                             levels = c("Positive at 4 Months",
                                                        "Positive at 8 Months",
                                                        "Neutral at 4 Months",
                                                        "Neutral at 8 Months",
                                                        "Negative at 4 Months",
                                                        "Negative at 8 Months")), 
                int_partner = dplyr::recode_factor(int_partner, 
                                                   `1` = "Father", 
                                                   `2` = "Mother", 
                                                   `3` = "Unfamiliar Adult")) %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(y = FE_category)) + 
  ggridges::geom_density_ridges(aes(x = valence, fill = `Automatic Valence`),
                                scale = 1.5, 
                                quantile_lines = TRUE, 
                                quantiles = 2, 
                                panel_scaling = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Automatic Valence", 
                                option = "D", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Automatic Valence Distribution per Age and Interaction Partner', 
                x = 'Automatic Valence',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-1, 1) +
  #facet_wrap(~age, scales = "free_x", strip.position = "bottom") + 
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) + 
  ggplot2::facet_wrap(~int_partner, scales = "free_x", strip.position = "bottom") 

# Save plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/automatic_valence_int_partner.jpg"), 
                automatic_valence_int_partner, width = 25, height = 20, units = "cm", dpi = 600)

## Infant- and Video-level Plots ----

# Infant-level automatic valence distributions per manually coded facial 
# expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                'Automatic Valence' = factor(paste(FE_category, age), 
                                             levels = c("Positive at 4 Months",
                                                        "Positive at 8 Months",
                                                        "Neutral at 4 Months",
                                                        "Neutral at 8 Months",
                                                        "Negative at 4 Months",
                                                        "Negative at 8 Months")), 
                int_partner = dplyr::recode_factor(int_partner, 
                                                   `1` = "Father", 
                                                   `2` = "Mother", 
                                                   `3` = "Unfamiliar Adult")) %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(x = valence, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = valence, fill = `Automatic Valence`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "Automatic Valence", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Automatic Valence Distribution per Age and Interaction Partner', 
                x = 'Automatic Valence',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-1, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 


# Interactions with Mother: Infant-level automatic valence distributions 
# per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                'Automatic Valence' = factor(paste(FE_category, age), 
                                             levels = c("Positive at 4 Months",
                                                        "Positive at 8 Months",
                                                        "Neutral at 4 Months",
                                                        "Neutral at 8 Months",
                                                        "Negative at 4 Months",
                                                        "Negative at 8 Months")), 
                int_partner = dplyr::recode_factor(int_partner, 
                                                   `1` = "Father", 
                                                   `2` = "Mother", 
                                                   `3` = "Unfamiliar Adult")) %>%
  dplyr::filter(FE_category != "Not Visible" & int_partner == "Mother") %>%
  ggplot2::ggplot(., aes(x = valence, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = valence, fill = `Automatic Valence`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "Automatic Valence", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Automatic Valence Distribution per Age and Interaction Partner', 
                x = 'Automatic Valence',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-1, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Interactions with Father: Infant-level automatic valence distributions per 
# manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                'Automatic Valence' = factor(paste(FE_category, age), 
                                             levels = c("Positive at 4 Months",
                                                        "Positive at 8 Months",
                                                        "Neutral at 4 Months",
                                                        "Neutral at 8 Months",
                                                        "Negative at 4 Months",
                                                        "Negative at 8 Months")), 
                int_partner = dplyr::recode_factor(int_partner, 
                                                   `1` = "Father", 
                                                   `2` = "Mother", 
                                                   `3` = "Unfamiliar Adult")) %>%
  dplyr::filter(FE_category != "Not Visible" & int_partner == "Father") %>%
  ggplot2::ggplot(., aes(x = valence, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = valence, fill = `Automatic Valence`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "Automatic Valence", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Automatic Valence Distribution per Age and Interaction Partner', 
                x = 'Automatic Valence',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-1, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 


# Interactions with Unfamiliar Adult: Infant-level automatic valence distributions
# per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                'Automatic Valence' = factor(paste(FE_category, age), 
                                             levels = c("Positive at 4 Months",
                                                        "Positive at 8 Months",
                                                        "Neutral at 4 Months",
                                                        "Neutral at 8 Months",
                                                        "Negative at 4 Months",
                                                        "Negative at 8 Months")), 
                int_partner = dplyr::recode_factor(int_partner, 
                                                   `1` = "Father", 
                                                   `2` = "Mother", 
                                                   `3` = "Unfamiliar Adult")) %>%
  dplyr::filter(FE_category != "Not Visible" & int_partner == "Unfamiliar Adult") %>%
  ggplot2::ggplot(., aes(x = valence, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = valence, fill = `Automatic Valence`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "Automatic Valence", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Automatic Valence Distribution per Age and Interaction Partner', 
                x = 'Automatic Valence',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-1, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Face Model Fit Quality ----

## Descriptives ----
BFR_OBXT_summary <- BFR_OBXT_valid_data %>%
  dplyr::group_by(., age, int_partner, FE_category) %>%
  filter(FE_category != "Not Visible") %>%
  dplyr::summarise(across(c(valence, video_quality, 
                            # positive & shared AU's
                            AU12, AU6, AU25, AU26, AU27,
                            # negative AU's
                            AU3_4, AU7, AU17, AU20,
                            # Other AUs for eye area
                            AU1, AU2, AU5, AU43, AU9, 
                            # Other AUs for mouth area
                            AU10, AU14, AU15, AU18, AU23, AU24), 
                          list(mean = ~ mean(.x, na.rm = TRUE), 
                               sd = ~ sd(.x, na.rm = TRUE), 
                               min = ~ min(.x, na.rm = TRUE), 
                               max = ~ max(.x, na.rm = TRUE)), 
                          .names = "{.col}_{.fn}"))

# Save descriptives table. 
readr::write_excel_csv2(BFR_OBXT_summary, 
                        file = paste0("output/", analysis_type, "BFR_OBXT_summary.csv"))

# Descriptives for Table 3: video quality per facial expression category, age and overall
BFR_OBXT_vq_summary <- BFR_OBXT_valid_data %>%
  #dplyr::group_by(., age) %>%
  #dplyr::group_by(., FE_category) %>%
  dplyr::group_by(., age, FE_category) %>%
  dplyr::summarise(across(c(video_quality), 
                          list(mean = ~ mean(.x, na.rm = TRUE), 
                               sd = ~ sd(.x, na.rm = TRUE), 
                               min = ~ min(.x, na.rm = TRUE), 
                               max = ~ max(.x, na.rm = TRUE)), 
                          .names = "{.col}_{.fn}"))

# Descriptives for Table S1: video quality per facial expression category, age and overall per interaction partner
BFR_OBXT_vq_summary <- BFR_OBXT_valid_data %>%
  filter(FE_category != "Not Visible") %>%
  dplyr::group_by(., int_partner, age) %>%
  #dplyr::group_by(., int_partner) %>%
  dplyr::group_by(., age, FE_category, int_partner) %>%
  #dplyr::group_by(., age, FE_category) %>%
  dplyr::summarise(across(c(video_quality), 
                          list(mean = ~ mean(.x, na.rm = TRUE), 
                               sd = ~ sd(.x, na.rm = TRUE)), 
                          .names = "{.col}_{.fn}"))

# Save descriptives table. 
readr::write_csv2(BFR_OBXT_vq_summary, 
                        file = paste0("output/", analysis_type, "BFR_OBXT_vq_summary.csv"))


## Plot ----

# Face Model Fit Quality per Facial Expression Category. 
vq_fe_category <- BFR_OBXT_valid_data %>%
  ggplot2::ggplot(., aes(x = video_quality, y = FE_category, fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(scale = 1.75, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2, 
                                         panel_scaling = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_c(name = "Face Model Fit Quality", 
                       option = "D") +
  ggplot2::labs(title = 'Face Model Fit Quality Distribution per Facial Expression Category', 
       x = 'Face Model Fit Quality',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0.4, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
        strip.text.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.title.y = element_text(angle = 90, hjust = 0.5), 
        axis.title.x = element_text(angle = 0, hjust = 0.5), 
        axis.text.y = element_text(vjust = 0.25), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/vq_fe_category.jpg"), 
       vq_fe_category, width = 25, height = 20, units = "cm", dpi = 600)

# Figure 2: Face Model Fit Quality per Age.. 
vq_age <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, `4` = "at 4 Months", `8` = "at 8 Months"),
         'Face Model Fit Quality' = base::factor(base::paste(FE_category, age), levels = c(
                                                                          "Positive at 4 Months", 
                                                                          "Positive at 8 Months", 
                                                                          "Neutral at 4 Months", 
                                                                          "Neutral at 8 Months", 
                                                                          "Negative at 4 Months", 
                                                                          "Negative at 8 Months", 
                                                                          "Not Visible at 4 Months", 
                                                                          "Not Visible at 8 Months"))) %>%
  #dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(y = FE_category)) + 
  ggridges::geom_density_ridges(aes(x = video_quality, fill = `Face Model Fit Quality`),
                                scale = 1.5, 
                                quantile_lines = TRUE, 
                                quantiles = 2, 
                                panel_scaling = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Face Model Fit Quality", 
                       option = "D", alpha = 0.7, direction = -1) +
  ggplot2::labs(title = 'Face Model Fit Quality Distribution per Facial Expression Category', 
       x = 'Face Model Fit Quality',  y = 'Facial Expression Category') + 
  ggplot2::xlim(0.4, 1) +
  #facet_wrap(~age, scales = "free_x", strip.position = "bottom") + 
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
        strip.text.x = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
        axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
        axis.text.y = element_text(vjust = 0.25),
        plot.margin = unit(c(1, 1, 1, 1), "cm"), 
        strip.background = element_rect(colour = "black",
                                        fill = "white")) 

# Save plot. 
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/vq_age.jpg"), 
       vq_age, width = 25, height = 20, units = "cm", dpi = 600)

# Infant-level face model fit quality per manually coded facial expression category and age.
vq_age_ind <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, `4` = "at 4 Months", `8` = "at 8 Months"),
                'Face Model Fit Quality' = base::factor(base::paste(FE_category, age), levels = c(
                  "Positive at 4 Months", 
                  "Positive at 8 Months", 
                  "Neutral at 4 Months", 
                  "Neutral at 8 Months", 
                  "Negative at 4 Months", 
                  "Negative at 8 Months", 
                  "Not Visible at 4 Months", 
                  "Not Visible at 8 Months"))) %>%
  #dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(x = video_quality, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges(aes(x = video_quality, fill = `Face Model Fit Quality`),
                                scale = 1.5, 
                                quantile_lines = TRUE, 
                                quantiles = 2, 
                                panel_scaling = FALSE) +
  ggplot2::facet_wrap(~FE_category,
                      ncol = 4,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Face Model Fit Quality", 
                                option = "C", alpha = 0.7, direction = -1) +
  ggplot2::labs(title = 'Face Model Fit Quality Distribution per Facial Expression Category', 
                x = 'Face Model Fit Quality',  y = 'Facial Expression Category') + 
  ggplot2::xlim(0.4, 1) +
  #facet_wrap(~age, scales = "free_x", strip.position = "bottom") + 
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 
  
# Save plot. 
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/vq_age_ind.jpg"), 
                vq_age_ind, width = 25, height = 20, units = "cm", dpi = 600)


# Head Rotations ----

## Descriptives ----

#Means and sd's for yaw, pitch, roll per facial expression category, age and overall
BFR_OBXT_head_rotation_summary <- BFR_OBXT_valid_data %>%
   dplyr::filter(FE_category != "Not visible") %>%
   #dplyr::group_by(., FE_category) %>%
   dplyr::group_by(., age, FE_category) %>%
   dplyr::summarise(across(c(head_rotation, pitch, yaw, roll), 
                           list(mean = ~ mean(.x, na.rm = TRUE), 
                                sd = ~ sd(.x, na.rm = TRUE), 
                                min = ~ min(.x, na.rm = TRUE), 
                                max = ~ max(.x, na.rm = TRUE),
                                n_hr_samples = ~ n(),
                                n_hr_above_20 = ~ sum(.x > 20), 
                                prop_hr_above_20 = ~ sum(.x > 20)/n()
                                ), 
                           .names = "{.col}_{.fn}"))

readr::write_excel_csv2(BFR_OBXT_head_rotation_summary, 
                         file = paste0("output/", analysis_type, "/head_rotation_summary_age.csv"), 
                         na = "NA")

## Plots ----

# Head Rotations per Manually Coded Facial 
# Expression Category at 4 and at 8 Months.
head_rotation_age <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months", 
                                           `8` = "at 8 Months"),
                'Head Rotation' = factor(paste(FE_category, age), 
                                         levels = c("Positive at 4 Months",
                                                    "Positive at 8 Months",
                                                    "Neutral at 4 Months",
                                                    "Neutral at 8 Months",
                                                    "Negative at 4 Months",
                                                    "Negative at 8 Months"))) %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(y = FE_category)) + 
  ggridges::geom_density_ridges(aes(x = head_rotation, fill = `Head Rotation`),
                                scale = 1.3, 
                                #quantile_lines = TRUE, 
                                #quantiles = 2, 
                                panel_scaling = TRUE, 
                                rel_min_height = .005,
                                stat = "binline", 
                                binwidth = 0.5,
                                draw_baseline = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Head Rotation", 
                                option = "D", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Head Rotation per Manually Coded Facial Expression Category', 
                x = 'Head Rotation',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-2, 35) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Save plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/head_rotation_age.jpg"), 
                head_rotation_age, width = 25, height = 20, units = "cm", dpi = 600)

# Pitch per Manually Coded Facial 
# Expression Category at 4 and at 8 Months.
pitch_age <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months", 
                                           `8` = "at 8 Months"),
                'Pitch' = factor(paste(FE_category, age), 
                                 levels = c("Positive at 4 Months",
                                            "Positive at 8 Months",
                                            "Neutral at 4 Months",
                                            "Neutral at 8 Months",
                                            "Negative at 4 Months",
                                            "Negative at 8 Months"))) %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(y = FE_category)) + 
  ggridges::geom_density_ridges(aes(x = pitch, fill = `Pitch`),
                                scale = 1.3, 
                                #quantile_lines = TRUE, 
                                #quantiles = 2, 
                                panel_scaling = TRUE, 
                                rel_min_height = .005,
                                stat = "binline", 
                                binwidth = 0.5,
                                draw_baseline = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Pitch", 
                                option = "D", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Pitch per Manually Coded Facial Expression Category', 
                x = 'Pitch',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-2, 35) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Save plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/pitch_age.jpg"), 
                pitch_age, width = 25, height = 20, units = "cm", dpi = 600)


# Yaw per Manually Coded Facial 
# Expression Category at 4 and at 8 Months.
yaw_age <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months", 
                                           `8` = "at 8 Months"),
                'Yaw' = factor(paste(FE_category, age), 
                               levels = c("Positive at 4 Months",
                                          "Positive at 8 Months",
                                          "Neutral at 4 Months",
                                          "Neutral at 8 Months",
                                          "Negative at 4 Months",
                                          "Negative at 8 Months"))) %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(y = FE_category)) + 
  ggridges::geom_density_ridges(aes(x = yaw, fill = `Yaw`),
                                scale = 1.3, 
                                #quantile_lines = TRUE, 
                                #quantiles = 2, 
                                panel_scaling = TRUE, 
                                rel_min_height = .005,
                                stat = "binline", 
                                binwidth = 0.5,
                                draw_baseline = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Yaw", 
                                option = "D", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Yaw per Manually Coded Facial Expression Category', 
                x = 'Yaw',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-2, 35) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Save plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/yaw_age.jpg"), 
                yaw_age, width = 25, height = 20, units = "cm", dpi = 600)


# Yaw per Manually Coded Facial 
# Expression Category at 4 and at 8 Months.
roll_age <- BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months", 
                                           `8` = "at 8 Months"),
                'Roll' = factor(paste(FE_category, age), 
                               levels = c("Positive at 4 Months",
                                          "Positive at 8 Months",
                                          "Neutral at 4 Months",
                                          "Neutral at 8 Months",
                                          "Negative at 4 Months",
                                          "Negative at 8 Months"))) %>%
  dplyr::filter(FE_category != "Not Visible") %>%
  ggplot2::ggplot(., aes(y = FE_category)) + 
  ggridges::geom_density_ridges(aes(x = roll, fill = `Roll`),
                                scale = 1.3, 
                                #quantile_lines = TRUE, 
                                #quantiles = 2, 
                                panel_scaling = TRUE, 
                                rel_min_height = .005,
                                stat = "binline", 
                                binwidth = 0.5,
                                draw_baseline = FALSE) +
  ggridges::theme_ridges() + 
  ggplot2::scale_fill_viridis_d(name = "Roll", 
                                option = "D", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'Roll per Manually Coded Facial Expression Category', 
                x = 'Roll',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(-2, 35) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Save plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/roll_age.jpg"), 
                roll_age, width = 25, height = 20, units = "cm", dpi = 600)


