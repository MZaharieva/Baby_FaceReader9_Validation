#---
#**Aim:** 
#*"Plot and compute descriptives to explore the relation between automatically 
#*detected action units (AUs) and manually coded facial expression caegories."
#---

# Libraries ----
library(dplyr) # data wrangling and manipulation
library(readxl) # read in excel data 
library(ggplot2) # plotting
library(hrbrthemes) # plotting
library(GGally) # plotting 
library(viridis) # plotting 
library(magrittr) # assignment pipe operator

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

# Read in the dyadic dataset of infants at 4 and 8 months analyzed with Baby FaceReader 9.
data <- readr::read_rds(bfr_data_path) %>%
  # All errors are filtered out in a previous step, and is useless.
  dplyr::select(-error)

# Make one primary data frame, from which data are subset for each plot. 
# Filter the relevant data, excluding events that were manually coded as "Not Visible".
data_mlr <- data %>% 
  dplyr::filter(FE_category != "Not Visible") %>%
  dplyr::mutate(manual_valence = base::factor(FE_category, 
                                              levels = c('Negative', 'Neutral', 'Positive'), 
                                              labels = c('negative', 'neutral', 'positive'),
                                              ordered = TRUE), 
         manual_valence_pos_rest = dplyr::recode(FE_category, 
                                                 `Negative` = 'rest', 
                                                 `Neutral` = 'rest', 
                                                 `Positive` = 'positive')) %>%
  dplyr::rename(automatic_valence = valence)

# Check whether FE_category is successfully recoded into manual_valence.
dplyr::count(data_mlr, manual_valence)
dplyr::count(data_mlr, FE_category)
dplyr::count(data_mlr, manual_valence_pos_rest)

# Sum AU6 & 7 
data_mlr %<>%
  dplyr::mutate(AU6_AU7_sum = (AU6 + AU7))

# Check whether there is any missing data.
data %>% dplyr::summarise(dplyr::across(dplyr::everything(), 
                                        ~ base::sum(is.na(.x))))

# Write a csv file. 
readr::write_csv2(data_mlr, 
                  file = paste0("output/", analysis_type, "/brf9_mlr_data.csv"), 
                  na = "NA")

# AU Definitions ----
##Action units for positive facial expressions: AU6, AU12
# AU6 = `Action Unit 06 - Cheek Raiser`, 
# AU12 = `Action Unit 12 - Lip Corner Puller`

## Action units for negative facial expressions: AU3+4, AU7, AU17, AU20, 
# AU3+4 = `Action Unit 03 + 04 - Brow Knitting and Knotting`,
# AU7 = `Action Unit 07 - Lid Tightener`,
# AU17 = `Action Unit 17 - Chin Raiser`,
# AU20 = `Action Unit 20 - Lip Stretcher`,

## Shared Positive & Negative Action Units: AU25, AU26, AU27
# AU25 = `Action Unit 25 - Lips Part`, 
# AU26 = `Action Unit 26 - Jaw Drop`,
# AU27 = `Action Unit 27 - Mouth Stretch`,

## Other AU's eye area:
# AU1 = `Action Unit 01 - Inner Brow Raiser`,
# AU2 = `Action Unit 02 - Outer Brow Raiser`,
# AU5 = `Action Unit 05 - Upper Lid Raiser`,
# AU43 = `Action Unit 43 - Eyes Closed`,
# AU9 = `Action Unit 09 - Nose Wrinkler`,

## Other AU's mouth area
# AU10 = `Action Unit 10 - Upper Lip Raiser`,
# AU14 = `Action Unit 14 - Dimpler`,
# AU15 = `Action Unit 15 - Lip Corner Depressor`,
# AU18 = `Action Unit 18 - Lip Pucker`,
# AU23 = `Action Unit 23 - Lip Tightener`,
# AU24 = `Action Unit 24 - Lip Pressor`

# Prepare data & define tick marks ----

## Aggregate plot data & define tick marks per age ----
plot_age <- data_mlr %>% 
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "4 Months",
                                           `8` = "8 Months")) %>%
  dplyr::select(participant_id, age, manual_valence, starts_with("AU")) %>%
  dplyr::group_by(age, manual_valence) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("AU"), mean)) %>%
  dplyr::ungroup()

## Aggregate plot data per interaction partner ----
plot_int_partner <- data_mlr %>% 
  dplyr::mutate(int_partner = dplyr::recode_factor(int_partner, 
                                                   `2` = "Mother",
                                                   `1` = "Father",
                                                   `3` = "Unfamiliar Adult")) %>%
  dplyr::select(participant_id, age, int_partner, manual_valence, dplyr::starts_with("AU")) %>%
  dplyr::group_by(age, int_partner, manual_valence) %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("AU"), mean)) %>%
  dplyr::ungroup()

## Define plot tick marks -----

# Apriori AU's tick marks
x_tick <- base::match(c("AU12", "AU6", "AU25",
                        "AU26", "AU27", "AU17",
                        "AU20", "AU3_4", "AU7"), base::names(plot_age))
# Remove missing values
x_tick <- stats::na.omit(x_tick)

# Other AU's tick marks
x_tick_rest <- base::match(c("AU1", "AU2", "AU5",
                             "AU43", "AU9", "AU10",
                             "AU15", "AU18", "AU23",
                             "AU24", "AU14"), base::names(plot_age))
# Remove missing values
x_tick_rest <- stats::na.omit(x_tick_rest)

# Plot Apriori AU's * Age ----

# Prepare plot data.
plot_AU_df <- plot_age

# Plot.
plot_AU_df <- plot_AU_df %>% 
  dplyr::mutate(manual_valence = dplyr::recode_factor(manual_valence, 
                                                      `negative` = "Negative",
                                                      `neutral` = "Neutral",
                                                      `positive` = "Positive")) %>%
  GGally::ggparcoord(.,
                     columns = x_tick, 
                     groupColumn = "manual_valence", 
                     showPoints = FALSE, 
                     title = "Action Unit Activation Intensity per Facial Expression Category",
                     alphaLines = 1,
                     scale = "globalminmax") +
  ggplot2::labs(y = element_text("Mean Activation Intensity"), 
                x = element_text("Automatically Detected Action Units"), 
                color = 'Manually Coded \nFacial Expression',  
                shape = 'Age') +
  viridis::scale_color_viridis(discrete = TRUE) +
  hrbrthemes::theme_ipsum()+
  ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")), 
                 axis.title.y = element_text(size = 14, angle = 90, hjust = 0.5), 
                 axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5), 
                 axis.text.y = element_text(size = 14, vjust = 0.25), 
                 plot.margin = unit(c(1, 1, 1, 1), "cm"),
                 legend.text = element_text(size = 14), 
                 legend.title = element_text(size = 14)) + 
  ggplot2::ylim(0, 1) +
  ggplot2::scale_x_discrete(
    labels = c("AU12 \nLip Corner Puller", "AU6 \nCheek Raiser", "AU25 \nLips Part", 
               "AU26 \nJaw Drop", "AU27 \nMouth Stretch", "AU17 \nChin Raiser", 
               "AU20 \nLip Stretcher", "AU3 & AU4 \nBrow Knitting & Knotting", 
               "AU7 \nLid Tightener")) 

# Make sure that the mapping of interaction partner is correct (and not jumbled by ggparcoord)
plot_AU_df$data <- plot_age %>%
  dplyr::select(age) %>%
  dplyr::mutate(`.ID` = base::factor(dplyr::row_number())) %>%
  dplyr::left_join(dplyr::select(plot_AU_df$data, -age), ., by = '.ID')

# Final plot.
AU_age <- plot_AU_df + 
  ggplot2::geom_point(aes(shape = age), size = 4) + 
  ggplot2::scale_shape_manual(name = "Age", values = c(17, 15))

# Save plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/AU_Age.jpg"),
       plot = AU_age, width = 46, height = 20, units = "cm", dpi = 600)

# Plot Apriori AU's * Age * Interaction Partner ----

## 4 Months ---- 

# Subset the data from the 4-month wave.
plot_AU_4_df <- plot_int_partner %>% 
  dplyr::filter(age == 4) %>%
  dplyr::select(-int_partner)

# Plot apriori AU's per age and interaction partner
plot_AU_4 <- plot_AU_4_df %>% 
  dplyr::mutate(manual_valence = dplyr::recode_factor(manual_valence, 
                                                      `negative` = "Negative",
                                                      `neutral` = "Neutral",
                                                      `positive` = "Positive")) %>%
  GGally::ggparcoord(.,
             columns = x_tick, 
             groupColumn = "manual_valence", 
             showPoints = FALSE, 
             title = "Action Units Activation per Facial Expression Category at 4 Months",
             alphaLines = 1,
             scale = "globalminmax") +
  ggplot2::geom_point(aes(shape = factor(int_partner)), size = 4) +
  ggplot2::scale_shape_manual(name = "Interaction Partner",
                              labels = c("Mother", "Father", "Unfamiliar Adult"),
                              values = c(17, 9, 15)) +
  ggplot2::labs(y = element_text("Mean Activation Intensity"), 
       x = element_text("Automatically Detected Action Units"), 
       color = 'Manually Coded \nFacial Expression',  
       shape = 'Interaction Partner') +
  viridis::scale_color_viridis(discrete = TRUE) +
  #scale_shape_manual() +
  hrbrthemes::theme_ipsum()+
  ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")), 
        axis.title.y = element_text(size = 14, angle = 90, hjust = 0.5), 
        axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5), 
        axis.text.y = element_text(size = 14, vjust = 0.25), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  ggplot2::ylim(0, 1) +
  ggplot2::scale_x_discrete(
    labels = c("AU12 \nLip Corner Puller", "AU6 \nCheek Raiser", "AU25 \nLips Part", 
               "AU26 \nJaw Drop", "AU27 \nMouth Stretch", "AU17 \nChin Raiser", 
               "AU20 \nLip Stretcher", "AU3 & AU4 \nBrow Knitting & Knotting", 
               "AU7 \nLid Tightener")) 

# Make sure that the mapping of interaction partner is correct (and not jumbled by ggparcoord)
plot_AU_4$data <- plot_int_partner %>%
  dplyr::filter(age == 4) %>%
  dplyr::select(int_partner) %>%
  dplyr::mutate(`.ID` = base::factor(row_number())) %>%
  dplyr::left_join(plot_AU_4$data, ., by = '.ID')

# Final plot
AU_apriori_4 <- plot_AU_4 + 
  ggplot2::geom_point(aes(shape = int_partner), size = 4) + 
  ggplot2::scale_shape_manual(name = "Interaction Partner", values = c(17, 9, 15))

# Save plot
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/AU_4MO_int_partner.jpg"),
       plot = AU_apriori_4, width = 46, height = 20, units = "cm", dpi = 600)

## 8 Months ----

# Subset the data from the 8-month wave.
plot_AU_8_df <- plot_int_partner %>% 
  dplyr::filter(age == 8) %>%
  dplyr::select(-int_partner)

# Plot
plot_AU_8 <- plot_AU_8_df %>% 
  dplyr::mutate(manual_valence = dplyr::recode_factor(manual_valence,
                                                      `negative` = "Negative",
                                                      `neutral` = "Neutral",
                                                      `positive` = "Positive")) %>%
  GGally::ggparcoord(.,
             columns = x_tick, 
             groupColumn = "manual_valence", 
             showPoints = FALSE, 
             title = "Action Units Activation per Facial Expression Category at 8 Months",
             alphaLines = 1,
             scale = "globalminmax") +
  ggplot2::geom_point(aes(shape = factor(int_partner)), size = 4) +
  ggplot2::scale_shape_manual(name = "Interaction Partner", 
                              labels = c("Mother", "Father", "Unfamiliar Adult"), 
                              values = c(17, 9, 15)) +
  ggplot2::labs(y = element_text("Mean Activation Intensity"), 
       x = element_text("Automatically Detected Action Units"), 
       color = 'Manually Coded \nFacial Expression',  
       shape = 'Interaction Partner') +
  viridis::scale_color_viridis(discrete = TRUE) +
  hrbrthemes::theme_ipsum() +
  ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")), 
        axis.title.y = element_text(size = 14, angle = 90, hjust = 0.5), 
        axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5), 
        axis.text.y = element_text(size = 14, vjust = 0.25), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) + 
  ggplot2::ylim(0, 1) +
  ggplot2::scale_x_discrete(
    labels = c("AU12 \nLip Corner Puller", "AU6 \nCheek Raiser", "AU25 \nLips Part", 
               "AU26 \nJaw Drop", "AU27 \nMouth Stretch", "AU17 \nChin Raiser", 
               "AU20 \nLip Stretcher", "AU3 & AU4 \nBrow Knitting & Knotting", 
               "AU7 \nLid Tightener")) 

# Make sure that the mapping of interaction partner is correct (and not jumbled by ggparcoord)
plot_AU_8$data <- plot_int_partner %>%
  dplyr::filter(age == 8) %>%
  dplyr::select(int_partner) %>%
  dplyr::mutate(`.ID` = base::factor(dplyr::row_number())) %>%
  dplyr::left_join(plot_AU_8$data, ., by = '.ID')

# Final plot
AU_apriori_8 <- plot_AU_8 + geom_point(aes(shape = int_partner), size = 4) + 
  scale_shape_manual(name = "Interaction Partner", values = c(17, 9, 15))

# Save plot
ggsave(file = paste0("rplots/", analysis_type, "/AU_8MO_int_partner.jpg"),
       plot = AU_apriori_8, width = 46, height = 20, units = "cm", dpi = 600)

# Plot Other AU's * Age ----

# Prepare plot data.
plot_other_AU_df <- plot_age

# Plot. 
plot_other_AU_df <- plot_other_AU_df %>% 
  dplyr::mutate(manual_valence = dplyr::recode_factor(manual_valence, 
                                                      `negative` = "Negative",
                                                      `neutral` = "Neutral", 
                                                      `positive` = "Positive")) %>%
  GGally::ggparcoord(.,
                     columns = x_tick_rest, 
                     groupColumn = "manual_valence", 
                     showPoints = FALSE, 
                     title = "Action Units Activation per Facial Expression Category",
                     alphaLines = 1,
                     scale = "globalminmax") +
  labs(y = element_text("Mean Activation Intensity"), 
       x = element_text("Automatically Detected Action Units"), 
       color = 'Manually Coded \nFacial Expression',  
       shape = 'Age') +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum()+
  theme(plot.title = element_text(size = 14, hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")), 
        axis.title.y = element_text(size = 14, angle = 90, hjust = 0.5), 
        axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5), 
        axis.text.y = element_text(size = 14, vjust = 0.25), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  ylim(0, 1) +
  scale_x_discrete(
    labels = c("AU1 \nInner Brow \nRaiser", "AU2 \nOuter Brow \nRaiser", "AU5 \nUpper Lid \nRaiser", 
               "AU43 \nEyes Closed", "AU9 \nNose Wrinkler", "AU10 \nUpper Lip \nRaiser", 
               "AU15 \nLip Corner \nDepressor", "AU18 \nLip Pucker", "AU23 \nLip Tightener", 
               "AU24 \nLip Pressor", "AU14 \nDimpler")) 

# Make sure that the mapping of interaction partner is correct (and not jumbled by ggparcoord).
plot_other_AU_df$data <- plot_age %>%
  dplyr::select(age) %>%
  dplyr::mutate(`.ID` = factor(dplyr::row_number())) %>%
  dplyr::left_join(dplyr::select(plot_other_AU_df$data, -age), ., by = '.ID')

# Final plot. 
AU_other_age <- plot_other_AU_df + 
  ggplot2::geom_point(aes(shape = age), size = 4) + 
  ggplot2::scale_shape_manual(name = "Age", values = c(17, 15))

# Save the last plot
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/Other_AU_Age.jpg"),
                plot = AU_other_age, width = 46, height = 20, units = "cm", dpi = 600)

# Plot Other AU's * Age * Interaction Partner ----

## 4 Months ----
# Subset the data from the 4-month wave.
plot_other_AU_4_df <- plot_int_partner %>% 
  dplyr::filter(age == 4) %>%
  dplyr::select(-int_partner)

# Plot
plot_other_AU_4 <- plot_other_AU_4_df %>% 
  dplyr::mutate(manual_valence = dplyr::recode_factor(manual_valence, 
                                                      `negative` = "Negative",
                                                      `neutral` = "Neutral",
                                                      `positive` = "Positive")) %>%
  GGally::ggparcoord(.,
             columns = x_tick_rest, 
             groupColumn = "manual_valence", 
             showPoints = FALSE, 
             title = "Action Units Activation per Facial Expression Category at 4 Months",
             alphaLines = 1,
             scale = "globalminmax") +
  ggplot2::labs(y = element_text("Mean Activation Intensity"), 
       x = element_text("Automatically Detected Action Units"), 
       color = 'Manually Coded \nFacial Expression', 
       shape = 'Interaction Partner') +
  viridis::scale_color_viridis(discrete = TRUE) +
  hrbrthemes::theme_ipsum()+
  ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")), 
        axis.title.y = element_text(size = 14, angle = 90, hjust = 0.5), 
        axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5), 
        axis.text.y = element_text(size = 14, vjust = 0.25), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  ggplot2::ylim(0, 1) +
  ggplot2::scale_x_discrete(
    labels = c("AU1 \nInner Brow \nRaiser", "AU2 \nOuter Brow \nRaiser", "AU5 \nUpper Lid \nRaiser", 
               "AU43 \nEyes Closed", "AU9 \nNose Wrinkler", "AU10 \nUpper Lip \nRaiser", 
               "AU15 \nLip Corner \nDepressor", "AU18 \nLip Pucker", "AU23 \nLip Tightener", 
               "AU24 \nLip Pressor", "AU14 \nDimpler"))

# Make sure that the mapping of interaction partner is correct (and not jumbled by ggparcoord).
plot_other_AU_4$data <- plot_int_partner %>%
  dplyr::filter(age == 4) %>%
  dplyr::select(int_partner) %>%
  dplyr::mutate(`.ID` = base::factor(dplyr::row_number())) %>%
  dplyr::left_join(plot_other_AU_4$data, ., by = '.ID')

# Final plot.
AU_other_4 <- plot_other_AU_4 + 
  ggplot2::geom_point(aes(shape = int_partner), size = 4) + 
  ggplot2::scale_shape_manual(name = "Interaction Partner", values = c(17, 9, 15))

# Save the last plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/Other_AU_4MO.jpg"),
       plot = AU_other_4, width = 46, height = 20, units = "cm", dpi = 600)

## 8 Months ----

# Subset the data from the 8-month wave. 
plot_other_AU_8_df <- plot_int_partner %>% 
  dplyr::filter(age == 8) %>%
  dplyr::select(-int_partner)

# Plot.
plot_other_AU_8 <- plot_other_AU_8_df %>% 
  dplyr::mutate(manual_valence = dplyr::recode_factor(manual_valence, 
                                                      `negative` = "Negative",
                                                      `neutral` = "Neutral",
                                                      `positive` = "Positive")) %>%
  GGally::ggparcoord(.,
             columns = x_tick_rest, 
             groupColumn = "manual_valence", 
             showPoints = FALSE, 
             title = "Action Units Activation per Facial Expression Category at 8 Months",
             alphaLines = 1,
             scale = "globalminmax") +
  ggplot2::labs(y = element_text("Mean Activation Intensity"), 
       x = element_text("Automatically Detected Action Units"), 
       color = 'Manually Coded \nFacial Expression',  
       shape = 'Interaction Partner') +
  viridis::scale_color_viridis(discrete = TRUE) +
  #scale_shape_manual() +
  hrbrthemes::theme_ipsum()+
  ggplot2::theme(plot.title = element_text(size = 14, hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")), 
        axis.title.y = element_text(size = 14, angle = 90, hjust = 0.5), 
        axis.title.x = element_text(size = 14, angle = 0, hjust = 0.5), 
        axis.text.y = element_text(size = 14, vjust = 0.25), 
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 14)) + 
  ggplot2::ylim(0, 1) +
  ggplot2::scale_x_discrete(
    labels = c("AU1 \nInner Brow \nRaiser", "AU2 \nOuter Brow \nRaiser", "AU5 \nUpper Lid \nRaiser", 
               "AU43 \nEyes Closed", "AU9 \nNose Wrinkler", "AU10 \nUpper Lip \nRaiser", 
               "AU15 \nLip Corner \nDepressor", "AU18 \nLip Pucker", "AU23 \nLip Tightener", 
               "AU24 \nLip Pressor", "AU14 \nDimpler"))

# Make sure that the mapping of interaction partner is correct (and not jumbled by ggparcoord).
plot_other_AU_8$data <- plot_int_partner %>%
  dplyr::filter(age == 8) %>%
  dplyr::select(int_partner) %>%
  dplyr::mutate(`.ID` = base::factor(dplyr::row_number())) %>%
  dplyr::left_join(plot_other_AU_8$data, ., by = '.ID')

# Final plot.
AU_other_8 <- plot_other_AU_8 + 
  ggplot2::geom_point(aes(shape = int_partner), size = 4) + 
  ggplot2::scale_shape_manual(name = "Interaction Partner", values = c(17, 9, 15))

# Save the last plot.
ggplot2::ggsave(file = paste0("rplots/", analysis_type, "/Other_AU_8MO.jpg"),
       plot = AU_other_8, width = 46, height = 20, units = "cm", dpi = 600)

# Individual-level plots ----

# Convert all AU variables to numeric
BFR_OBXT_valid_data <- data_mlr %>%
  dplyr::mutate(AU6 = as.numeric(AU6), AU12 = as.numeric(AU12), 
         AU1 = as.numeric(AU1), AU3_4 = as.numeric(AU3_4), 
         AU7 = as.numeric(AU7), AU20 = as.numeric(AU20), 
         AU25 = as.numeric(AU25), AU43 = as.numeric(AU43))

## Positive Apriori AU's ----

# Infant-level AU12 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU 12` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU12, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU12, fill = `AU 12`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU12 (Lip Corner Puller)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU12 Distribution per Age and Interaction Partner', 
                x = 'AU12',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Infant-level AU6 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU 6` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU6, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU6, fill = `AU 6`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU6 (Cheeck Raiser)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU6 Distribution per Age and Interaction Partner', 
                x = 'AU6',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

## Shared Positive & Negative Apriori AU's ----

# Infant-level AU25 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU 25` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU25, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU25, fill = `AU 25`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU25 (Lips Part)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU25 Distribution per Age and Interaction Partner', 
                x = 'AU25',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Infant-level AU26 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU 26` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU26, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU26, fill = `AU 26`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU26 (Jaw Drop)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU26 Distribution per Age and Interaction Partner', 
                x = 'AU26',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Infant-level AU27 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU 27` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU27, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU27, fill = `AU 27`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU27 (Mouth Stretch)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU27 Distribution per Age and Interaction Partner', 
                x = 'AU27',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

## Negative Apriori AU's ----

# Infant-level AU3+4 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU3+4` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU3_4, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU3_4, fill = `AU3+4`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU3+4 (Brow Knitting \n& \nKnotting)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU3+4 Distribution per Age and Interaction Partner', 
                x = 'AU3+4',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Infant-level AU7 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU 7` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU7, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU7, fill = `AU 7`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU7 (Lid Tightener)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU7 Distribution per Age and Interaction Partner', 
                x = 'AU7',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white")) 

# Infant-level AU17 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU 17` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU17, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU17, fill = `AU 17`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU17 (Chin Raiser)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU17 Distribution per Age and Interaction Partner', 
                x = 'AU17',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white"))


# Infant-level AU20 distributions per manually coded facial expression category and age
BFR_OBXT_valid_data %>%
  dplyr::mutate(age = dplyr::recode_factor(age, 
                                           `4` = "at 4 Months",
                                           `8` = "at 8 Months"),
                `AU 20` = factor(paste(FE_category, age), 
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
  ggplot2::ggplot(., aes(x = AU20, y = as.character(participant_id), fill = stat(x))) + 
  ggridges::geom_density_ridges_gradient(aes(x = AU20, fill = `AU 20`),
                                         scale = 0.8, 
                                         quantile_lines = TRUE, 
                                         quantiles = 2) +
  ggplot2::facet_wrap(~FE_category,
                      scales = "free_x", 
                      strip.position = "bottom") + 
  ggridges::theme_ridges() +
  ggplot2::scale_fill_viridis_d(name = "AU20 (Lip Stretcher)", 
                                option = "C", alpha = 0.7, 
                                direction = -1) +
  ggplot2::labs(title = 'AU20 Distribution per Age and Interaction Partner', 
                x = 'AU20',  y = 'Manually Coded Facial Expression Category') + 
  ggplot2::xlim(0, 1) +
  ggplot2::theme(panel.spacing = unit(0.5, "lines"), 
                 strip.text.x = element_text(size = 14),
                 plot.title = element_text(hjust = 0.5, margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
                 axis.title.y = element_text(angle = 90, hjust = 0.5 ), 
                 axis.title.x = element_text(angle = 0, hjust = 0.5, vjust = 1), 
                 axis.text.y = element_text(vjust = 0.25),
                 plot.margin = unit(c(1, 1, 1, 1), "cm"), 
                 strip.background = element_rect(colour = "black",
                                                 fill = "white"))


