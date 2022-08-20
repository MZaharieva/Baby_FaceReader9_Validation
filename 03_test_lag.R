#---
#**Aim:** 
# "Determine the lag order that yields the best separability between automatic 
# *valence distributions across manually coded facial expression categories."
#---

# Libraries ----
library(dplyr) # data wrangling & manipulation
library(tibble) # data tables
library(ggplot2) # plotting
library(magrittr) # assignment pipe operator

# Read in manually and automatically coded valence data ----

# A function checking whether the automatically analyzed data with or without
# smoothing are being used, based on which to input and output paths are specified. 
if (!exists("analysis_type")){
  analysis_type <- "smooth"
} else if (!analysis_type %in% c("smooth", "raw")) {
  stop("Illegal value for analysis type. Use smooth or raw.")
}

# Based on the dataset, specify input and output paths. 
# Path to individual data files analyzed with(out) temporal smoothing.
bfr_data_path <- paste0('output/', analysis_type,'/BFR_OBXT_data.rds')

# Selects the required columns for the lag analysis and removes duplicates.
df <- readRDS(bfr_data_path) %>%
  dplyr::select(participant_id, analysis_id, valence, FE_category, BFR) %>%
  dplyr::distinct()

# Gather all analysis_IDs without any data for Valence in a single data frame. 
completely_missing <- df %>%
  dplyr::group_by(participant_id, analysis_id) %>%
  # Compute n available data & ungroup for the next step. 
  dplyr::summarise(not_missing = sum(!is.na(valence)), .groups = "drop") %>%
  # Select the rows with missing data. 
  dplyr::filter(not_missing == 0) %>%
  dplyr::ungroup()
# If any participants have no valid data for valence, remove them from dataset. 
# Remove participant x analysis_ids  without any data for valence.
df <- df %>%
  dplyr::anti_join(completely_missing, by = c('participant_id', 'analysis_id'))

df <- df %>%
  # Arrange to assure lag works
  dplyr::arrange(participant_id, analysis_id, BFR) %>%
  # Lag per group
  dplyr::group_by(participant_id, analysis_id) 

# Determine best lag ----

# Find the minimum lag correction that yields the greatest degree of separability
# of automatic valence means across the manually coded facial expression categories. 
# Per hypothesis, positive automatic valence values should map onto positive 
# manually coded facial expressions, and negative automatic valence values - onto
# negative manually coded facial expressions. Manually coded neutral state 
# should correspond to automatic valence values centered around the 0. 

# Define the range of lags (i.e., number of rows) that we want to assess: 0 to 75 rows or 5 seconds.  
lags_to_test <- -75:75

# Function to calculate the correspondence between the two data streams at different lags. 
# Per facial expression category, summary statistics (min, max, mean, median, sd) 
# are calculated for automatic valence at each time lag specified in lag_to_test. 
# Then we use visual inspection to assess which lag yields the highest automatic valence
# in the positive FE_category and the lowest in the negative FE category.
dfs <- list()
for (correction_val in lags_to_test){
  
  iteration_df <- df %>%
    { if (correction_val < 0) {
        dplyr::mutate(., FE_category = dplyr::lead(FE_category, n = abs(correction_val))) %>%
              dplyr::filter(., dplyr::row_number() <= (nrow(.) + correction_val))
      } else { 
        dplyr::mutate(., FE_category = dplyr::lag(FE_category, n = correction_val)) %>%
          dplyr::filter(., dplyr::row_number() > correction_val)}
    } %>%
    dplyr::ungroup() 
  
  dfs[[paste(correction_val)]] <- iteration_df %>%
    dplyr::group_by(FE_category) %>%
    dplyr::summarise(
      valid_n = sum(!is.na(valence)),
      min = min(valence, na.rm = TRUE), 
      max = max(valence, na.rm = TRUE),
      mean = mean(valence, na.rm = TRUE),
      median = median(valence, na.rm = TRUE),
      sd = sd(valence, na.rm = TRUE),
      .groups = "drop") %>%
    tidyr::pivot_wider(names_from = FE_category, values_from = c(valid_n, min, max, mean, sd, median)) %>%
    dplyr::mutate(correction = correction_val) %>%
    dplyr::select(correction, dplyr::everything())
}

# The loop creates a bunch of dfs in a list, which are combined into a single 
# dataframe by pasting them together.
df <- dplyr::bind_rows(dfs)

# Plots based on the difference scores between manually coded positive and negative
# facial expressions. 
df <- df %>%
  # Filter only the positive, neutral, and negative FE categories.
  dplyr::select(correction, dplyr::ends_with(c("positive", "negative", "neutral"))) %>%
  dplyr::mutate(diff_mean = mean_positive - mean_negative,
         diff_median = median_positive - median_negative,
         # Convert correction to seconds
         correction = correction * 0.0667) %>%
  dplyr::select(correction, diff_mean, diff_median, dplyr::everything()) 
# Plot the fluctuations in automatic Valence at different lag corrections.
plot(df$correction, df$mean_positive)
plot(df$correction, df$mean_neutral)
plot(df$correction, df$mean_negative)
plot(df$correction, df$diff_mean)

# Create a dataframe containing the mean and SD of automatic valence for each 
# correction-category combination.
df <- tidyr::pivot_longer(
    dplyr::select(df, correction, dplyr::starts_with('mean')),
    cols = starts_with('mean'), 
    names_prefix = "mean_",  
    names_to = "category", 
    values_to = "mean_valence") %>%
  # Join the SD values on to this.
  dplyr::left_join(
    tidyr::pivot_longer(dplyr::select(df, correction, dplyr::starts_with('sd')),
                 cols = dplyr::starts_with('sd'), 
                 names_prefix = "sd_",  
                 names_to = "category", 
                 values_to = "sd_valence"),
      by = c("correction", "category"))

# Use this plot to compare the mean automatic valence for each manually coded 
# facial expression category at each lag/lead correction.
view(df, title = "Mean automatic valence per manually coded facial expression category at each lag/lead correction")

# Figure S1 in Supplementary materials. 
df %>%
  dplyr::mutate(lowerbound = mean_valence - sd_valence,
                upperbound = mean_valence + sd_valence) %>%
  dplyr::rename(c('Correction (seconds)' = correction, 
                  'Mean Automatic Valence' = mean_valence, 
                  'Manually Coded Facial\nExpression Category' = category)) %>%
  ggplot2::ggplot(ggplot2::aes(`Correction (seconds)`, `Mean Automatic Valence`,
                               color = `Manually Coded Facial\nExpression Category`)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lowerbound, ymax = upperbound,
                                    fill = `Manually Coded Facial\nExpression Category`), alpha = 0.3) +
  ggplot2::geom_line() %>%
  ggplot2::geom_vline(xintercept = 0)


