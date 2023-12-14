#---
#**Aim:** 
#*"Compute missing data in the automated measurement (following bug fix)."
#---

# Libraries ----
library(readxl) # reading xlsx files
library(readr) # reading rds files 
library(dplyr) # data wrangling and manipulation
library(lubridate) # date stamps
library(ggplot2) # data visualization 
library(ggridges) # data visualization 
library(plotly) # interactive plots
library(rmarkdown) # data reports
library(magrittr) # assignment pipe operator
library(stringr) # manipulating string variables 

# Check whether the automatically analyzed data with or without
# smoothing are being used, based on which to input and output paths are specified later. 
if (!exists("analysis_type")){
  analysis_type <- "smooth"
} else if (!analysis_type %in% c("smooth", "raw")) {
  stop("Illegal value for analysis type. Use 'smooth' or 'raw'.")
}

# BFR & OBXT data preparation ----

# Read .rds.
OBXT_ts_data <- readRDS('output/OBXT_0408MO_dyadic_15Hz.rds')

# Check count unique participant files
# N = 289
OBXT_ts_data %>% 
  dplyr::group_by(`Age group`) %>%
  distinct(`Observation`) %>%
  dplyr::count(`Age group`)

# Order and rename variables.
OBXT_ts_data <- OBXT_ts_data %>% 
  dplyr::select(time_rel_sf = Time_Relative_sf,
                participant_code = Observation,
                # Behavior...14 belongs to Participant = Child
                FE_category = `Behavior...14`,
                FE_intensity = `Modifier_1...15`,
                participant_id = ID, 
                sex = `Child gender`,
                age = `Age group`,	
                int_partner_join = `Interaction Partner`) %>%
  # Convert to numeric variables to numeric.
  dplyr::mutate(across(.cols = c(-participant_code, -FE_category), .fns = as.numeric))

# Add interaction partner from the file name.
OBXT_ts_data <- OBXT_ts_data %>% dplyr::mutate(int_partner_join = stringr::str_sub(participant_code, 
                                                                                   start = 6, end = 7),
                                               int_partner_join = dplyr::case_when(
                                                 # BFR 1 (father) is assigned to OBXT 0 (father).
                                                 int_partner_join %in% c("F_", "M2") ~ 0,
                                                 # BFR 2 (mother) is assigned to OBXT 1 (mother).
                                                 int_partner_join %in% c("M_", "M1") ~ 1,
                                                 # BFR 3 (stranger) is assigned to OBXT 2 (stranger).
                                                 int_partner_join == "E_" ~ 2,
                                                 TRUE ~ as.numeric(NA)))

## Read and preprocess BFR dyadic data ----

# Based on the dataset ("smooth" or "raw"), specify input and output paths. 
if(analysis_type == "smooth") {
  # to recompute missing BFR data labels following bug fix
  bfr_data_path <- 'data/BFR9_smoothing/Logs_fit_find_failed_bug_fix/'
} else if (analysis_type == "raw") {
  # Path to individual data files analyzed withOUT temporal smoothing.
  bfr_data_path <- 'data/BFR9_raw/Logs/'
} 

# List the available file names.
bfr_data_files <- list.files(bfr_data_path, pattern = '.*detailed.txt$')
# Loop across all file names and append the data into the same data frame.

BFR_ts_data <- purrr::map_dfr(bfr_data_files, 
                              # All fields are treated as character type to accommodate for missing data.
                              ~ readr::read_tsv(paste0(bfr_data_path, .x), skip = 10, 
                                                col_types = paste(rep("c", 79), collapse = "")) %>%
                                dplyr::mutate(src = .x,
                                              #age = as.numeric(stringr::str_match(src, 'Analysis (\\d)\\d')[,2]),
                                )) %>%
                                dplyr::select(participant_id = `Participant Name`, 
                                              analysis_id = `Analysis Index`, 
                                              time_st = `Video Time`,
                                              valence = Valence) 

# Save data file. 
#saveRDS(BFR_ts_data, file = paste0("output/", analysis_type, "/BFR_ts_missing_data.rds"))
BFR_ts_data <- readRDS(file = paste0("output/", analysis_type, "/BFR_ts_missing_data.rds"))


# Recode interaction partner based on the original file names in participant_code. 
BFR_ts_data <- BFR_ts_data %>%
  # Extract participant number from the BFR participant_id column and convert to numeric.
  dplyr::mutate(participant_id = as.numeric(stringr::str_match(participant_id, '(\\d+)')[,2]),
                age = as.numeric(stringr::str_match(analysis_id, 'Analysis (\\d)\\d')[,2]),
                int_partner = as.numeric(stringr::str_match(analysis_id, 'Analysis \\d(\\d)')[,2]),
                # Uncomment if adding out-of-plane head rotation variables
                int_partner_join = dplyr::case_when(
                  # BFR 1 (father) is assigned to OBXT 0 (father)
                  int_partner == 1 ~ 0,
                  # BFR 2 (mother) is assigned to OBXT 1 (mother)
                  int_partner == 2 ~ 1,
                  # BFR 3 (stranger) is assigned to OBXT 2 (stranger)
                  int_partner == 3 ~ 2,
                  TRUE ~ as.numeric(NA))) %>%
  # Re-code the error variable into three categories: fit failed, find failed, and available.
  dplyr::mutate(error = dplyr::case_when(valence == 'FIT_FAILED' ~ 'fit failed',
                                         valence == 'FIND_FAILED' ~ 'find failed',
                                         TRUE ~ ''),
                dplyr::across(.cols = c(valence), .fns = ~ dplyr::na_if(.x, 'FIND_FAILED')),
                dplyr::across(.cols = c(valence), .fns = ~ dplyr::na_if(.x, 'FIT_FAILED')),
                dplyr::across(.cols = c(valence), .fns = as.numeric)) 

# Remove any duplicates (this could occur if logs in the data folder are duplicated).
BFR_ts_data <- distinct(BFR_ts_data)

# Save data file containing out-of-head rotation variables. 
#saveRDS(BFR_ts_data, file = paste0("output/", analysis_type, "/BFR_ts_data_head_rotations.rds"))

# Check count unique participant files
# N = 307?
BFR_ts_data %>% 
  dplyr::group_by(analysis_id) %>%
  distinct(participant_id) %>%
  dplyr::count(analysis_id)

# Inspect types of missingness in the BFR output.
BFR_ts_data %>% 
  dplyr::count(error)

# Downsample by filtering every second sample (30 >> 15 Hz)
BFR_ts_data <- BFR_ts_data %>%
  dplyr::arrange(participant_id, analysis_id, time_st) %>%
  dplyr::group_by(participant_id, analysis_id) %>%
  # Keep every second row starting at 1. So 1,3,5,...
  dplyr::filter(row_number() %% 2 == 1) %>%
  dplyr::ungroup()

# Disregard the tryadic interaction part of the video data using the overview of
# the start-stop times of the dyadic interactions (taken from the manual coding).
# Read in the start-stop time overview for each measurement wave.
df_4 <- readxl::read_xlsx('data/Start_time_dyadic.xlsx', sheet = "4_months") %>%
  mutate(int_partner = as.numeric(int_partner))
df_8 <- readxl::read_xlsx('data/Start_time_dyadic.xlsx', sheet = "8_months") %>% 
  mutate(int_partner = as.numeric(int_partner))

# Attach age variable
start_time_overview <- dplyr::bind_rows(df_4, df_8, .id = 'age') 

# Compute average video durations
video_duration <- start_time_overview %>% 
  dplyr::mutate(vid_duration = (as.numeric(end_dyadic) - as.numeric(start_dyadic))) 

mean_video_duration <- video_duration %>%
  dplyr::summarize(mean_vid_duratin = round(mean(vid_duration, na.rm = TRUE), digits = 2), 
                   sd_vid_duration = round(sd(vid_duration, na.rm = TRUE), digits = 2), 
                   n = sum(!is.na(vid_duration)))

# Test whether there is any text in the numeric fields preventing
# conversion to numeric. If there are no text fields, should evaluate
# to "no issues with text in numeric fields". 
start_time_overview %>%
  dplyr::filter(!is.na(start_dyadic)) %>% 
  dplyr::mutate(start_test = as.numeric(start_dyadic), 
                end_test = as.numeric(end_dyadic)) %>%
  dplyr::filter((is.na(end_test) & !is.na(end_dyadic)) | (is.na(start_test) & !is.na(start_dyadic))) %>%
  { if(nrow(.) > 0) View(., 'Numeric conversion check') else print('No issues with text in numeric fields') }

start_time_overview %>%
  # If start_end_dyadic is filled in, start and end should be filled as well.
  dplyr::filter(!is.na(start_end_dyadic) & (is.na(end_dyadic) | is.na(start_dyadic))) %>%
  { if(nrow(.) > 0) View(., 'Numeric conversion check') else print('Fill check passed') }

# Get the start-stop times of the available manually coded data. 
start_time_overview <- start_time_overview %>%
  dplyr::mutate(age = dplyr::if_else(age == 1, 4, 8),
                participant_id_orig = participant_id,
                participant_id = as.numeric(stringr::str_match(participant_id, '(\\d+)')[,2])) %>%
  # Filter the data for which there is a recorded starting time for the dyadic interaction.
  dplyr::filter(!is.na(start_dyadic)) %>% 
  dplyr::select(participant_id, age, int_partner, start_dyadic, end_dyadic) %>%
  dplyr::mutate(dplyr::across(c(dplyr::everything()), as.numeric))

# Attach the dyadic start time to the BFR data by pp_id, age, int_partner
BFR_ts_data <- BFR_ts_data %>%
  dplyr::left_join(start_time_overview, 
                   by = c('participant_id', 'age', 'int_partner_join' = 'int_partner')) 

BFR_ts_data <- BFR_ts_data %>%
  # Transform time stamp from 00:00:00.000 format into seconds.
  dplyr::mutate(time_st_seconds = lubridate::period_to_seconds(lubridate::hms(time_st))) %>%
  # Keep only the rows for which the time stamp is greater/eq than start_dyadic & lower/eq than end_dyadic or if end_dyadic is missing. 
  dplyr::filter(time_st_seconds >= start_dyadic & (is.na(end_dyadic) | (time_st_seconds <= end_dyadic))) %>%  
  dplyr::select(-start_dyadic, -end_dyadic) 

# Add a column with the timestamps within each analysis_id (indexing measurement wave * interaction partner)
BFR_ts_data <- BFR_ts_data %>%
  dplyr::group_by(participant_id, analysis_id) %>%
  dplyr::mutate(time_st_seconds_join = time_st_seconds - min(time_st_seconds)) %>%
  dplyr::ungroup()

# Remove the start time overview from the environment. 
rm(start_time_overview)

## Match the OBXT and BFR time stamps ----

# Recalculate the OBXT time stamps to the BFR format.
OBXT_ts_data <- OBXT_ts_data %>% 
  dplyr::group_by(participant_code) %>%
  # Get ms since start of trial, round by floor to 3 sig digits.
  dplyr::mutate(time_join = time_rel_sf - min(time_rel_sf),
                time_st_seconds_join = floor(time_join * 1000)/1000)  %>%
  dplyr::ungroup()

# Loop over each combination of id, age and interation partner.
# If id x age x partner combo is found in both datasets, match them by closest timestamp and append to output df (df_out)
df_out <- data.frame('participant_id' = numeric(), 'int_partner_join' = numeric(), 'age' = numeric(),
                     'BFR' = numeric(), 'OBXT' = numeric())
# Used to keep track of number of rows from each source (BFR and OBXT) could be joined.
df_diagnostic <- dplyr::tibble('participant_id' = numeric(), 'int_partner_join' = numeric(), 'age' = numeric(),
                               'nbfr' = numeric(), 'nobxt' = numeric())
for (id in unique(BFR_ts_data$participant_id)){
  subset_id <- dplyr::filter(BFR_ts_data, participant_id == id)
  
  for (current_age in unique(subset_id$age)){
    subset_id_age <- dplyr::filter(subset_id, age == current_age)
    
    for (partner in unique(subset_id_age$int_partner_join)){
      BFR <- BFR_ts_data %>% 
        dplyr::filter(participant_id == id, int_partner_join == partner, age == current_age)
      OBXT <- OBXT_ts_data %>%
        dplyr::select(participant_id, int_partner_join, age, OBXT = time_st_seconds_join) %>%
        dplyr::filter(participant_id == id, int_partner_join == partner, age == current_age)
      
      #print(c(id, current_age, partner))
      #print(nrow(df_out))
      print(c(id, partner, current_age, nrow(BFR), nrow(OBXT)))
      df_diagnostic <- dplyr::add_row(df_diagnostic, 
                                      'participant_id' = id, 
                                      'int_partner_join' = partner, 
                                      'age' = current_age, 
                                      'nbfr' = nrow(BFR), 
                                      'nobxt' = nrow(OBXT))
      
      if (nrow(BFR) > 0 & nrow(OBXT) > 0){
        OBXT$BFR <- purrr::map_dbl(OBXT$OBXT, ~ BFR$time_st_seconds_join[which.min(abs(BFR$time_st_seconds_join - .x))])
        df_out <- dplyr::bind_rows(df_out, OBXT)
      }
    }    
  }
}

# Visually inspect any cases in which the number of rows differs substantially between the sources.
# Check large differences for errors in the labeling of the (raw) data.
df_diagnostic %>%
  dplyr::mutate(diff = abs(nbfr - nobxt)) %>%
  tibble::view('Row match test')

rm(list = c('BFR', 'OBXT'))

# Get rid of duplicates created by matching.
df_out <- df_out %>% 
  dplyr::arrange(participant_id, age, int_partner_join) %>%
  dplyr::filter(BFR != dplyr::lag(BFR, n = 1))
df_out <- df_out %>% 
  dplyr::arrange(participant_id, age, int_partner_join) %>%
  dplyr::filter(OBXT != dplyr::lag(OBXT, n = 1))

## Sync the BFR and OBXT data streams into a single data frame by participant ID and time stamp ----
# Could also consider left join to only keep rows that were matched, filter would then not be required. 
BFR_OBXT_data <- df_out %>%
  dplyr::full_join(., dplyr::mutate(BFR_ts_data, bfr = TRUE),  
                   by = c('participant_id', 'age', 'int_partner_join',
                          'BFR'='time_st_seconds_join'), 
                   copy = FALSE) %>%
  dplyr::full_join(., dplyr::mutate(OBXT_ts_data, obxt = TRUE),  
                   by = c('participant_id', 'age', 'int_partner_join',
                          'OBXT'='time_st_seconds_join'), 
                   copy = FALSE, suffix = c(".FR", ".OB")) %>%
  dplyr::filter(!is.na(OBXT), !is.na(BFR))

count(BFR_OBXT_data, is.na(bfr))

# Descriptives ----

## Sample Descriptives ----
# Table 2 in Methods section: Mean and SD of video duration per age group.
BFR_OBXT_data %>%
  dplyr::filter(OBXT = TRUE) %>%
  dplyr::group_by(age, participant_id) %>% 
  # Across age and participant_id
  dplyr::summarize(n_vid = dplyr::n_distinct(participant_code), vid_dur_mean = (dplyr::n()/15/n_vid)) %>%
  # Across age (participant_id is automatically ungrouped)
  dplyr::summarize(vid_dur_sd = sd(vid_dur_mean), vid_dur_mean = mean(vid_dur_mean)) %>%
  ungroup()

# N time samples per age.
BFR_OBXT_data %>% 
  dplyr::group_by(age) %>%
  dplyr::summarize(n_ts = dplyr::n()) %>%
  ungroup()

# N videos per age.
BFR_OBXT_data %>% 
  #dplyr::group_by(age) %>%
  # N videos per age and interaction partner.
  dplyr::group_by(age, int_partner) %>%
  dplyr::summarize(
    n_ids = dplyr::n_distinct(participant_id),
    n_obs_participant_code = dplyr::n_distinct(participant_code)) %>%
  dplyr::ungroup()

# Compute infant sex. 
# The counts did not add up with the overall number of participants - found 
# mistakes in the manual data for one participant, who is reported as female 
# in one set of observations and male in another. The sex for this participant
# were manually edited from the data collection log.
pp_descriptives <- BFR_OBXT_data %>% 
  dplyr::select(participant_id, age, sex) %>% 
  dplyr::distinct()
View(pp_descriptives)
pp_descriptives %>% dplyr::group_by(age) %>%
  dplyr::summarise(n_male = sum(sex))

# List of all participant id's at the 4 and 8 month measurement wave.
pp_list <- BFR_OBXT_data %>% 
  dplyr::select(participant_id, age) %>% 
  dplyr::distinct()
View(pp_descriptives, title = "Age and sex per infant")

## Missing Data ----

# Visualize the data available in both OBXT and BFR.
BFR_OBXT_data %>% 
  dplyr::count(obxt, bfr)

# Inspect types of missingness in the BFR output.
BFR_OBXT_data %>% 
  dplyr::count(error)

# Compute Table 5 descriptives.
# Overview of missing data from the automatic analysis per manual FE category.
BFR_OBXT_data %>% 
  #dplyr::group_by(error) %>%
  #dplyr::group_by(age, error) %>%
  #dplyr::group_by(FE_category, error) %>%
  dplyr::group_by(age, FE_category, error) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(prop = count/sum(count)) %>%
  View()

# Compute Table 4 descriptives.
# Overview of available data from the manual coding per manual FE category.
BFR_OBXT_data %>% 
  dplyr::group_by(age, FE_category) %>%
  #dplyr::group_by(FE_category) %>%
  #dplyr::group_by(age) %>%
  dplyr::summarise(count = dplyr::n()) %>%
  dplyr::mutate(prop = count/sum(count))

# Prepare data for descriptive & ROC analyses ----

## Re-code OBXT facial expression category frequency into a factor for grouping ----
BFR_OBXT_data %>% 
  dplyr::count(FE_category)

# Any FE category with no label is relabeled as "error"
BFR_OBXT_data <- BFR_OBXT_data %>%
  dplyr::mutate(FE_category = dplyr::case_when(
    FE_category == 'Negative' ~  "negative",
    FE_category == 'Positive' ~  "positive",
    FE_category == 'Neut' ~  "neutral",
    FE_category == 'Not codable' ~  "not_codable",
    TRUE ~ "error"))

# Count time samples in each manually coded facial expression category. 
BFR_OBXT_data %>% 
  dplyr::count(FE_category)

# Count the number of participants with valid data (not marked as "not codable"
# by the manual coders).
BFR_OBXT_data %>% 
  dplyr::filter(FE_category != "not_codable") %>%
  dplyr::count(age, error)

## Subset the available OBXT data and filter out the "error" FE category ----
BFR_OBXT_valid_data <- BFR_OBXT_data %>%
  # No BFR reader errors or missing data in the BFR set.
  dplyr::filter(error == '') %>%
  # Exclude missing OBXT data.
  dplyr::filter(obxt)

## Save data for lag analysis ----

# Continue in "03_test_lag.R" after saving the data.
saveRDS(BFR_OBXT_valid_data, file = paste0("output/", analysis_type, "/BFR_OBXT_data.rds"))

# Continue in "03_test_lag.R" after saving the data.
saveRDS(BFR_OBXT_valid_data, file = paste0("output/", analysis_type, "/BFR_OBXT_data_head_rotations.rds"))




