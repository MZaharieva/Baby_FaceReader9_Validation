#---
#**Author:** "Martina Zaharieva"

#**Title:** "Validating Automatic Classification of Global Emotional Valence Using
#*Baby FaceReader 9 in a Longitudinal Sample of 4- and 8-Month-Old Infants"

#**Aim:** "Run the analysis scripts using the dataset from the automatic Baby Facereader 9
#*analysis with temporal smoothing (for the main analysis reported in the manuscript) or 
#*the raw automatically analyzed data without temporal smoothing (for robustness check)."

#**Study design:** "Compare automatic-manual performance vs.manual-manual coding 
#*of facial expression categories across two measurement waves - 4 and 8 months."

#**Longitudinal study design:** "The data were collected in Eliala Salvadori's
#*longitudinal study. Each infant performed a 2-min dyadic and a 2-min tryadic
#*face-to-face interaction with three interaction partners (mother, father, 
#*unfamiliar adult) at two measurement points: 4 and 8 months of age. The video
#*data from the dyadic interaction are used in the main analysis reported in the
#*manuscript. The video data from the tryadic interaction were used for developing 
#*and piloting the data processing scripts and to inform analysis decisions for 
#*the project's pre-registration."
#---

# Clear environment ----
rm(list = ls())

## Main analysis ----

# Analysis type, sets input and output directories (options: "smooth", "raw")
# If not set, the default used is smooth.
analysis_type <- "smooth"

# Specify scripts to be run ----

ls_scripts <- c("02_compute_face_size.R", "03_sync_clean_data.R", "04_test_lag.R", 
                "05_apply_lag_and_explore.R", "06_ROC_global_valence.R", 
                "07_explore_AU_distributions.R", "08_logistic_regression_apriori_AU.R")

# Run scripts ----
for (script in ls_scripts) {
  source(script, echo = TRUE)
}
