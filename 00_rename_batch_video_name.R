#---
#**Aim:** 
#*"Rename all video files for automatic import in a Baby FaceReader project."
#---

# Libraries ----
library(tidyverse) # data wrangling and manipulation

# Specify the folder where the video data are stored.
dir <- 'X:/Babylab/Babylab/Eliala_My-BEST/BabyFACS/_raw video_data'

# List the names of the available video data files.
files <- list.files(dir)

# Specify the expected file name pattern.
patterns <- stringr::str_match(files, "([0-9]{3}) ?-([0-9]{1})(E|F|M).*?.mp4")

print(paste('Failed:', paste0(files[! files %in% patterns[,1]], collapse = ', ')))

# A function that assigns new file names using the original file name patterns: 
# 1) Recode Father (F) = 1; Mother (M) = 2; Unfamiliar Adult (E) = 3
# 2) Recode 4 = 4 months; 8 = 8 months
# E.g., The participant code for the interaction of participant 001 at 4 months with mother is "1_42"
original_names <- patterns[,1]
new_names <- c()
for (row in 1:nrow(patterns)){
  row <- patterns[row,]
  if (any(is.na(row))){
    print(paste('Skipping', row[1]))
    new_names <- append(new_names, '')
    next
  } else{
    if (row[4] == 'F'){
      # Assign 1 to father.
      analysis_id <- 1
    } else if (row[4] == 'M'){
      # Assign 2 to mother.
      analysis_id <- 2
    } else {
      # Assign 3 to stranger.
      analysis_id <- 3
    }
    
    # Recode to the new file names.
    new_name <- paste0(as.numeric(row[2]), "_", row[3], analysis_id, ".mp4")
    out_path <- paste0(dir, '/', new_name)
    # Throw an error whenever duplicate file names are created.
    if (file.exists(out_path)){
      stop(paste("Collision: Two input files led to the same output file name.", new_name))
    }
    
    file.rename(paste0(dir, '/', row[1]), out_path)
    print(paste(row[1], '->', new_name))
    new_names <- append(new_names, new_name)
  }
}