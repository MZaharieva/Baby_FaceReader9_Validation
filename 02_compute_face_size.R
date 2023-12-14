#---
#**Aim:** 
#*"Compute the area of interest (AOI) of the mouth and eye area of the infant face
#* (in pixels) for the project Baby FaceReader Validation."
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
library(tidyr)

## Read and prepare data ----

# Based on the dataset ("smooth" or "raw"), specify input and output paths. 
if(analysis_type == "smooth") {
  # Path to individual data files analyzed with temporal smoothing.
  bfr_data_path <- 'data/BFR9_smoothing/Logs_landmarks_heart_rate/'
} else if (analysis_type == "raw") {
  # Path to individual data files analyzed withOUT temporal smoothing.
  bfr_data_path <- 'data/BFR9_raw/Logs_landmarks_heart_rate/'
} 

# List the available file names.
# Loop across all file names and append the data into the same data frame.
bfr_data_files <- list.files(bfr_data_path, pattern = '.*detailed.txt$')

# Read the log files and append the source (filename) in the column "src"
BFR_ts_data <- purrr::map_dfr(bfr_data_files, 
                              # All fields are treated as character type to accommodate for missing data.
                              ~ readr::read_tsv(paste0(bfr_data_path, .x), skip = 10, 
                                                col_types = paste(rep("c", 79), collapse = "")) %>%
                                dplyr::filter(Landmarks != "FIT_FAILED") %>%
                                dplyr::mutate(src = .x,
                                              # Unselect to add age as a variable.
                                              age = as.numeric(stringr::str_match(src, 'Analysis (\\d)\\d')[,2]),
                                              ) %>%
                                dplyr::select(src, age, Landmarks))

# Save data file. 
saveRDS(BFR_ts_data, file = paste0("output/", analysis_type, "/BFR_ts_data_landmarks.rds"))

# Read data file.
BFR_ts_data <- readRDS(paste0("output/", analysis_type, "/BFR_ts_data_landmarks.rds"))
BFR_ts_data_age <- readRDS(paste0("output/", analysis_type, "/BFR_ts_data_landmarks_age.rds"))

## AOI (pixels) of the infant face ----

#width face = left most x coord left eyebrow - right most x coord right eyebrow; 
#hight face = (x coord left and right eyebrow)/2 

# Column Landmarks contains the X-Y-coordinates of the mesh dots around the eyes, nose and mouth. 
# The log file has 134 numbers in the column Landmarks. The first two numbers are the X- and Y-coordinates
# of landmark 0, the third and fourth are the coordinates of landmark 1, etc. The X,Y-coordinates are in pixels
# relative to the upper left corner of the image.(from user manual; Noldus, 2021)

#n0-9 - left eyebrow
#n10-19 - right eyebrow
#n20-21 - corners left eye
#n22-23 - corners right eye
#n24-26 - nose
#n27-66 - mouth

# Inspect dataframe structure
BFR_ts_data %>% head()
BFR_ts_data_age %>% head()

# 134 numbers, 2 numbers per landmark, so 67 landmarks
# There are 134 "." in each row, strongly indicating that these are decimals.
stringr::str_count(BFR_ts_data$Landmarks[1], fixed("."))

# Create a vector of column names.
columns <- c()
for (datapoint in 0:133){
  # integer cuts off decimals so x and y are mapped to the same landmark.
  landmark_id <- as.integer(datapoint/2)
  
  if (landmark_id <= 9){
    landmark <- "left_eyebrow"
  } else if (landmark_id <= 19){
    landmark <- "right_eyebrow"
  } else if (landmark_id <= 21){
    landmark <- "left_eye"
  } else if (landmark_id <= 23){
    landmark <- "right_eye"
  } else if (landmark_id <= 26){
      landmark <- "nose"
  } else if (landmark_id <= 66){
    landmark <- "mouth"
  } else {
    print("ERROR: Extra datapoints?")
  }
    
  if (datapoint %% 2 == 0){
    # 0 and even numbers are x
    col <- paste(landmark, landmark_id, "x", sep="_")
  } else {
    # rest are y
    col <- paste(landmark, landmark_id, "y", sep="_")
  }
  columns <- append(columns, col)
}
# Split the landmarks field into x and y coordinates.
BFR_ts_data_age <- BFR_ts_data_age %>% 
  #filter(row_number() < 1000) %>%
  tidyr::separate(Landmarks, 
                  sep = ",", 
                  into = columns,
                  # Convert to numeric, saves memory
                  convert = TRUE)

# 135 columns, two for each landmark and 1 for src.
# Certain columns are not relevant for the size (nose, eyes)
names(BFR_ts_data)
# Reorder
BFR_ts_data_age <- BFR_ts_data_age %>%
  dplyr::select(src, dplyr::everything())

# Save clean data.
saveRDS(BFR_ts_data, file=paste0("output/", analysis_type, "/BFR_ts_data_landmarks_clean.rds"))

# Read clean data.
BFR_ts_data <- readRDS(paste0("output/", analysis_type, "/BFR_ts_data_landmarks_clean.rds"))

# Plot x, y coordinates to visualize the different landmarks
coords_2d <- BFR_ts_data %>%
  # Get random sample
  slice(1) %>%
  select(-src)
x <- coords_2d[seq(1, length(coords_2d), by=2)] %>% unlist()
y <- coords_2d[seq(2, length(coords_2d), by=2)] %>% unlist()
# 0,0 is the top-left pixel so correct - not important for AOI size calculation
plot(x, y)
y_flipped <- y * -1
plot(x, y_flipped)

# Vertical (y) and horizontal (x) size:
x_size <- max(x) - min(x)
y_size <- max(y) - min(y)

# Now apply this to the whole dataset:
BFR_face_coords <- BFR_ts_data %>%
  dplyr::select(-src)

# 4 mo + 8 mo = 542793 + 517783 = 1060576
# Total is 1068642, which is 8066 higher 

# at 4 months:
BFR_face_coords <- BFR_ts_data_age %>%
  dplyr::filter(age == 4) %>%
  dplyr::select(-src)

# at 8 months:
BFR_face_coords <- BFR_ts_data_age %>%
  dplyr::filter(age == 8) %>%
  dplyr::select(-src)  

x_min <- apply(BFR_face_coords[seq(1, length(BFR_face_coords), by=2)],1,min)
x_max <- apply(BFR_face_coords[seq(1, length(BFR_face_coords), by=2)],1,max)
y_min <- apply(BFR_face_coords[seq(2, length(BFR_face_coords), by=2)],1,min)
y_max <- apply(BFR_face_coords[seq(2, length(BFR_face_coords), by=2)],1,max)

x_size <- x_max - x_min
y_size <- y_max - y_min
area <- x_size * y_size
print(mean(area))
print(min(area))
print(max(area))
print(sd(area))
min(area) / (780 * 1280)*100
mean(area)/(780 * 1280)
min(x_size)
max(x_size)
