library(tidyverse)
library(devtools)
source_url('https://raw.githubusercontent.com/R-miss-tastic/website/master/static/how-to/generate/amputation.R')

# function to add nan 
add_NA <- function(data, missing_pattern, missing_prop, id_vars, study_name){
  study_path <- file.path("output", study_name, "additional_NA")
  if(!dir.exists(study_path)) {
    dir.create(study_path, recursive = TRUE)
  }
  # Remove ID variables
  id_var <- data %>% select(all_of(id_vars))
  data_no_id <- data %>% select(-all_of(id_vars))

  for(i in 1:2){ # Adjusted for testing
    # Add missing values
    missingness <- produce_NA(data_no_id, mechanism = missing_pattern, perc.missing = missing_prop, seed = i)
    missing <- cbind(id_var, missingness$data.incomp)
    missing_per = missing_prop * 100
  
    # Write to CSV for study1
    write_csv(missing, paste0(study_path, "/NA_", missing_pattern, "_", missing_per, "_", i, ".csv"))
  }
}

# load data 
df_study1 <- read_csv("../Data/Preprocessed/answers_study1.csv")
df_study2 <- read_csv("../Data/Preprocessed/answers_study2.csv")
id_vars <- readLines("id_vars.txt")

# create missingness over grid
grid_study1 <- expand.grid(missing_pattern = c("MCAR", "MAR", "MNAR"), missing_prop = c(0.1, 0.2, 0.3, 0.4, 0.5))
grid_study2 <- expand.grid(missing_pattern = c("MCAR", "MAR", "MNAR"), missing_prop = c(0.1, 0.2, 0.3))

# add NA for study 1
for(i in 1:nrow(grid_study1)){
  # Extract the current missing_pattern and missing_prop
  current_pattern <- grid_study1$missing_pattern[i]
  current_prop <- grid_study1$missing_prop[i]
  
  # Call function with the current parameters
  add_NA(data = df_study1, missing_pattern = current_pattern, missing_prop = current_prop, id_vars = id_vars, study_name = "study1")
}

# add NA for study 2
for(i in 1:nrow(grid_study2)){
  # Extract the current missing_pattern and missing_prop
  current_pattern <- grid_study2$missing_pattern[i]
  current_prop <- grid_study2$missing_prop[i]
  
  # Call function with the current parameters
  add_NA(data = df_study2, missing_pattern = current_pattern, missing_prop = current_prop, id_vars = id_vars, study_name = "study2")
}