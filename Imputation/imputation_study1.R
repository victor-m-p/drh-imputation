source('project_support.R')
library(missForest)
library(tidyverse)
print(getwd())

# setup 
study <- 2

# path to output folder
modified_folder_path <- paste0("output/study", study, "/missForest")
if (!dir.exists(modified_folder_path)) {
  dir.create(modified_folder_path)
}

# path to input folder 
folder_path <- paste0("output/study", study, "/additional_NA")
file_names <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

data_missing <- list()
for (file_path in file_names) {
  file_name <- basename(file_path)
  data <- read.csv(file_path)
  file_id <- sub(".*/NA_(.*)\\.csv", "\\1", file_path)
  data_missing[[file_id]] <- data
}

# Questions has the correct data types 
questions <- read_csv("../Data/Preprocessed/answers.csv", show_col_types = FALSE) 
questions <- questions %>% select(question_id, data_type) %>% distinct()

# Create index of variable types for GLRM and conversion of data type
print('fix variable types')
var_types <- lapply(data_missing, variables_types)

# Convert variables to correct class
print('correct variable classes')
data_class <- Map(correct_class, data_missing, var_types)

## dataset id strings
question_level <- read_csv(paste0("../Data/Preprocessed/question_level_study", study, ".csv"), show_col_types = FALSE)
dataset_id_strings <- generate_datasets_ids_string(question_level, max_level = max(question_level$question_level))

# Loop through each level 
imputed_data_class <- data_class
print('running imputation')
for (stage in seq_along(dataset_id_strings)) {
  print(sprintf("imputation stage: %s", stage))
  # Identify the columns to keep for this stage
  columns_keep <- dataset_id_strings[[stage]]
  
  # Subset the original dataframes to keep only the relevant columns for this stage
  data_class_subset <- subset_dataframes(imputed_data_class, columns_keep)
  
  # Impute missing values using the subset data
  # Make sure that the imputation function returns a list of dataframes with the same structure as data_class_subset
  forest_imp_subset <- lapply(data_class_subset, function(x) rf_impute(data = x, seed = seed))

  # Update the imputed_data_class with the new imputed values from forest_imp_subset
  for (i in seq_along(imputed_data_class)) {
    # Make sure that forest_imp_subset has the same structure and order of rows as imputed_data_class
    # Replace the columns with the newly imputed data
    imputed_data_class[[i]][, columns_keep] <- forest_imp_subset[[i]][, columns_keep]
  }

  # If parent question is no then child question should be no 
  imputed_data_class <- lapply(imputed_data_class, function(df) update_child_entries(df, question_level, function(x) x == 0, 0))
}

# write imputations
write_imputations(data = imputed_data_class, study = study, algorithm = "missForest", appendix = "mf")