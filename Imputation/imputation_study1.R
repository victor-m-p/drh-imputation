source('project_support.R')
library(missForest)
library(tidyverse)

# path to output folder
modified_folder_path <- "output/study1/missForest"
if (!dir.exists(modified_folder_path)) {
  dir.create(modified_folder_path)
}

# path to input folder 
folder_path <- "output/study1/additional_NA"
file_names <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

data_missing <- list()
for (file_path in file_names) {
  file_name <- basename(file_path)
  data <- read.csv(file_path)
  file_id <- sub(".*/NA_(.*)\\.csv", "\\1", file_path)
  data_missing[[file_id]] <- data
}

# find relevant questions for study 1
answers <- read_csv("../Data/Preprocessed/answers.csv")
answers <- answers %>%
  select(question_id, parent_question_id) %>%
  distinct()
answers$question_level <- sapply(answers$question_id, function(x) find_question_level(x, answers))
study1 <- read_csv("Data/Preprocessed/answers_study1.csv")
column_names <- names(study1)
filtered_names <- column_names[str_detect(column_names, "^X")]
transformed_names <- str_replace(filtered_names, "^X", "")
study1_questions <- data.frame(question_id = as.integer(transformed_names))
merged_df <- inner_join(answers, study1_questions, by = "question_id")

# add nan 
updated_data_missing <- lapply(data_missing, function(df) {
  update_child_questions(df, merged_df)
})

# Questions has the correct data types 
questions <- read_csv("../Data/Raw/drh_v6_poll.csv") 

# Create index of variable types for GLRM and conversion of data type
var_types <- lapply(data_missing, variables_types)

# Convert variables to correct class
data_class <- Map(correct_class, data_missing, var_types)

## dataset id strings
dataset_id_strings <- generate_datasets_ids_string(merged_df, max_level = max(merged_df$question_level))

# Loop through each level 
imputed_data_class <- data_class
for (stage in seq_along(dataset_id_strings)) {
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
}

# write imputations
write_imputations(data = imputed_data_class, study = 1, algorithm = "missForest", appendix = "mf")