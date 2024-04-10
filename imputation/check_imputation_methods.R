remove_na_from_constant <- function(df) {
  relevant_cols <- names(df)[startsWith(names(df), "X")]
  
  for(col_name in relevant_cols) {
    # Since we now assume all these columns are factors, directly proceed to check for single level
    if(length(levels(df[[col_name]])) == 1) {
      # Replace NAs with the observed level
      observed_level <- levels(df[[col_name]])[1]
      df[[col_name]][is.na(df[[col_name]])] <- observed_level
    }
  }
  
  return(df)
}

mice_impute <- function(data, seed) {
  # Convert Region to numeric as MICE performs poorly with too many factors
  # This seems suspicious immediately because the dummies are factors.
  # Why would it do poorly with factors (and better with character?)

  # Question with MICe is whether we should actually remove collinear.
  # Another question is whether we should remove constant and just set that to not NA.
  # I.e., do a "dumb" imputation where if it is constant then we always assign it as that value
  entry_id_col <- data$entry_id 
  data <- data %>% 
    select(-entry_id)
  
  # fill constant columns
  data <- remove_na_from_constant(data)

  MICE_imp <- mice::mice(data, m=1, print = FALSE, seed = seed, nnet.MaxNWts = 10000, remove.collinear=FALSE)
  MICE_imputed <- mice::complete(MICE_imp)
  MICE_imputed <- cbind(entry_id_col, MICE_imputed)
  return(MICE_imputed)
} 

rf_impute <- function(data, seed) {
  
  # Save entry_id column and remove it from the data
  entry_id_col <- data$entry_id
  data_without_entry_id <- data %>%
    select(-entry_id)
  
  # Perform imputation on data without entry_id
  forest_imp <- missForest(data_without_entry_id, variablewise = TRUE)
  forest_df <- forest_imp$ximp
  
  # Add the entry_id column back to the imputed data
  forest_df <- cbind(entry_id = entry_id_col, forest_df)
  
  return(forest_df)
}

mode_impute <- function(data, seed) {
  
  # Function to calculate the mode for a factor column
  calculate_mode <- function(x) {
    mode_level <- names(sort(table(x), decreasing = TRUE))[1]
    return(mode_level)
  }
  
  # Identify columns that start with "X"
  relevant_cols <- names(data)[startsWith(names(data), "X")]
  
  # Apply mode imputation to each relevant column
  for(col_name in relevant_cols) {
    # Calculate mode for the column
    mode_value <- calculate_mode(data[[col_name]])
    
    # Replace NA values with the mode
    data[[col_name]][is.na(data[[col_name]])] <- mode_value
  }
  
  return(data)
}

source('Imputation/imputation_functions.R')
library(missForest)
library(tidyverse)
library(mice)
print(getwd())

# setup 
study <- 1

# path to output folder
#modified_folder_path <- paste0("Imputation/output/study", study, "/missForest")
#if (!dir.exists(modified_folder_path)) {
#  dir.create(modified_folder_path)
#}

# path to input folder 
folder_path <- paste0("Imputation/output/study", study, "/additional_NA")
file_names <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

data_missing <- list()
for (file_path in file_names) {
  file_name <- basename(file_path)
  data <- read.csv(file_path)
  file_id <- sub(".*/NA_(.*)\\.csv", "\\1", file_path)
  data_missing[[file_id]] <- data
}

# Questions has the correct data types 
questions <- read_csv("Data/Preprocessed/answers.csv", show_col_types = FALSE) 
questions <- questions %>% select(question_id, data_type) %>% distinct()

# Create index of variable types for GLRM and conversion of data type
print('fix variable types')
var_types <- lapply(data_missing, variables_types)

# Convert variables to correct class
print('correct variable classes')
data_class <- Map(correct_class, data_missing, var_types)

## dataset id strings
question_level <- read_csv(paste0("Data/Preprocessed/question_level_study", study, ".csv"), show_col_types = FALSE)
dataset_id_strings <- generate_datasets_ids_string(question_level, max_level = max(question_level$question_level))

# okay now just do it for 1 to test: 
imputed_data_class <- data_class
stage <- 1
columns_keep <- dataset_id_strings[[stage]]
data_class_subset <- subset_dataframes(imputed_data_class, columns_keep)

df_test <- data_class_subset[[1]]
df_test

# imputation methods
forest_imp_subset <- lapply(data_class_subset, function(x) rf_impute(data = x, seed = 658))
mice_imp_subset <- lapply(data_class_subset, function(x) mice_impute(data = x, seed = 658)) 

# what if we wanted to do the simplest possible
# just impute the most common value for each column

# we want to see the warnings though
mice_test <- function(data, seed){
  entry_id_col <- data$entry_id 
  data <- data %>% 
    select(-entry_id)
  
  # fill constant columns
  data <- remove_na_from_constant(data)

  MICE_imp <- mice::mice(data, m=1, print = FALSE, seed = seed, nnet.MaxNWts = 10000, remove.collinear=FALSE)
  return (MICE_imp)
}
names(data_class_subset)

data_case <- data_class_subset[[5]]
data_case <- remove_na_from_constant(data_case)
mice_imp <- mice_test(data_case, 512)
head(mice_imp$loggedEvents) # mostly just logging logistic regression and constants
d <- mice::complete(mice_imp)
head(d)
length(levels(data_case[["X4699"]]))
d[["X4699"]]
d[["X4701"]]
levels(d[["X4699"]])
data_case[["X4699"]] <- as.factor(data_case[["X4699"]])
questions %>% filter(question_id == 4699)
column_types <- sapply(data_case, class)
column_types
## check mode impute ##
mode_imp_subset <- lapply(data_class_subset, function(x) mode_impute(x, seed))
mode_imp_subset

# 4699: what is it?
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