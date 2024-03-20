library(tidyverse)

# load data
answers <- read_csv("Data/Preprocessed/answers.csv")
answers <- answers %>%
  select(question_id, parent_question_id) %>%
  distinct()

# find question level
find_question_level <- function(question_id, drh) {
  computed_levels <- list() # Use a list for caching in R
  
  inner_find_question_level <- function(question_id) {
    # Base case: if the parent question ID is 0, the level is 0
    if (question_id == 0) {
      return(0)
    }
    # If already computed, return the stored level
    if (!is.null(computed_levels[[as.character(question_id)]])) {
      return(computed_levels[[as.character(question_id)]])
    }
    
    # Recursive case: find the parent question's ID and level
    parent_id <- drh[drh$question_id == question_id, ]$parent_question_id
    level <- inner_find_question_level(parent_id) + 1
    # Store the computed level in the cache
    computed_levels[[as.character(question_id)]] <- level
    return(level)
  }
  
  return(inner_find_question_level(question_id))
}

answers$question_level <- sapply(answers$question_id, function(x) find_question_level(x, answers))



# now for a given study only take the questions that are relevant
study1 <- read_csv("Data/Preprocessed/answers_study1.csv")
# Step 1: Extract and transform column names
column_names <- names(study1)
filtered_names <- column_names[str_detect(column_names, "^X")]
transformed_names <- str_replace(filtered_names, "^X", "")
study1_questions <- data.frame(question_id = as.integer(transformed_names))

# okay we now have this ....
merged_df <- inner_join(answers, study1_questions, by = "question_id")

### now we can do datasets ###


### now update all of the nan datasets ###
# NB: check that this works for more crazy datasets ...
update_child_questions <- function(df_wide, df_relationships) {
  updated_df <- df_wide
  
  update_descendants <- function(parent_id, affected_entries, updated_df) {
    children <- df_relationships %>%
      filter(parent_question_id == parent_id) %>%
      pull(question_id)
    
    for (child_id in children) {
      child_id_str <- paste0("X", child_id)  # Add the "X" prefix
      if (child_id_str %in% names(updated_df)) {
        updated_df[affected_entries, child_id_str] <- NA
      }
    }
    return(updated_df)  # Return the modified dataframe
  }
  
  root_questions <- df_relationships %>%
    filter(parent_question_id == 0) %>%
    pull(question_id)
  
  for (question_id in root_questions) {
    question_id_str <- paste0("X", question_id)  # Add the "X" prefix
    if (question_id_str %in% names(updated_df)) {
      affected_entries <- is.na(updated_df[[question_id_str]])
      updated_df <- update_descendants(question_id, affected_entries, updated_df)  # Capture the returned, modified df
    }
  }
  
  return(updated_df)
}

nan_file <- read_csv("Imputation/output/study1/additional_NA/NA_MAR_20_1.csv")
nan_file <- update_child_questions(nan_file, merged_df)

### generate datasets ###
### generate datasets ###
generate_datasets_ids_string <- function(df, max_level) {
  datasets_ids <- list() # Use a list to store IDs strings keyed by level
  
  # For level 1, simply take the question IDs
  # Ensure we always return a data frame with drop = FALSE
  question_ids_level_1 <- df[df$question_level == 1, "question_id", drop = FALSE]
  # Apply paste0 to each element of the column
  datasets_ids[[1]] <- paste0("X", question_ids_level_1$question_id)
  
  encountered_parents <- numeric(0) # Initialize an empty numeric vector for encountered parents
  for (i in 2:max_level) {
    level_i_questions <- df[df$question_level == i, "question_id", drop = FALSE]
    
    # Find the parents of level i questions
    parents_of_level_i <- unique(df[df$question_id %in% level_i_questions$question_id, "parent_question_id", drop = FALSE])
    
    # Add to list of encountered parents
    encountered_parents <- unique(c(encountered_parents, parents_of_level_i$parent_question_id))
    
    # For the second and subsequent datasets, include question IDs not in encountered_parents and up to current level
    # Ensure we always return a data frame with drop = FALSE
    question_ids_level_i <- df[!df$question_id %in% encountered_parents & df$question_level <= i, "question_id", drop = FALSE]
    # Apply paste0 to each element of the column
    datasets_ids[[i]] <- paste0("X", question_ids_level_i$question_id)
  }
  
  return(datasets_ids)
}
dataset_id_strings <- generate_datasets_ids_string(merged_df, max_level = max(merged_df$question_level))



dataset_id_strings


# okay try to actually run some of this process #
source('Imputation/project_support.R')
library(missForest)
library(tidyverse)

# path to output folder
modified_folder_path <- "Imputation/output/study1/missForest"
if (!dir.exists(modified_folder_path)) {
  dir.create(modified_folder_path)
}

# path to input folder 
folder_path <- "Imputation/output/study1/additional_NA"
file_names <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

data_missing <- list()
for (file_path in file_names) {
  file_name <- basename(file_path)
  data <- read.csv(file_path)
  file_id <- sub(".*/NA_(.*)\\.csv", "\\1", file_path)
  data_missing[[file_id]] <- data
}

# double check this, but seems right
updated_data_missing <- lapply(data_missing, function(df) {
  update_child_questions(df, merged_df)
})


# Questions has the correct data types 
questions <- read_csv("Data/Raw/drh_v6_poll.csv") 

# Create index of variable types for GLRM and conversion of data type
var_types <- lapply(updated_data_missing, variables_types)

# Convert variables to correct class
data_class <- Map(correct_class, updated_data_missing, var_types)

### now we need to subset the dataframe ###
subset_dataframes <- function(dataframes_list, columns_to_keep) {
  
  # Function to subset a single dataframe based on the criteria
  subset_dataframe <- function(df) {
    # Identify columns that do not start with "X"
    non_x_columns <- names(df)[!grepl("^X", names(df))]
    
    # From the columns that start with "X", keep those that are in columns_to_keep
    x_columns_in_df <- intersect(names(df)[grepl("^X", names(df))], columns_to_keep)
    
    # Combine the non-X columns with the allowed X columns and subset the dataframe
    columns_to_subset <- c(non_x_columns, x_columns_in_df)
    return(df[, columns_to_subset, drop = FALSE])
  }
  
  # Apply the subsetting function to each dataframe in the list
  modified_dataframes_list <- lapply(dataframes_list, subset_dataframe)
  
  return(modified_dataframes_list)
}


imputed_data_class <- data_class

# Loop through each stage
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

# now we can save this ...