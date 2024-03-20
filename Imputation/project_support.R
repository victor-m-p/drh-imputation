library(tidyverse)
library(stringr)

# Create index of variable types for GLRM and conversion of data type
variables_types <- function(data){
  var_types <- data.frame(ID = colnames(data)) %>%
    mutate(`Question ID` = gsub("_.*$","", ID)) %>%
    mutate(`Question ID` = gsub("[^0-9.]", "", `Question ID`)) %>% #[^0-9.-]
    mutate(`Question ID` = as.numeric(`Question ID`)) %>%
    left_join(questions) %>%
    mutate(loss_func = case_when(
        ID == "entry_id" ~ "Categorical",
        ID %in% c("year_from", "year_to", "log_area") ~ "Absolute",
        str_detect(ID, "^region_") ~ "Categorical",  # Match any ID starting with "region_"
        `Data Type` %in% c("Nominal", "Nominal - Other", "Nominal - Multiple") ~ "Categorical",
        `Data Type` %in% c("Discrete", "Continuous") ~ "Absolute")
    )
}

# Convert variables to correct class
correct_class <- function(data, var_types) {
  continuous <- filter(var_types, `Data Type` == "Continuous")
  discrete <- filter(var_types, `Data Type` == "Discrete")
  nominal <- filter(var_types, `Data Type` == "Nominal" | `Data Type` == "Nominal - Multiple" | `Data Type` == "Nominal - Other")
  data <- data %>%
      mutate(entry_id = factor(entry_id)) %>%
      mutate(across(year_from:year_to, as.integer)) %>%
      mutate(log_area = as.numeric(log_area)) %>%
      mutate(across(region_africa:region_southwest_asia, factor)) %>%
      mutate(across(nominal$ID, factor)) %>%
      mutate(across(continuous$ID, as.numeric)) %>%
      mutate(across(discrete$ID, as.integer))
  data <- as.data.frame(data)
}


# RF impute
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

# Save files
write_imputations <- function(data, study, algorithm, appendix) {
  # Create the base directory based on the study number
  base_dir <- paste0("output/study", study)
  
  # Create the algorithm-specific directory if it doesn't exist
  algo_dir <- file.path(base_dir, algorithm)
  if (!dir.exists(algo_dir)) {
    dir.create(algo_dir, recursive = TRUE)
  }

  # Use mapply to write each element of the data list to its own CSV file
  mapply(write_csv, data, paste0(algo_dir, "/", appendix, "_", names(data), '.csv'))
}


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