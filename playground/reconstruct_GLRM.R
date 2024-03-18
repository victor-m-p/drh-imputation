library(tidyverse)
library(mice)
library(missForest)
library(VIM)
library(missMDA)
library(h2o)
library(Metrics)
library(devtools)
library(ggpubr)
library(ggrepel)

variables_types <- function(data){
  var_types <- data.frame(ID = colnames(data)) %>%
    mutate(`Question ID` = gsub("_.*$","", ID)) %>%
    mutate(`Question ID` = gsub("[^0-9.-]", "", `Question ID`)) %>%
    mutate(`Question ID` = as.numeric(`Question ID`)) %>%
    left_join(questions) %>%
    mutate(loss_func = case_when(ID == "ID" ~ "Categorical",
                                 ID == "elite" ~ "Categorical",
                                 ID == "non_elite" ~ "Categorical",
                                 ID == "religious_specialist" ~ "Categorical",
                                 ID == "start_date" ~ "Absolute",
                                 ID == "end_date" ~ "Absolute",
                                 ID == "region" ~ "Categorical",
                                 `Data Type` == "Nominal" ~ "Categorical",
                                 `Data Type` == "Nominal - Other"  ~ "Categorical",
                                 `Data Type` == "Nominal - Multiple"  ~ "Categorical",
                                 `Data Type` == "Discrete" ~ "Absolute",
                                 `Data Type` == "Continuous" ~ "Absolute"))
}


replace_branching <- function(data){
  data <- data %>%
    rename("ID" = `Entry ID`, start_date = `Start Date`, end_date = `End Date`, region = `Region ID`) %>%
    mutate(elite = ifelse(grepl("E", `Branching question`), "1", "0")) %>%
    mutate(non_elite = ifelse(grepl("N", `Branching question`), "1", "0")) %>%
    mutate(religious_specialist = ifelse(grepl("R", `Branching question`), "1", "0")) %>%
    select(-`Branching question`) %>%
    select(ID, elite, non_elite, religious_specialist, everything()) 
}

correct_class <- function(data, var_types) {
  continuous <- filter(var_types, `Data Type` == "Continuous")
  discrete <- filter(var_types, `Data Type` == "Discrete")
  nominal <- filter(var_types, `Data Type` == "Nominal" | `Data Type` == "Nominal - Multiple" | `Data Type` == "Nominal - Other")
  data <- data %>%
      mutate(across(ID:religious_specialist, factor)) %>%
      mutate(across(start_date:end_date, as.integer)) %>%
      mutate(region = factor(region)) %>%
      mutate(across(nominal$ID, factor)) %>%
      mutate(across(continuous$ID, as.numeric)) %>%
      mutate(across(discrete$ID, as.integer))
  data <- as.data.frame(data)
}

glrm_impute <- function(data_missing, data_validation = NULL, var_types, GLRM_k, seed) {
  if(!is.null(data_validation)) {
  validation_data <- as.h2o(data_validation)
  }
  GLRM_data <- as.h2o(data_missing)
  GLRM_model <- h2o.glrm(training_frame = GLRM_data, validation_frame = , cols = 1:ncol(GLRM_data), k = GLRM_k,
                         loss = "Absolute", transform = "None", regularization_x = "None", regularization_y = "None",
                         multi_loss = "Categorical", loss_by_col = var_types$loss_func, loss_by_col_idx = c(0:(ncol(GLRM_data)-1)),
                         recover_svd = TRUE, seed = 5, ignore_const_cols = FALSE, max_iterations = 1000)
  GLRM_imputed <- as.data.frame(h2o.predict(GLRM_model, GLRM_data)) 
  GLRM_imputed <- GLRM_imputed %>%
    rename_all(~ sub("reconstr_", "", names(GLRM_imputed)))
}

# Add missing data
# data_missing <- add_NA(data, study, missing_pattern = missing_pattern, missing_prop = missing_prop)
data <- read_csv("output/study1/complete_cases/complete_cases.csv")
data_missing <- read_csv("output/study1/additional_NA/NA_MAR_10_1.csv")
questions <- read_csv("data/drh_v6_poll.csv")
var_types <- variables_types(data_missing)
data_class <- correct_class(data_missing, var_types)

# Calculate missing percentage
missing_per = 0.1 * 100

# Replace branching questions with separate variables for each group of people
data <- replace_branching(data)

#data_complete_class <- correct_class(data, var_types[[1]]) # must use the raw complete
data_complete_class <- data # think this already happened 

# Change ordered to factor as H2O will not accepted ordered class
data_complete_class <- data_complete_class %>% mutate(across(where(is.ordered), ~ factor(., ordered = FALSE)))


data_class_glrm <- data_class %>% mutate(across(where(is.ordered), ~ factor(., ordered = FALSE)))

h2o.init()

GLRM_data <- as.h2o(data_class_glrm)
# why is loss absolute??
# why do we not have regularization?? (no regularization will overfit, do we want that?)
# okay it is just the purpose that this actually reconstructs the data; sure. 
# however; why do we just get always 1 in some columns??
# ahhh; maybe we also get that for other columns, but we just have to specifically
# fix this one I think...
# K really important: we do seem to get lowest SSE for k=8
GLRM_model <- h2o.glrm(training_frame = GLRM_data, validation_frame = , cols = 1:ncol(GLRM_data), k = 8,
                         loss = "Absolute", transform = "None", regularization_x = "None", regularization_y = "None",
                         multi_loss = "Categorical", loss_by_col = var_types$loss_func, loss_by_col_idx = c(0:(ncol(GLRM_data)-1)),
                         recover_svd = TRUE, seed = 5, ignore_const_cols = FALSE, max_iterations = 1000)
GLRM_imputed <- as.data.frame(h2o.predict(GLRM_model, GLRM_data)) 
GLRM_data
h2o.predict(GLRM_model, GLRM_data)
GLRM_data
GLRM_imputed
GLRM_imputed <- GLRM_imputed %>%
    rename_all(~ sub("reconstr_", "", names(GLRM_imputed)))
var_types
GLRM_imputed
GLRM_model
summary(GLRM_model)
glrm_imp <- glrm_impute(data_missing=data_class_glrm, data_validation=data_complete_class, var_types=var_types[[1]], GLRM_k=8, seed=658)

x <- h2o::h2o.reconstruct(GLRM_model, GLRM_data, reverse_transform = TRUE)


# archetypes
new_data = as.data.frame(h2o::h2o.getFrame(GLRM_model@model$representation_name))
(variances = sapply(new_data, stats::var))

GLRM_model@model$archetypes


# GLRM imputation
glrm_imputation <- possibly(glrm_impute, otherwise = "The algorithm failed to converge")
if(study == 1){
glrm_imp <- lapply(data_class_glrm, function(x) glrm_imputation(data_missing = x, data_validation = data_complete_class, var_types = var_types[[1]], GLRM_k = GLRM_k, seed = seed))
} else if(study == 2){
glrm_imp <- lapply(data_class_glrm, function(x) glrm_imputation(data_missing = x, var_types = var_types[[1]], GLRM_k = GLRM_k, seed = seed))
}
names(glrm_imp) <- seq(1:length(glrm_imp))

# Extract run imputations
glrm_imp_run <- Filter(function(x) length(x) > 1, glrm_imp)

# Extract imputations that didn't run
glrm_imp_not_run <- Filter(function(x) length(x) == 1, glrm_imp)