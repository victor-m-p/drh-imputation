# Run imputation on complete cases dataset

# Load functions
#getwd()
#setwd("/home/vmp/imputation_comparison/02_study_1_complete_cases")
source("../project_support.R")

extract_components <- function(file_path) {
  lines <- readLines(file_path)
  first_line <- lines[2]
  numbers_in_line <- regmatches(first_line, gregexpr("[[:digit:]]+", first_line))
  num_of_components <- as.numeric(numbers_in_line[[1]][1])
  return(num_of_components)
}

# Load data
#data <- read_csv("../data/drh_t.csv")
questions <- read_csv("../data/drh_v6_poll.csv") 

# Select only complete cases of the 15 most answered questions
# Almost no data here; we need to find the "good" cases earlier...
#data_complete <- complete_cases_filter(data, 15)
data_complete <- read_csv('../data/drh_t_binary_super.csv')

# Save complete cases data
if(dir.exists(file.path("../output/study1/complete_cases/")) == FALSE) {
  dir.create(file.path("../output/study1/complete_cases/"))
}

write_csv(data_complete, paste0("../output/study1/complete_cases/complete_cases.csv"))

# Initiate h2o
h2o.init()

# Run imputation
# do not understand why the k parameter is hard coded here?
missing_prop <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
missing_types <- c("MCAR", "MAR", "MNAR")

# Loop over each missing proportion
for (prop in missing_prop) {
  # Inside that loop, loop over each missing type
  for (type in missing_types) {
    kNN_k <- extract_components(paste0("../output/study1/k_value/kNN/", type, "_", prop*100, "_kNN_k_value.txt"))
    GLRM_k <- extract_components(paste0("../output/study1/k_value/GLRM/", type, "_", prop*100, "_GLRM_k_value.txt"))
    FAMD_c <- extract_components(paste0("../output/study1/c_value/FAMD/", type, "_", prop*100, "_FAMD_c_value.txt"))
    run_imputation(data_complete, study = 1, missing_pattern = type, missing_prop = prop, kNN_k = kNN_k, GLRM_k = GLRM_k, FAMD_c = FAMD_c, seed = 658)
  }
}

#run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.1, kNN_k = 6, GLRM_k = 6, FAMD_c = 7, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.2, kNN_k = 7, GLRM_k = 10, FAMD_c = 3, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.3, kNN_k = 11, GLRM_k = 11, FAMD_c = 7, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.4, kNN_k = 9, GLRM_k = 10, FAMD_c = 3, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.5, kNN_k = 10, GLRM_k = 8, FAMD_c = 8, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.6, kNN_k = 5, GLRM_k = 11, FAMD_c = 2, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.7, kNN_k = 10, GLRM_k = 8, FAMD_c = 2, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MCAR", missing_prop = 0.8, kNN_k = 5, GLRM_k = 9, FAMD_c = 2, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.1, kNN_k = 8, GLRM_k = 8, FAMD_c = 6, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.2, kNN_k = 11, GLRM_k = 9, FAMD_c = 8, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.3, kNN_k = 11, GLRM_k = 9, FAMD_c = 2, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.4, kNN_k = 11, GLRM_k = 11, FAMD_c = 6, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.5, kNN_k = 11, GLRM_k = 8, FAMD_c = 6, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.6, kNN_k = 5, GLRM_k = 10, FAMD_c = 2, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.7, kNN_k = 7, GLRM_k = 9, FAMD_c = 2, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MAR", missing_prop = 0.8, kNN_k = 11, GLRM_k = 7, FAMD_c = 2, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.1, kNN_k = 11, GLRM_k = 9, FAMD_c = 6, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.2, kNN_k = 7, GLRM_k = 8, FAMD_c = 11, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.3, kNN_k = 10, GLRM_k = 9, FAMD_c = 11, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.4, kNN_k = 10, GLRM_k = 11, FAMD_c = 4, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.5, kNN_k = 9, GLRM_k = 6, FAMD_c = 8, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.6, kNN_k = 11, GLRM_k = 5, FAMD_c = 10, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.7, kNN_k = 7, GLRM_k = 7, FAMD_c = 7, seed = 658)
#run_imputation(data_complete, study = 1, missing_pattern = "MNAR", missing_prop = 0.8, kNN_k = 9, GLRM_k = 5, FAMD_c = 2, seed = 658)

# Shutdown h2o    
h2o.shutdown(prompt=FALSE)