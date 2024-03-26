library(tidyverse)

mice_impute <- function(data, seed) {
  MICE_data <- data
  # Convert Region to numeric as MICE performs poorly with too many factors (VMP: wait, what?)
  MICE_data <- MICE_data %>% 
    mutate(region = as.numeric(as.character(region))) %>%
    select(-ID)
  
  colnames(MICE_data) <- paste0("x_", colnames(MICE_data))
  MICE_imp <- mice::mice(MICE_data, print = FALSE, seed = seed, nnet.MaxNWts = 10000, remove.collinear=FALSE, remove.constant=FALSE) #remove.constant=FALSE, remove.collinear=FALSE)
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

#### CONSTANT and COLLINEARITY ####
seed <- 658
data_missing <- read_csv("output/study1/additional_NA/NA_MCAR_60_3.csv")

# here we get 7 columns without imputations (if we do not fix it)
questions <- read_csv("data/drh_v6_poll.csv")
var_types <- variables_types(data_missing)
data_class <- correct_class(data_missing, var_types)

#imp <- mice_impute(data_class, seed=seed)
#imp <- mice::complete(imp)

# 7 cols with nan (does not change): (TRUE just silences the error message)
#head(imp$loggedEvents, 20)
#data_missing

#### automatically imputing with constant values ####
MICE_data <- data_class
MICE_data <- MICE_data %>% 
  mutate(region = as.numeric(as.character(region))) %>%
  select(-ID)
colnames(MICE_data) <- paste0("x_", colnames(MICE_data))
MICE_imp <- mice::mice(MICE_data, print = FALSE, seed = seed, nnet.MaxNWts = 10000, remove_constant=FALSE, remove_collinear=FALSE)
test <- mice::complete(MICE_imp)
head(MICE_imp$loggedEvents, 20) # shows us the collinear and the constant columns 
test

## check whether we always get constant out for constant
unique(test["x_4776"]) # 1
unique(test["x_4654"]) # 1
unique(test["x_5132"]) # 0

## check what we get out for collinear columns
# what are the collinear ones?: 4729, 4954, 4794, 4730
column_names <- c("x_4729", "x_4954", "x_4794", "x_4730")
df_numeric <- sapply(test[column_names], as.numeric)
cor_matrix <- cor(df_numeric, use="complete.obs", method="pearson")
# does not seem like it is just always predicting the same.
# we should actually try to implement this I believe.
# but that would be going back to the original data then.

#### manually imputing with constant values ####
# check which variables are constant. 
# (NB: to handle this such that we do not always get the same value you would need priors because no information from other columns can be informative)
check_constant_columns <- function(df) { 
  sapply(df, function(x)  {
    x_non_na <- na.omit(x)
    length(unique(x_non_na)) == 1
  })
}
constant_columns <- check_constant_columns(data_missing)
constant_column_names <- names(constant_columns)[constant_columns]
print(constant_column_names) # constant columns: 4776, 4654, 5132

# now remove the NA values from the constant columns
data_non_constant <- data_missing # create copy 
for (col_name in constant_column_names) {
  constant_value <- unique(na.omit(data_non_constant[[col_name]]))
  if (length(constant_value)==1) {
    data_non_constant[[col_name]][is.na(data_non_constant[[col_name]])] <- constant_value
  } else {
    warning(paste("Column", col_name, "has more than one unique non-NA value."))
  }
}

# okay, now put this through MICE again (but without these columns)
data_class <- correct_class(data_non_constant, var_types)
typeof(data_class) # unclear to me what tells R what to impute...?
imp <- mice_impute(data_class, seed=seed)
imp <- mice::complete(imp) # now we get 4 columns without imputations (4729, 4954, 4794, 4730)
head(imp$loggedEvents, 20)

MICE_data <- data_class
# Convert Region to numeric as MICE performs poorly with too many factors (VMP: wait, what?)
MICE_data <- MICE_data %>% 
  mutate(region = as.numeric(as.character(region))) %>%
  select(-ID)
colnames(MICE_data) <- paste0("x_", colnames(MICE_data))
MICE_imp <- mice::mice(MICE_data, print = FALSE, seed = seed, nnet.MaxNWts = 10000, remove_constant=FALSE, remove_collinear=FALSE)
test <- mice::complete(MICE_imp)
head(MICE_imp$loggedEvents, 20)
test
# fill these with the most common value:
# now do we still have a problem?


# okay, so we still have perfect collinearity.
# this we can also fix manually, I guess.



#### how it is supposed to be used ####
# https://stefvanbuuren.name/RECAPworkshop/Practicals/RECAP_Practical_II.html
fit <- with(imp, lm(x_4983 ~ x_4794 + x_4821))
# okay, so as default we get multiple imputations (n=5)
# we are just extracting the first data set here (first iteration).
# https://stackoverflow.com/questions/51370292/what-exactly-does-complete-in-mice-do
# you can use "long" format to get out multiple imputations from MICE. 
est <- mice::pool(fit)
coef(fit$analyses[[3]]) # they are different; should account for this... seems to be the whole point of MICE. 
# only a problem in 4654 somehow. 
# maybe because we only have observations of (1). 
# check the full data






#### original test ####
d_test <- read_csv("output/study1_original/additional_NA/NA_MCAR_30_3.csv")

rows_to_keep <- !apply(d_test[, c("4827", "4745", "4780", "4776", "4654", "4828", "4729", "4954", "4794", "5226", "4676", "4983", "5132", "4730", "4821")], 1, function(x) any(x == 2, na.rm = TRUE))
new_df <- d_test[rows_to_keep, ]
var_types <- variables_types(new_df)
data_class <- correct_class(new_df, var_types)
output <- mice_impute(data_class, seed=seed)
output
head(output$loggedEvents, 20)

# okay so something with e.g. 4780 and 4776
# okay, so I think we have the culprit...
# it drops predictors in these cases. 
for (i in 1:nrow(new_df)) {
  # Use %s as the format specifier to treat everything as strings
  tuple_str <- sprintf("(%s, %s)", as.character(new_df[i, "4780"]), as.character(new_df[i, "4776"]))
  
  # Replace NA character representation with NaN if necessary
  tuple_str <- gsub("NA", "NaN", tuple_str)
  
  cat(tuple_str, "\n")
}


# errors on 4780 which has 0, 1, 2: so here probably it is using
# predictive mean matching.