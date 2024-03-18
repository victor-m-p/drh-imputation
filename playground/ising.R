library(IsingFit)
library(tidyverse)

setwd("/home/vmp/imputation_comparison/playground")
d_complete <- read_csv("../output/study1/complete_cases/complete_cases.csv")

d_complete |> head()
d_complete_binary <- d_complete |> select(-c("Entry ID", "Branching question", "Region ID", "Start Date", "End Date"))
res_complete <- IsingFit(d_complete_binary, family="binomial", progressbar=True) # too little variance. 
d_complete_subset <- d_complete_binary |> select(-c("4654", "4828", "4954", "4676"))
complete_subset_matrix <- as.matrix(d_complete_subset)
res_complete <- IsingFit(complete_subset_matrix, family="binomial", progressbar=True) # too little variance.

complete_subset_matrix
d_complete_subset

# we need to figure out what "2" is. 
# and also; does it treat this as a factor or numeric?

# the data removed for the smallest case (0.1)
d_01 <- 

# first normal form.
# make sure that we are making commits 
# retaining the structure: 
# in theory latent space in general: why does it work. 