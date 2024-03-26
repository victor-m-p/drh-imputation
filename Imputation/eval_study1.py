import pandas as pd
import os
import warnings

warnings.filterwarnings("ignore")  # this makes sense here for "precision" warning
from evaluation_functions import (
    basic_metrics_study1,
    pairwise_metrics_study1,
    pairwise_correlations_study1,
    flag_constant_variables,
)

# paths to nan files
nan_path = f"output/study1/additional_NA"
nan_files = os.listdir(nan_path)

# paths to match the imputed data
data_path = "output/study1"
from constants import method_grid

# get the complete data and columns
df_complete = pd.read_csv("../Data/Preprocessed/answers_study1.csv")

# calculate basic metrics
df_metrics = basic_metrics_study1(method_grid, df_complete, nan_path, data_path)

# save basic metrics
df_metrics.to_csv("evaluation/metrics_study1.csv", index=False)

# calculate pairwise metrics
methods = [x for x, y in method_grid]

# only calculate pairwise metrics for the variables that are never constant
constant_columns = flag_constant_variables(methods, df_complete, data_path)

pairwise_metrics = pairwise_metrics_study1(
    methods, df_complete, data_path, constant_columns
)

pairwise_metrics.to_csv("evaluation/pairwise_study1.csv", index=False)

# run pairwise metrics
df_pairwise_var = pairwise_correlations_study1(
    methods, df_complete, data_path, constant_columns
)

df_pairwise_var.to_csv("evaluation/correlations_study1.csv", index=False)
