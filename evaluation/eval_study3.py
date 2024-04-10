import pandas as pd
import warnings
import os
import re
from evaluation_functions import flag_constant_variables
from itertools import combinations
import numpy as np

# from evaluation_functions import basic_metrics_study2
# from constants import method_grid

methods = ["mice"]
warnings.filterwarnings("ignore")  # this makes sense here for "precision" warning

# prepare function inputs
nan_path = f"../imputation/output/study1/additional_NA"
data_path = "../imputation/output/study3"
df_complete = pd.read_csv("../data/preprocessed/answers_study1.csv")

# figure out how well we are doing
question_columns = [col for col in df_complete.columns if col.startswith("X")]
df_complete = df_complete[question_columns]

nan_files = os.listdir(nan_path)
data_list = []

constant_variables = flag_constant_variables(methods, df_complete, data_path)

# get the columns that we calculate on
question_columns = [col for col in df_complete.columns if col.startswith("X")]
if constant_variables:
    question_columns = [
        col for col in question_columns if col not in constant_variables
    ]
column_pairs = list(combinations(question_columns, 2))

# pattern to match files
pattern = r"^(.*?)_(MCAR|MAR|MNAR)_(\d+)_(\d+)\..*?(?:csv|txt)$"
evaluation_list = []
for method in methods:
    files = os.listdir(os.path.join(data_path, method))

    for file in files:
        # get the type, percent and iter
        _, type, percent, iter = re.match(pattern, file).groups()
        iter = iter.split(".")[0]
        # load the imputed data
        inpath_imputed = os.path.join(data_path, method, file)
        imputed_data = pd.read_csv(inpath_imputed)
        for column_pair in column_pairs:
            column_x, column_y = column_pair
            complete_filter = df_complete[[column_x, column_y]]
            complete_corr = complete_filter.corr()
            complete_np = complete_corr.to_numpy()
            complete_value = complete_np[np.triu_indices(2, k=1)][0]

            imputed_filter = imputed_data[[column_x, column_y]]
            imputed_corr = imputed_filter.corr()
            imputed_np = imputed_corr.to_numpy()
            imputed_value = imputed_np[np.triu_indices(2, k=1)][0]
            evaluation_list.append(
                [
                    type,
                    percent,
                    iter,
                    method,
                    column_x,
                    column_y,
                    complete_value,
                    imputed_value,
                ]
            )
evaluation_df = pd.DataFrame(
    evaluation_list,
    columns=[
        "Type",
        "Percent",
        "Iter",
        "Method",
        "var_x",
        "var_y",
        "corr_complete",
        "corr_imputed",
    ],
)
evaluation_df["Iter"] = evaluation_df["Iter"].astype(int)
evaluation_df = evaluation_df.sort_values(by=["Type", "Percent", "Iter"])
