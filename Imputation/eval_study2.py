import pandas as pd
import numpy as np

import os
import re
import warnings

warnings.filterwarnings("ignore")  # this makes sense here for "precision" warning
from evaluation_functions import calculate_metrics

### run simple evaluation ###

# paths to nan files
nan_path = f"output/study2/additional_NA"
nan_files = os.listdir(nan_path)

# paths to match the imputed data
data_path = "output/study2"
method_grid = [
    ("missForest", "mf"),
    ("mice", "mice"),
    ("mode", "mode"),
]

# get the complete data and columns
df_complete = pd.read_csv("../Data/Preprocessed/answers_study2.csv")
question_columns = [col for col in df_complete.columns if col.startswith("X")]
df_complete = df_complete[question_columns]

data_path = "output/study2"
data_list = []


for nan_file in nan_files:
    # get metadata
    missing_type, missing_percent, iter = re.match(
        r"NA_(MCAR|MNAR|MAR)_(\d+)_(\d+).csv", nan_file
    ).groups()

    # load nan file
    df_nan = pd.read_csv(os.path.join(nan_path, nan_file))

    # loop over methods
    for method in method_grid:
        # load methods
        path_name, file_prepend = method
        filename = f"{file_prepend}_{missing_type}_{missing_percent}_{iter}.csv"
        df_impute = pd.read_csv(os.path.join(data_path, path_name, filename))

        # loop over columns
        for question_column in question_columns:
            # where we have nan in additional nan
            df_nan_c = df_nan[question_column]
            mat_nan_c = df_nan_c.to_numpy()
            mask_nan_c = np.isnan(mat_nan_c)
            # where we do not have nan in original data
            df_complete_c = df_complete[question_column]
            mat_complete_c = df_complete_c.to_numpy()
            mask_complete_c = ~np.isnan(mat_complete_c)
            # combine the masks (i.e., where we have added nan, but not nan in original)
            combined_mask_c = mask_nan_c & mask_complete_c
            # get imputed values
            df_impute_c = df_impute[question_column]
            mat_impute_c = df_impute_c.to_numpy()
            # get y_pred and y_true
            y_pred = mat_impute_c[combined_mask_c]
            y_true = mat_complete_c[combined_mask_c]
            rmse, mpb, accuracy, precision, recall, f1, matthews_corr = (
                calculate_metrics(y_true, y_pred)
            )
            data_list.append(
                [
                    question_column,
                    path_name,
                    missing_type,
                    missing_percent,
                    iter,
                    rmse,
                    mpb,
                    accuracy,
                    precision,
                    recall,
                    f1,
                    matthews_corr,
                ],
            )
    df_metrics = pd.DataFrame(
        data_list,
        columns=[
            "question",
            "method",
            "type",
            "percent",
            "iter",
            "rmse",
            "mpb",
            "accuracy",
            "precision",
            "recall",
            "f1",
            "matthews_corr",
        ],
    )
    df_metrics["iter"] = df_metrics["iter"].astype(int)
    df_metrics = df_metrics.sort_values(by=["method", "type", "percent", "iter"])
    df_metrics.to_csv("evaluation/metrics_study2.csv", index=False)
