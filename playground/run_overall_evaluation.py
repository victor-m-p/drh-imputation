import pandas as pd
import numpy as np
from sklearn.metrics import (
    accuracy_score,
    precision_score,
    recall_score,
    f1_score,
    mean_squared_error,
    matthews_corrcoef,
)
import os
import re


def calculate_metrics(y_true, y_pred):
    # Check whether there were any nan
    nan_in_y_pred = np.isnan(y_pred).any()

    # Take only the valid ones
    valid_indices = ~np.isnan(y_pred)

    # Apply this mask to both y_true and y_pred to exclude NaNs in y_pred
    y_true_filtered = y_true[valid_indices]
    y_pred_filtered = y_pred[valid_indices]

    # Now, you can safely calculate the metrics
    rmse = np.sqrt(mean_squared_error(y_true_filtered, y_pred_filtered))

    # For classification metrics, ensure y_true_filtered and y_pred_filtered are in a classification format (0 or 1)
    # If y_pred_filtered was generated as probabilities, you might need to convert these to 0 or 1 based on a threshold (e.g., 0.5)
    # This conversion is necessary for accuracy, precision, recall, and F1 calculations
    # Example conversion (adjust according to your needs):
    y_pred_classified = np.round(y_pred_filtered).astype(int)

    accuracy = accuracy_score(y_true_filtered, y_pred_classified)
    precision = precision_score(y_true_filtered, y_pred_classified)
    recall = recall_score(y_true_filtered, y_pred_classified)
    f1 = f1_score(y_true_filtered, y_pred_classified)
    matthews_corr = matthews_corrcoef(y_true_filtered, y_pred_classified)

    return rmse, accuracy, precision, recall, f1, matthews_corr, nan_in_y_pred


from variables import columns, method_grid

# load and process original data
df_complete = pd.read_csv("../output/study1/complete_cases/complete_cases.csv")
df_complete = df_complete[columns]
df_complete = df_complete.to_numpy()

# run over nan datasets
nan_path = "../output/study1/additional_NA"
nan_files = os.listdir(nan_path)
missing_type, missing_percent, iter = re.match(
    r"NA_(MCAR|MNAR|MAR)_(\d+)_(\d+).csv", nan_files[0]
).groups()

data_path = "../output/study1"
data_list = []
for nan_file in nan_files:
    # get metadata
    missing_type, missing_percent, iter = re.match(
        r"NA_(MCAR|MNAR|MAR)_(\d+)_(\d+).csv", nan_file
    ).groups()
    # load file
    df_nan = pd.read_csv(os.path.join(nan_path, nan_file))
    df_nan = df_nan[columns]
    # find nan indices
    valid_indices = df_nan.isna()
    valid_indices = valid_indices.to_numpy()
    # create true for this subset
    y_true = df_complete[valid_indices]
    # get the imputed
    for method in method_grid:
        path_name, file_prepend = method
        filename = f"{file_prepend}_{missing_type}_{missing_percent}_{iter}.csv"
        df_impute = pd.read_csv(os.path.join(data_path, path_name, filename))
        df_impute = df_impute[columns]
        df_impute = df_impute.to_numpy()
        y_pred = df_impute[valid_indices]
        rmse, accuracy, precision, recall, f1, matthews_corr, nan_in_y_pred = (
            calculate_metrics(y_true, y_pred)
        )
        data_list.append(
            [
                path_name,
                nan_in_y_pred,
                missing_type,
                missing_percent,
                iter,
                rmse,
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
        "method",
        "nan_in_y_pred",
        "type",
        "percent",
        "iter",
        "rmse",
        "accuracy",
        "precision",
        "recall",
        "f1",
        "matthews_corr",
    ],
)
df_metrics["iter"] = df_metrics["iter"].astype(int)
df_metrics = df_metrics.sort_values(by=["method", "type", "percent", "iter"])
df_metrics.to_csv("evaluation/df_metrics.csv", index=False)
