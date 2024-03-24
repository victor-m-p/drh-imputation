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
import warnings

warnings.filterwarnings("ignore")  # this makes sense here for "precision" warning


def calculate_metrics(y_true, y_pred):

    # Calculate Mean Percent Bias
    # NB: not quite MPB but this makes more sense
    mpb = np.abs(np.mean((y_true - y_pred) / 1)) * 100

    # inverse accuracy for just mse because 0/1
    rmse = np.sqrt(mean_squared_error(y_true, y_pred))

    # accuracy
    accuracy = accuracy_score(y_true, y_pred)

    # undefined (or meaningless) if all values are 0 or 1
    if np.all(y_true == 0) or np.all(y_true == 1):
        matthews_corr = np.nan
    else:
        matthews_corr = matthews_corrcoef(y_true, y_pred)

    # undefined (or meaningless) if all values are 0
    # consider whether this should also check all true == 1
    if np.all(y_true == 0):
        precision = np.nan
        recall = np.nan
        f1 = np.nan
    else:
        precision = precision_score(y_true, y_pred)
        recall = recall_score(y_true, y_pred)
        f1 = f1_score(y_true, y_pred)

    return rmse, mpb, accuracy, precision, recall, f1, matthews_corr


# paths to nan files
nan_path = f"output/study1/additional_NA"
nan_files = os.listdir(nan_path)

# paths to match the imputed data
data_path = "output/study1"
method_grid = [("missForest", "mf")]

# get the complete data and columns
df_complete = pd.read_csv("../Data/Preprocessed/answers_study1.csv")
question_columns = [col for col in df_complete.columns if col.startswith("X")]
df_complete = df_complete[question_columns]
matrix_complete = df_complete.to_numpy()

data_path = "output/study1"
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
            # print(missing_type, missing_percent, iter, question_column)
            # valid indices for the column
            df_nan_c = df_nan[question_column]
            valid_indices = df_nan_c.isna()
            valid_indices = valid_indices.to_numpy()
            # y_true for the column
            y_true = df_complete[question_column]
            y_true = y_true.to_numpy()
            y_true = y_true[valid_indices]
            # imputed for the column
            y_pred = df_impute[question_column]
            y_pred = y_pred.to_numpy()
            y_pred = y_pred[valid_indices]
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
    df_metrics.to_csv("evaluation/df_var_metrics.csv", index=False)
