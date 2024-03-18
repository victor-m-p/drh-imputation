import numpy as np
import pandas as pd
import os
import re


def filter_dataframe(df, columns):
    """
    Preprocess the data by filtering out non-binary columns and rows with missing values.
    """
    df = df[columns]
    filtered_df = df[
        df.select_dtypes(include=["number"]).applymap(lambda x: x in [0, 1]).all(axis=1)
    ]
    return filtered_df


def pairwise_metrics(original, imputed):
    """
    Calculate metrics for the difference between the original and imputed data.
    """
    # Get the difference matrix
    diff_matrix = original - imputed
    diff_matrix = diff_matrix.dropna()

    # Calculate the Frobenius norm for the difference (excluding diagonal and duplicates)
    frobenius_norm = np.sqrt(np.sum(diff_matrix**2))

    # Calculate the average squared difference
    avg_squared_difference = np.mean(diff_matrix**2)

    # Calculate MAE
    mae = np.mean(np.abs(diff_matrix))

    return frobenius_norm, avg_squared_difference, mae


# only looking at binary for now
from variables import columns, method_grid

# load and process original data
original_data = pd.read_csv("../output/study1/complete_cases/complete_cases.csv")
original_filter = filter_dataframe(original_data, columns)
original_corr = original_filter.corr()
mask = np.triu(np.ones_like(original_corr, dtype=bool))
original_flat = original_corr.mask(mask).stack()
complete_length = len(original_flat)
pattern = r"^(.*?)_(MCAR|MAR|MNAR)_(\d+)_(\d+)\..*?(?:csv|txt)$"
data_path = "../output/study1"
method_grid = [x for x, y in method_grid]
for method in method_grid:
    files = os.listdir(os.path.join(data_path, method))
    evaluation_list = []
    for file in files:
        print(file)
        # get the type, percent and iter
        _, type, percent, iter = re.match(pattern, file).groups()
        iter = iter.split(".")[0]
        print(type, percent, iter)
        # load the imputed data
        inpath_imputed = os.path.join(data_path, method, file)
        imputed_data = pd.read_csv(inpath_imputed)
        imputed_filter = filter_dataframe(imputed_data, columns)
        imputed_corr = imputed_filter.corr()
        imputed_flat = imputed_corr.mask(mask).stack()
        print(len(imputed_flat))
        # check whether we have nan
        complete_data = len(imputed_flat) == complete_length
        # calculate pairwise metrics
        frobenius, avg_squared, mae = pairwise_metrics(original_flat, imputed_flat)
        evaluation_list.append(
            [type, percent, iter, method, frobenius, avg_squared, mae, complete_data]
        )
    evaluation_df = pd.DataFrame(
        evaluation_list,
        columns=[
            "type",
            "percent",
            "iter",
            "method",
            "frobenius",
            "avg_squared",
            "mae",
            "complete_data",
        ],
    )
    evaluation_df["iter"] = evaluation_df["iter"].astype(int)
    evaluation_df = evaluation_df.sort_values(by=["type", "percent", "iter"])
    evaluation_df.to_csv("evaluation/{}_evaluation.csv".format(method), index=False)
