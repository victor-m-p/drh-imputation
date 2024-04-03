import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os
import re
from sklearn.metrics import (
    accuracy_score,
    precision_score,
    recall_score,
    f1_score,
    mean_squared_error,
    matthews_corrcoef,
)
from itertools import combinations


def single_lineplot(
    df,
    metric,
    hue,
    figsize=(8, 5),
    alpha=1,
    ncol_legend=None,
    outpath=None,
    outname=None,
):
    # basic plot setup
    fig, ax = plt.subplots(figsize=figsize)
    fig.patch.set_facecolor("white")

    # group by Percent and hue, average metric
    df_grouped = df.groupby(["Percent", hue])[metric].mean().reset_index(name="average")

    # line-plot
    sns.lineplot(
        data=df_grouped, x="Percent", y="average", hue=hue, alpha=alpha, marker="o"
    )

    # aesthetics
    plt.legend().remove()
    plt.ylabel(f"Average {metric}", fontsize=12)
    plt.xlabel("Percent Missing", fontsize=12)
    x_ticks = df["Percent"].unique()
    plt.xticks(x_ticks)
    plt.tight_layout()

    # add legend
    handles, labels = ax.get_legend_handles_labels()
    ncol = ncol_legend if ncol_legend else len(handles)
    nrow = np.ceil(len(df[hue].unique()) / ncol)
    y_align_legend = -0.08 * nrow
    fig.legend(
        handles,
        labels,
        loc="lower center",
        bbox_to_anchor=(0.5, y_align_legend),
        ncol=ncol,
        frameon=False,
        fontsize=12,
    )

    plt.subplots_adjust(left=0.15, bottom=0.1)

    # save or show
    if outpath:
        plt.savefig(os.path.join(outpath, outname), dpi=300, bbox_inches="tight")
    else:
        plt.show()


def multiple_lineplots(
    df,
    metric,
    hue,
    grid,
    legend=True,
    figsize=(7, 7),
    alpha=1,
    color=None,
    sharey="none",
    ncol_legend=None,
    outpath=None,
    outname=None,
):
    unique_grid_values = df[grid].unique()
    n_subplots = len(unique_grid_values)
    fig, ax = plt.subplots(n_subplots, 1, figsize=figsize, sharex=True)
    fig.patch.set_facecolor("white")

    # Initialize variables to determine global y-axis limits if needed
    global_ymin = float("inf")
    global_ymax = float("-inf")

    for num, ele in enumerate(unique_grid_values):
        # Select the subset of data for the current grid value
        df_subset = df[df[grid] == ele]

        # Determine unique hue values for the legend if needed
        hue_values = df_subset[hue].unique()

        for hue_value in hue_values:
            # Subset the data for each hue value
            df_grouped = (
                df_subset[df_subset[hue] == hue_value]
                .groupby(["Percent"])[metric]
                .mean()
                .reset_index(name="average")
            )

            # Plotting with specified or default color
            sns.lineplot(
                data=df_grouped,
                x="Percent",
                y="average",
                label=hue_value if legend else None,
                color=color if color else None,
                alpha=alpha,
                marker="o",
                ax=ax[num],
            )

            # Update global y-axis limits
            ymin, ymax = df_grouped["average"].min(), df_grouped["average"].max()
            global_ymin, global_ymax = min(global_ymin, ymin), max(global_ymax, ymax)

        # Remove the legend from individual plots
        ax[num].legend().remove()
        ax[num].set_ylabel("")
        ax[num].set_title(f"{metric} ({grid} = {ele})")
        ax[num].set_xlabel("Percent Missing", fontsize=12)
        x_ticks = df_subset["Percent"].unique()
        ax[num].set_xticks(x_ticks)

    # If sharey='all', set the same y-axis limits for all subplots
    global_ymin = global_ymin - global_ymax * 0.05
    global_ymax = global_ymax + global_ymax * 0.05
    if sharey == "all":
        for a in ax:
            a.set_ylim(global_ymin, global_ymax)

    # Set a global y-axis label for the entire figure instead of each subplot
    fig.text(
        0.05, 0.5, "Average " + metric, va="center", rotation="vertical", fontsize=12
    )

    # Only add the legend if specified
    plt.tight_layout()
    if legend:
        handles, labels = ax[0].get_legend_handles_labels()
        ncol = ncol_legend if ncol_legend else len(handles)
        nrow = int(np.ceil(n_subplots / ncol))
        y_align_legend = -0.08 * nrow
        fig.legend(
            handles,
            labels,
            loc="lower center",
            bbox_to_anchor=(0.5, y_align_legend),
            ncol=ncol,
            frameon=False,
            fontsize=12,
        )

    plt.subplots_adjust(left=0.15, bottom=0.1)

    # save or sjow
    if outpath:
        plt.savefig(os.path.join(outpath, outname), dpi=300, bbox_inches="tight")
    else:
        plt.show()


# consider whether we are actually going to need this
# currently we are not using this.
def pairwise_metrics(original, imputed):
    """
    Calculate metrics for the difference between the original and imputed data.
    """
    # Get the difference matrix
    diff_matrix = original - imputed
    # diff_matrix = diff_matrix.dropna()

    # Calculate the Frobenius norm for the difference (excluding diagonal and duplicates)
    frobenius_norm = np.sqrt(np.sum(diff_matrix**2))

    # Calculate the average squared difference
    avg_squared_difference = np.mean(diff_matrix**2)

    # Calculate MAE
    mae = np.mean(np.abs(diff_matrix))

    return frobenius_norm, avg_squared_difference, mae


# calculate a host of metrics.
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


# calculates metrics within variables
def basic_metrics_study1(method_grid, df_complete, nan_path, data_path):
    """
    calculate metrics by:
    - variable
    - method
    - condition (missing type, missing percent, iteration)
    """
    question_columns = [col for col in df_complete.columns if col.startswith("X")]
    df_complete = df_complete[question_columns]

    nan_files = os.listdir(nan_path)
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
            "Question",
            "Method",
            "Type",
            "Percent",
            "Iter",
            "RMSE",
            "Mean Percent Bias",
            "Accuracy",
            "Precision",
            "Recall",
            "F1 score",
            "Matthews Correlation",
        ],
    )
    df_metrics["Iter"] = df_metrics["Iter"].astype(int)
    df_metrics = df_metrics.sort_values(by=["Method", "Type", "Percent", "Iter"])
    return df_metrics


# this we are not currently using.
# but I would still love to know why this is different.
# this is definitely a priority.
# tends to give higher values.
def pairwise_metrics_study1(methods, df_complete, data_path, constant_variables=None):
    # get the columns that we calculate on
    question_columns = [col for col in df_complete.columns if col.startswith("X")]
    if constant_variables:
        question_columns = [
            col for col in question_columns if col not in constant_variables
        ]

    # load and preprocess complete data
    df_complete = df_complete[question_columns]
    original_corr = df_complete.corr()
    mask = np.triu(np.ones_like(original_corr, dtype=bool))
    original_flat = original_corr.mask(mask).stack()

    # get length of original data for testing
    original_length = len(original_flat)

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
            imputed_filter = imputed_data[question_columns]
            imputed_corr = imputed_filter.corr()
            imputed_flat = imputed_corr.mask(mask).stack()
            # check whether we have nan
            complete_data = len(imputed_flat) == original_length
            # calculate pairwise metrics
            frobenius, avg_squared, mae = pairwise_metrics(original_flat, imputed_flat)
            evaluation_list.append(
                [
                    type,
                    percent,
                    iter,
                    method,
                    frobenius,
                    avg_squared,
                    mae,
                    complete_data,
                ]
            )
    evaluation_df = pd.DataFrame(
        evaluation_list,
        columns=[
            "Type",
            "Percent",
            "Iter",
            "Method",
            "Frobenius Norm",
            "Squared Difference",
            "MAE",
            "complete_data",
        ],
    )
    evaluation_df["Iter"] = evaluation_df["Iter"].astype(int)
    evaluation_df = evaluation_df.sort_values(by=["Type", "Percent", "Iter"])
    return evaluation_df


def flag_constant_variables(methods, df_complete, data_path):
    question_columns = [col for col in df_complete.columns if col.startswith("X")]
    constant_columns = []
    for method in methods:
        files = os.listdir(os.path.join(data_path, method))
        for file in files:
            df_impute = pd.read_csv(os.path.join(data_path, method, file))
            for question_column in question_columns:
                if len(df_impute[question_column].unique()) == 1:
                    constant_columns.append(question_column)
    constant_columns = list(np.unique(np.array(constant_columns)))
    return constant_columns


# we should do this on a by-question-basis
def pairwise_correlations_study1(
    methods, df_complete, data_path, constant_variables=None
):
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
    return evaluation_df


def basic_metrics_study2(method_grid, df_complete, nan_path, data_path):

    question_columns = [col for col in df_complete.columns if col.startswith("X")]
    df_complete = df_complete[question_columns]
    nan_files = os.listdir(nan_path)
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
            "Question",
            "Method",
            "Type",
            "Percent",
            "Iter",
            "RMSE",
            "Mean Percent Bias",
            "Accuracy",
            "Precision",
            "Recall",
            "F1 score",
            "Matthews Correlation",
        ],
    )
    df_metrics["Iter"] = df_metrics["Iter"].astype(int)
    df_metrics = df_metrics.sort_values(by=["Method", "Type", "Percent", "Iter"])
    return df_metrics
