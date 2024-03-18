import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df_metrics = pd.read_csv("evaluation/df_metrics.csv")

# this is already weird.
# we know that MICE has nan.
df_nan = df_metrics[df_metrics["nan_in_y_pred"] == True]
df_nan.groupby("method").size()


# plot regardless for now
def plot_evaluation(df, metric):

    # Assuming df is your DataFrame and it's already imported
    df_grouped = (
        df.groupby(["method", "percent"])[metric].mean().reset_index(name="average")
    )
    plt.figure(figsize=(10, 6))  # Adjust the figure size as needed
    sns_plot = sns.lineplot(data=df_grouped, x="percent", y="average", hue="method")
    plt.title(f"{metric} by Percent Missing")
    plt.xlabel("Percent Missing")
    plt.ylabel("Average " + metric)
    # Place the legend outside the plot
    plt.legend(title="Method", bbox_to_anchor=(1.05, 1), loc="upper left")
    # Adjust plot layout to make room for the legend
    plt.tight_layout()
    plt.show()


# very consistent performance on different metrics
# some differences; but nothing that flips the script

# make sure that they are always the same color
# fix legend to be outside of plot
# get a title

plot_evaluation(df_metrics, "rmse")  # MICE is terrible
plot_evaluation(df_metrics, "f1")  # MICE is terrible
plot_evaluation(df_metrics, "accuracy")  # MICE approaches baseline really quickly
plot_evaluation(df_metrics, "precision")  # (TP)/(TP+FP)
plot_evaluation(df_metrics, "recall")  # (TP)(TP+FN)
plot_evaluation(df_metrics, "matthews_corr")  # = 0

# number of (0, 1) across whole dataset.
# does tell us something; I would bet that they all overestimate "yes".
# question: from random_forest (for instance) do we take probabilistically or mechanistically?

# just seems that MICE is completely useless.
# based on this evaluation

# 1. remember that we have NAN in MICE.
# 2. check whether we match RMSE from Rachel
random_forest_mar_10 = pd.read_csv("../output/study1/overall_metrics/MICE_MAR_70.csv")

# she has a crazy spike for NRMSE for the missing case...?
# how does she get that value??

df_comparison = df_metrics[
    (df_metrics["method"] == "MICE")
    & (df_metrics["percent"] == 70)
    & (df_metrics["type"] == "MAR")
]

# agree exactly on MEAN (good so far).
df_comparison["rmse"].mean()
random_forest_mar_10["NRMSE"].mean()

# do not agree on iter??
# but I do think that my ITER is correct

### test who is right about iter ###
columns = [
    "4745",
    "4729",
    "4954",
    "4794",
    "5226",
    "4676",
    "4983",
    "4821",
]


df_complete = pd.read_csv("../output/study1/complete_cases/complete_cases.csv")
df_complete = df_complete[columns]
df_complete = df_complete.to_numpy()

mt, mp, iter = "MAR", 10, 2
df_nan = pd.read_csv(f"../output/study1/additional_NA/NA_{mt}_{mp}_{iter}.csv")
df_nan = df_nan[columns]
valid_indices = df_nan.isna()
valid_indices = valid_indices.to_numpy()

df_impute = pd.read_csv(f"../output/study1/random_forest/rf_{mt}_{mp}_{iter}.csv")
df_impute = df_impute[columns]
df_impute = df_impute.to_numpy()

y_true = df_complete[valid_indices]
y_pred = df_impute[valid_indices]

from sklearn.metrics import (
    accuracy_score,
    precision_score,
    recall_score,
    f1_score,
    mean_squared_error,
)

rmse = np.sqrt(mean_squared_error(y_true, y_pred))

"""
Rachel has a bug in evaluation.
The runs do not correspond.
This does not really matter, but still a bug. 

Rachel calculates the ones with missing values differently.
Whatever she does it greatly inflates "how wrong" the model is.
She codes them as -1 somehow? Not sure why. 
In general, does not make sense to score models that have NAN.
"""

df_metrics[df_metrics["method"] == "GLRM"]

# for MAR 10 1 how do we get such good results for GLRM?
d_nan = pd.read_csv("../output/study1/additional_NA/NA_MAR_10_1.csv")
d_GLRM = pd.read_csv("../output/study1/GLRM/GLRM_MAR_10_1.csv")
d_complete = pd.read_csv("../output/study1/complete_cases/complete_cases.csv")

from variables import columns

d_nan = d_nan[columns]
d_GLRM = d_GLRM[columns]
d_complete = d_complete[columns]

d_nan = d_nan.to_numpy()
d_GLRM = d_GLRM.to_numpy()
d_complete = d_complete.to_numpy()

valid_indices = np.isnan(d_nan)
y_true = d_complete[valid_indices]
y_pred = d_GLRM[valid_indices]

from sklearn.metrics import (
    accuracy_score,
    precision_score,
    recall_score,
    f1_score,
    mean_squared_error,
    matthews_corrcoef,
)

f1_score(y_true, y_pred)
accuracy_score(y_true, y_pred)
precision_score(y_true, y_pred)  # (TP)/(TP+FP)
recall_score(y_true, y_pred)  # (TP)/(TP+FN)
matthews_corrcoef(y_true, y_pred)  # = 0
# XGBoost feature importance?
