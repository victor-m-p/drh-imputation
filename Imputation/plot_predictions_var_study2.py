import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df_metrics = pd.read_csv("evaluation/df_metrics_study2.csv")


def plot_evaluation(df, metric, hue="method", legend=True):
    df_grouped = df.groupby([hue, "percent"])[metric].mean().reset_index(name="average")
    plt.figure(figsize=(10, 6))
    sns.lineplot(
        data=df_grouped,
        x="percent",
        y="average",
        hue=hue,
        marker="o",
        legend=legend,
    )
    plt.title(f"{metric} by Percent Missing")
    plt.xlabel("Percent Missing")
    plt.ylabel("Average " + metric)

    # Set x-axis ticks to only show the x values present in your data
    x_ticks = df_grouped["percent"].unique()  # Extract unique 'percent' values
    plt.xticks(x_ticks)  # Set x-axis ticks

    plt.tight_layout()
    plt.show()


### check by question ###
# Mean percent bias
plot_evaluation(df=df_metrics, metric="mpb", hue="question", legend=False)
"""
Mean percent bias: 
check highest (consistently).
"""


plot_evaluation(df=df_metrics, metric="accuracy", hue="question", legend=False)
"""
Again, lowest around 65-70% we should just check this. 
"""


plot_evaluation(df_metrics, "f1", hue="question", legend=False)
plot_evaluation(df_metrics, "precision", hue="question", legend=False)
plot_evaluation(df_metrics, "recall", hue="question", legend=False)
"""
Some clear outliers that are just 0. 
We should again make sure to check up on this. 
"""

# this means that we are not doing particularly well...
plot_evaluation(df_metrics, "matthews_corr", hue="question", legend=False)
"""
Many questions that do poorly. 
Check up on why this is. 
Also gives us better performance in some cases for more missingness...
"""

plot_evaluation(df_metrics, "rmse", hue="type")
plot_evaluation(df_metrics, "mpb", hue="type")
plot_evaluation(df_metrics, "accuracy", hue="type")
plot_evaluation(df_metrics, "f1", hue="type")
plot_evaluation(df_metrics, "precision", hue="type")
plot_evaluation(df_metrics, "recall", hue="type")
plot_evaluation(df_metrics, "matthews_corr", hue="type")

"""
Definitely harder to predict for the MCAR case. 
But also less bias in these cases. 
Would be misleading just to show MNAR as being "easier" to predict.
In some sense it is--but should also be "worse" for actual imputation.
"""


# check whether harder or easier by how strongly "Yes" or "No" is represented
def plot_mean_eval(df_metrics, df_mean, metric):
    df_metric = df_metrics.groupby("question")[metric].mean().reset_index(name=metric)
    df_mean_metric = df_mean.merge(df_metric, left_on="index", right_on="question")
    fig, ax = plt.subplots(figsize=(10, 6))
    plt.scatter(df_mean_metric["mean"], df_mean_metric[metric])
    plt.xlabel("Global mean")
    plt.ylabel(metric)
    plt.show()


""" NB: 
X4699 has 100% "YES" so we cannot calculate some metrics here (e.g. Matthew).
"""

# prepare plot
df_overall = pd.read_csv("../Data/Preprocessed/answers_study2.csv")
columns = [x for x in df_overall.columns if x.startswith("X")]
df_overall = df_overall[columns]
df_mean = df_overall.mean().reset_index(name="mean").sort_values("mean")


plot_mean_eval(df_metrics, df_mean, "matthews_corr")
"""
For Matthews corr really hard for the almost exclusively 1/0.
"""

plot_mean_eval(df_metrics, df_mean, "accuracy")
"""
For accuracy easier for the almost exclusively 1/0.
This will be the inverse for RMSE. 
"""

plot_mean_eval(df_metrics, df_mean, "f1")
plot_mean_eval(df_metrics, df_mean, "precision")
plot_mean_eval(df_metrics, df_mean, "recall")
"""
For all of these metrics easier for higher "Yes". 
"""

plot_mean_eval(df_metrics, df_mean, "mpb")
"""
MPB is also easier for more extreme (there we do not see as strong bias). 
If it is always 1 we always predict 1. 
"""
