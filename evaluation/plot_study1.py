import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from evaluation_functions import multiple_lineplots, single_lineplot

df_metrics = pd.read_csv("evaluation/metrics_study1.csv")
df_long = pd.melt(
    df_metrics,
    id_vars=["Question", "Method", "Type", "Percent", "Iter"],
    value_vars=["Accuracy", "Mean Percent Bias", "Matthews Correlation"],
    var_name="metric",
    value_name="values",
)

# overall metrics plot #
multiple_lineplots(
    df=df_long,
    metric="values",
    hue="Method",
    grid="metric",
    ncol_legend=4,
    outpath="../figures/study1",
    outname="overall_metrics.png",
)

""" 
MICE (standard) surprisingly bad (MPB, Accuracy). 
missForest best across all metrics. 
"""

# correlations between variables plot #
pairwise_corr = pd.read_csv("evaluation/correlations_study1.csv")
pairwise_corr["corr_delta"] = (
    pairwise_corr["corr_complete"] - pairwise_corr["corr_imputed"]
)
pairwise_corr["corr_delta_abs"] = abs(pairwise_corr["corr_delta"])
pairwise_corr["corr_delta_sq"] = pairwise_corr["corr_delta"] ** 2
pairwise_corr["pairs"] = pairwise_corr["var_x"] + " " + pairwise_corr["var_y"]
pairwise_corr = pairwise_corr.rename(columns={"corr_delta_sq": "Squared Difference"})
single_lineplot(
    df=pairwise_corr,
    metric="Squared Difference",
    hue="Method",
    ncol_legend=4,
    outpath="../figures/study1",
    outname="correlation_difference.png",
)

""" 
miceRF (best), miceCART (second), missForest (third) 
MICE (standard) doing worse than mode is surprising.
"""

### additional plots ###

## correlation

# correlation by missingness mechanism #
multiple_lineplots(
    df=pairwise_corr,
    grid="Type",
    metric="Squared Difference",
    hue="Method",
    legend=True,
    figsize=(7, 7),
    sharey="all",
)

# correlation by question #
multiple_lineplots(
    df=pairwise_corr,
    grid="method",
    metric="Squared Difference",
    hue="pairs",
    legend=False,
    figsize=(7, 7),
    alpha=0.5,
    color="tab:grey",
    sharey="all",
)

## overall metrics

# overall metrics by missingness mechanism #
multiple_lineplots(
    df=df_metrics,
    metric="Accuracy",
    hue="method",
    grid="type",
    sharey="all",
    ncol_legend=4,
)
multiple_lineplots(
    df=df_metrics,
    metric="Mean Percent Bias",
    hue="method",
    grid="type",
    sharey="all",
    ncol_legend=4,
)
multiple_lineplots(
    df=df_metrics,
    metric="Matthews Correlation",
    hue="method",
    grid="type",
    sharey="all",
    ncol_legend=4,
)

# overall metrics by question #
multiple_lineplots(
    df=df_metrics,
    metric="Accuracy",
    hue="question",
    grid="method",
    color="tab:grey",
    legend=False,
    sharey="all",
)
multiple_lineplots(
    df=df_metrics,
    metric="Matthews Correlation",
    hue="question",
    grid="method",
    color="tab:grey",
    legend=False,
    sharey="all",
)
multiple_lineplots(
    df=df_metrics,
    metric="Mean Percent Bias",
    hue="question",
    grid="method",
    color="tab:grey",
    legend=False,
    sharey="all",
)

## explaining question variation
df_overall = pd.read_csv("../Data/Preprocessed/answers_study1.csv")
columns = [x for x in df_overall.columns if x.startswith("X")]
df_overall = df_overall[columns]
df_global_average = (
    df_overall.mean().reset_index(name="Global Average").sort_values("Global Average")
)
df_mean_metrics = (
    df_metrics.groupby(["question", "method"])[
        ["Accuracy", "Mean Percent Bias", "Matthews Correlation"]
    ]
    .agg("mean")
    .reset_index()
)
df_mean_metrics = df_mean_metrics.merge(
    df_global_average, left_on="question", right_on="index"
)


def plot_scatter(df, x, y, jitter=0):
    fig, ax = plt.subplots()
    df[x] = df[x] + np.random.normal(0, jitter, len(df))
    df[y] = df[y] + np.random.normal(0, jitter, len(df))
    sns.scatterplot(data=df, x=x, y=y, hue="method")
    plt.show()


plot_scatter(df_mean_metrics, "Global Average", "Accuracy", jitter=0.01)
plot_scatter(df_mean_metrics, "Global Average", "Mean Percent Bias", jitter=0.01)
plot_scatter(df_mean_metrics, "Global Average", "Matthews Correlation", jitter=0.01)

""" 
Accuracy: higher for extreme (almost all 1 or 0) across methods. 
Mean Percent Bias: lower for extreme (almost all 1 or 0) -- MICE stable. 
Matthews Correlation: lower for extreme (almost all 1 or 0) across methods. 
"""
