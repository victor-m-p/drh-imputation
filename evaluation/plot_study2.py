import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from evaluation_functions import multiple_lineplots, single_lineplot

# setup
outpath = "../figures/study2"
df_metrics = pd.read_csv("evaluation/metrics_study2.csv")
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
    outpath=outpath,
    outname="overall_metrics.png",
)

# some more plots to better understand this ... #
