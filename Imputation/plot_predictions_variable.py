import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df_metrics = pd.read_csv("evaluation/df_var_metrics.csv")


# plot regardless for now
def plot_evaluation(df, metric, hue="method"):

    # Assuming df is your DataFrame and it's already imported
    df_grouped = df.groupby([hue, "percent"])[metric].mean().reset_index(name="average")
    plt.figure(figsize=(10, 6))  # Adjust the figure size as needed
    sns_plot = sns.lineplot(
        data=df_grouped, x="percent", y="average", hue=hue, marker="o"
    )
    plt.title(f"{metric} by Percent Missing")
    plt.xlabel("Percent Missing")
    plt.ylabel("Average " + metric)
    # Place the legend outside the plot
    plt.legend(title=f"{hue}", bbox_to_anchor=(1.05, 1), loc="upper left")
    # Adjust plot layout to make room for the legend
    plt.tight_layout()
    plt.show()


### check by question ###
# Mean percent bias
plot_evaluation(df=df_metrics, metric="mpb", hue="question")
"""
Mean percent bias: 
highest around 10%: how good or bad is that?
"""


plot_evaluation(df=df_metrics, metric="accuracy", hue="question")
"""
Accuracy generally pretty good. 
Lowest are around 70% which is not that impressive.
"""


plot_evaluation(df_metrics, "f1", hue="question")
plot_evaluation(df_metrics, "precision", hue="question")
plot_evaluation(df_metrics, "recall", hue="question")
"""
For these the ones that are hard are the ones that are
just always 0. 
"""

# this means that we are not doing particularly well...
plot_evaluation(df_metrics, "matthews_corr", hue="question")
"""
Here we have more "bad" cases; 
These are both the ones that are always 0 and always 1. 
Because we are not really doing better than just predicting
always 1 and 0.
This will give really good accuracy (basically perfect)
but is not treated as better than baseline. 
"""

plot_evaluation(df_metrics, "rmse", hue="type")
plot_evaluation(df_metrics, "mpb", hue="type")
plot_evaluation(df_metrics, "accuracy", hue="type")
plot_evaluation(df_metrics, "f1", hue="type")
plot_evaluation(df_metrics, "precision", hue="type")
plot_evaluation(df_metrics, "recall", hue="type")
plot_evaluation(df_metrics, "matthews_corr", hue="type")
"""
No large differences between the missingness types.
Makes sense because this is for prediction (evaluation). 
Should be possible to construct an evaluation that takes
into account the missingness type. 
Could potentially be an Ising type thing for the all binary features. 
"""

##### INVESTIGATION ######

### generally variables we are doing bad on ###
df_matthew = (
    df_metrics.groupby("question")["matthews_corr"].mean().reset_index(name="matthew")
)

"""
X4780
X4776
X4827
X4654
X4954
X5132
X5196
X4821
"""

### do these have anything in common? ###
df_overall = pd.read_csv("../Data/Preprocessed/answers_study1.csv")
columns = [x for x in df_overall.columns if x.startswith("X")]
df_overall = df_overall[columns]
df_mean = df_overall.mean().reset_index(name="mean").sort_values("mean")
df_mean_matthew = df_mean.merge(df_matthew, left_on="index", right_on="question")

# clearly not the only thing driving it
# but definitely one of the things driving it.
fig, ax = plt.subplots()
plt.scatter(df_mean_matthew["mean"], df_mean_matthew["matthew"])
plt.xlabel("Global mean")
plt.ylabel("Matthews correlation")
plt.show()

### Things I am wondering ###
""" 
Which predictors actually make a difference?
"""
