import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

df_var_metrics = pd.read_csv("evaluation/df_var_metrics.csv")


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


plot_evaluation(df_var_metrics, "mpb")


# Questions
# 1. why does this initially "dip"
# 2. for the methods that do well (e.g. FAMD) why do we get so high scores
# --> is it because they always predict "Yes" for instance??
