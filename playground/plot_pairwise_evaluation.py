import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

d_NA = pd.read_csv("evaluation/additional_NA_evaluation.csv")
d_GLRM = pd.read_csv("evaluation/GLRM_evaluation.csv")
d_kNN = pd.read_csv("evaluation/kNN_evaluation.csv")
d_RF = pd.read_csv("evaluation/random_forest_evaluation.csv")
d_MICE = pd.read_csv("evaluation/MICE_evaluation.csv")
d_FAMD = pd.read_csv("evaluation/FAMD_evaluation.csv")
d_pmm = pd.read_csv("evaluation/pmm_evaluation.csv")
d_rfmi = pd.read_csv("evaluation/rf_mi_evaluation.csv")
d_samplemi = pd.read_csv("evaluation/sample_mi_evaluation.csv")
d_cart = pd.read_csv("evaluation/cart_evaluation.csv")

# first look at nan
# percent where we have nan
d_NA.groupby("percent")["complete_data"].mean()

# what is wrong with GLRM?
# check up on what is happening here...
d_GLRM.groupby("percent")["complete_data"].mean()

# these aer almost identical
d_kNN.groupby("percent")["complete_data"].mean()
d_RF.groupby("percent")["complete_data"].mean()
d_MICE.groupby("percent")["complete_data"].mean()  # almost

# okay, but how are they doing?
# all doing pretty similar actually.
# not sure how "good" this is.
# think about what this actually means.
d = pd.concat(
    [d_NA, d_GLRM, d_kNN, d_RF, d_MICE, d_FAMD, d_pmm, d_rfmi, d_samplemi, d_cart]
)


# plot stuff
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


plot_evaluation(d, "avg_squared")
plot_evaluation(d[~d["method"].isin(["GLRM", "additional_NA"])], "avg_squared")

# average squared seems useful
plot_evaluation(
    d[(~d["method"].isin(["GLRM", "additional_NA", "MICE"]) & (d["percent"] < 50))],
    "avg_squared",
)

# why is frobenius so different?
plot_evaluation(
    d[(~d["method"].isin(["GLRM", "additional_NA", "MICE"]) & (d["percent"] > 50))],
    "frobenius",
)

# mae here is easy to interpret
plot_evaluation(
    d[(~d["method"].isin(["GLRM", "additional_NA", "MICE"]) & (d["percent"] < 50))],
    "mae",
)

## GLRM:
# this is terrible for a couple of reasons.
# 1) it does not seem that good in general
# 2) it replaces all value (not just predicted) with imputations
# this does not matter for the other validations but for this one it is crazy.
