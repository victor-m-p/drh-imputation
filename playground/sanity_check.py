import numpy as np
import pandas as pd
import os
import re
from itertools import product
import matplotlib.pyplot as plt
import seaborn as sns

# A supreme high god is present: 4828 (sub-question of)
# Are supernatural beings present: 4827

# Are they written: 4730 (sub-question of)
# Does the religious group have scriptures: 4729

# So in our data we should always have:
## if 4729 is false then 4730 is false.
## if 4827 is false then 4828 is false.

# This should also be true about the predictions.
# Because this is a rule and not just a statistical relationship
# We would expect this to always hold (if this information has been supplied).


### now check whether this holds in the predictions ###
def find_inconsistency(path, file, var_super, var_sub):
    filename = os.path.join(path, file)
    d = pd.read_csv(filename)
    d = d.groupby([var_super, var_sub]).size().reset_index(name="count")
    mismatch = (
        len(d[(d[var_super].isin([0, "No"])) & (d[var_sub].isin([1, "Yes"]))]) > 0
    )
    return mismatch


def find_nan(path, file):
    filename = os.path.join(path, file)
    d = pd.read_csv(filename)
    return d.isna().any().any()


def problem_to_df(problem_cases, regex_pattern):
    problem_tuples = [re.match(regex_pattern, x).groups() for x in problem_cases]
    problem_df = pd.DataFrame(
        problem_tuples, columns=["method", "type", "percent", "iter"]
    )
    return problem_df


### 1. validate that this pattern is true in the input data ###
find_inconsistency(
    "../output/study1/complete_cases", "complete_cases.csv", "4827", "4828"
)
find_inconsistency(
    "../output/study1/complete_cases", "complete_cases.csv", "4729", "4730"
)

methods = [  # datawig seems obsolete
    "cart",
    "rf_mi",
    "sample_mi",
    "kNN",
    "GLRM",
    "MICE",
    "pmm",
    "random_forest",
    "FAMD",
]

inconsistent_dataframes = []
nan_dataframes = []

# pattern = r"^(.*?)_(MCAR|MAR|MNAR)_(\d+)_.*\.(?:csv|txt)$"
pattern = r"^(.*?)_(MCAR|MAR|MNAR)_(\d+)_(\d+)\..*?(?:csv|txt)$"

for method in methods:

    print(method)
    path = os.path.join("../output/study1", method)
    files = os.listdir(path)

    inconsistent_cases = []
    for i in files:
        if i.endswith(".csv"):
            inconsistency_x = find_inconsistency(path, i, "4827", "4828")
            inconsistency_y = find_inconsistency(path, i, "4729", "4730")
            if inconsistency_x | inconsistency_y:
                inconsistent_cases.append(i)

    nan_cases = []
    for i in files:
        if i.endswith(".txt"):
            nan_cases.append(i)
        elif find_nan(path, i):
            nan_cases.append(i)

    # inconsistency_pattern
    inconsistent_df = problem_to_df(inconsistent_cases, pattern)
    nan_df = problem_to_df(nan_cases, pattern)

    # append
    inconsistent_dataframes.append(inconsistent_df)
    nan_dataframes.append(nan_df)


df_inconsistent = pd.concat(inconsistent_dataframes)
df_nan = pd.concat(nan_dataframes)

### plot for inconsistency ###
# all of the method shave inconsistencies
# we are definitely not handling this correctly.
sns.countplot(data=df_inconsistent, x="percent", hue="method")
plt.legend(title="Method", bbox_to_anchor=(1.05, 1), loc="upper left")
plt.title("Imputed datasets with inconsistencies")
plt.tight_layout()
plt.savefig("inconsistent_imputation.jpg")

# plot for nan (should fill when it is nan)
# almost identical here.
df_nan["percent"] = df_nan["percent"].astype(int)
sns.countplot(data=df_nan, x="percent", hue="method")
plt.legend(title="Method", bbox_to_anchor=(1.05, 1), loc="upper left")
plt.title("Imputed datasets with NANs")
plt.tight_layout()
plt.savefig("nan_imputation.jpg")

# 2 Questions
## 1. How do we evaluate datasets with NAN?
## 2. Why do we get datasets with NAN? (also; the same ones??)

#### 2. Any obvious patterns in the datasets with nans? ####
low_pct = df_nan[df_nan["percent"] == 10]

# what is happening in these datasets??
# I really do not know
mar_10_6 = pd.read_csv("../output/study1/additional_NA/NA_MAR_10_6.csv")
