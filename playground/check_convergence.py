import numpy as np
import pandas as pd
import os
import re
from itertools import product
import matplotlib.pyplot as plt
import seaborn as sns


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

    nan_cases = []
    for i in files:
        if i.endswith(".txt"):
            nan_cases.append(i)
        elif find_nan(path, i):
            nan_cases.append(i)

    nan_df = problem_to_df(nan_cases, pattern)
    nan_dataframes.append(nan_df)

# overview
df_nan = pd.concat(nan_dataframes)  # only in MICE now
df_nan["percent"].min()  # minimum is 70% missingness now

# this gives us the information on where we have problems.
# iterate over rows in df_nan and extract type, percent, and iter
# use these to find the corresponding file in inpath
# read the file and calculate the mean of the columns
# if the mean is 1, record the type, percent, and iter
# do it for the following columns:
inpath = "../output/study1/additional_NA"
files = os.listdir(inpath)
columns = [
    "4827",
    "4745",
    "4780",
    "4776",
    "4654",
    "4828",
    "4729",
    "4954",
    "4794",
    "5226",
    "4676",
    "4983",
    "5132",
    "4730",
    "4821",
]

results = []
for file in files:
    # load file
    filepath = os.path.join(inpath, file)
    d = pd.read_csv(filepath)

    # record meta information
    nan, type, percent, iter = re.match(pattern, file).groups()

    # figure out whether there is a column with mean 1 or mean 0
    # ignoring nan
    d = d[columns]
    unique_value_columns = d.apply(lambda x: x.nunique(dropna=True) == 1)
    any_unique_value_column = unique_value_columns.any()
    results.append([type, percent, iter, any_unique_value_column])

df = pd.DataFrame(results, columns=["type", "percent", "iter", "unique_value_column"])
df_true = df[df["unique_value_column"] == True]

## compare the two data sets ##
df_nan_uniq = (
    df_nan.groupby(["type", "percent", "iter"]).size().reset_index(name="count")
)

## 1. we have nan in all cases where there is a sole unique value
inner = pd.merge(df_nan_uniq, df_true, on=["type", "percent", "iter"], how="inner")

## 2. what are the cases where we have nan but no unique value?
merged_df = pd.merge(
    df_nan_uniq, df_true, on=["type", "percent", "iter"], how="left", indicator=True
)
anti_joined_df = merged_df[merged_df["_merge"] == "left_only"].drop(columns=["_merge"])

## NB: these are almost certainly the ones where the FAMD converges.
## verify that (yes)
## so: FAMD diverges iff there is a unique value column
only_FAMD = df_nan[df_nan["method"] == "FAMD"]
only_FAMD = only_FAMD[["type", "percent", "iter"]].drop_duplicates()
inner_FAMD = pd.merge(only_FAMD, df_true, on=["type", "percent", "iter"], how="inner")

## how about MICE
nan_MICE = pd.read_csv("../output/study1/additional_NA/NA_MCAR_30_3.csv")
nan_MICE_c = nan_MICE[columns]
nan_MICE_c.mean()  # there is a column with mean==1
nan_MICE_c.groupby(["4780"]).size()
nan_MICE_c.groupby(["4776"]).size()
nan_MICE_c.groupby(["4654"]).size()

# it is not across rows that we have a problem
# something else is wrong for sure.
complete = pd.read_csv("../output/study1/complete_cases/complete_cases.csv")

for i in columns:
    print(i)
    print(complete.groupby(i).size())

complete[columns[0]]
complete_c.groupby([columns]).size()

# 4654
drh_v6 = pd.read_csv("../data/drh_v6_poll.csv")
drh_v6[drh_v6["Question ID"] == 4654]
