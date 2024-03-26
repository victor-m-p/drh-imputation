import numpy as np
import pandas as pd
import os


# setup
study = 2

# load reference
question_level = pd.read_csv(f"../Data/Preprocessed/question_level_study{study}.csv")

# we only need the cases with children
question_level_sub = question_level[question_level["question_level"] > 1]

# recode for compatibility
question_level_sub["question_id"] = [f"X{x}" for x in question_level_sub["question_id"]]
question_level_sub["parent_question_id"] = [
    f"X{x}" for x in question_level_sub["parent_question_id"]
]

# extract as a dictionary
question_dictionary = question_level_sub.set_index("question_id")[
    "parent_question_id"
].to_dict()


### 2. check conditions for nan files ###
def check_files(inpath):
    filepaths = os.listdir(inpath)
    for filename in filepaths:
        d = pd.read_csv(os.path.join(inpath, filename))
        for child, parent in question_dictionary.items():
            # parents 0 should have children 0
            parent_zero = d[d[parent] == 0]
            condition_zero = (parent_zero[child] == 0).all()

            # parents nan should have children nan
            parent_nan = d[d[parent].isna()]
            condition_nan = (parent_nan[child].isna()).all()

            if not condition_zero:
                print(
                    f"In {nan_file}, some {child} entries are not 0 when {parent} is 0. Check the data."
                )
            if not condition_nan:
                print(
                    f"In {nan_file}, some {child} entries are not NaN when {parent} is NaN. Check the data."
                )


inpath_nan = f"output/study{study}/additional_NA"
inpath_mice = f"output/study{study}/mice"
inpath_missForest = f"output/study{study}/missForest"
inpath_mode = f"output/study{study}/mode"
check_files(inpath_nan)
check_files(inpath_mice)
check_files(inpath_missForest)
check_files(inpath_mode)

## okay so now it seems like it works ##
## now we should check some nans across columns ##
nan_files = os.listdir(inpath_nan)
question_level = question_level.sort_values("question_id")
columns = question_level["question_id"].unique().tolist()
columns = [f"X{x}" for x in columns]
nan_list = []
for nan_file in nan_files:
    d = pd.read_csv(os.path.join(inpath_nan, nan_file))
    d = d[columns]
    na_count_df = d.isna().sum().reset_index(name="na_count")
    nan_list.append(na_count_df)
nan_df = pd.concat(nan_list)
question_level["question_id"] = [f"X{x}" for x in question_level["question_id"]]
mean_nan_question = (
    nan_df.groupby("index")["na_count"].mean().reset_index(name="mean_na")
)
nan_overview = question_level.merge(
    mean_nan_question, left_on="question_id", right_on="index"
)
nan_overview.groupby("question_level")["mean_na"].mean()
