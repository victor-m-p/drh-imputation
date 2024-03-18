import pandas as pd

# load data
drh_t_binary = pd.read_csv("../data/drh_t_binary.csv")

# load all data
drh = pd.read_csv("../data/drh.csv")
drh = drh[["Poll", "Question ID", "Question", "Parent question ID"]].drop_duplicates()
drh = drh[drh["Poll"] == "Religious Group (v6)"]
drh["Parent question ID"] = drh["Parent question ID"].fillna(0).astype(int)

# compute "question level"
from imputation_functions import (
    find_question_level,
    update_child_questions,
    check_impossible_cases,
    generate_datasets,
)

drh["question_level"] = drh["Question ID"].apply(lambda x: find_question_level(x, drh))

# subset the questions we use
drh_questions = drh_t_binary.drop(
    columns=["Entry ID", "Branching question", "Region ID", "Start Date", "End Date"]
).columns.tolist()
drh_questions = [int(x) for x in drh_questions]
drh_subset = drh[drh["Question ID"].isin(drh_questions)]


### first add nan to all levels ###
### then check whether we have impossible case ###
df_wide_nan = pd.read_csv(
    "/home/vmp/imputation_comparison/output/study1_legacy/additional_NA/NA_MAR_10_2.csv"
)


# Check the modified DataFrame (looks good)
df_wide_nan_updated = update_child_questions(df_wide_nan, drh_subset)
inconsistencies = check_impossible_cases(df_wide_nan_updated, drh_subset)
assert len(inconsistencies) == 0

# 3. now we have verified that this is good.
# we now need to create the first subset to impute on.
max_n = drh_subset["question_level"].max()
datasets = generate_datasets(drh_subset, max_n)

# do it manually for now just to get a sense;
predictor_columns = [
    "elite",
    "non_elite",
    "religious_specialist",
    "start_date",
    "end_date",
    "region",
]
question_columns = datasets[1]["Question ID"].tolist()
question_columns = [str(x) for x in question_columns]
all_columns = predictor_columns + question_columns

# subset
df_wide_nan_updated_subset = df_wide_nan_updated[all_columns]
for column in question_columns:
    df_wide_nan_updated_subset[column] = df_wide_nan_updated_subset[column].astype(
        "Int64"
    )

#### now run imputation on this ####
# https://medium.com/@sanjushusanth/missing-value-imputation-techniques-in-python-62aeab65a6a6
# https://scikit-learn.org/stable/auto_examples/impute/plot_missing_values.html#sphx-glr-auto-examples-impute-plot-missing-values-py
import numpy as np

rng = np.random.RandomState(0)
from sklearn.ensemble import RandomForestRegressor

# To use the experimental IterativeImputer, we need to explicitly ask for it:
from sklearn.experimental import enable_iterative_imputer  # noqa
from sklearn.impute import IterativeImputer, KNNImputer, SimpleImputer
from sklearn.model_selection import cross_val_score
from sklearn.pipeline import make_pipeline

# iterative imputer
imp = IterativeImputer(max_iter=10, random_state=0)
imp.fit(df_wide_nan_updated_subset)
x = imp.transform(df_wide_nan_updated_subset)

# turn this into dataframe
# okay so this clearly did not work as intended in the first pass....
df_wide_imputed = pd.DataFrame(x, columns=df_wide_nan_updated_subset.columns)

#### after running imputation we need to carry over "NO" imputations
#### to sub-questions


""" imputing hierarchical data
1. add nan to all levels
2. impute super-questions 
3. if super-question==No then sub-questions==No
4. remove all super-questions that have sub-questions. 
5. impute sub-questions  
"""
