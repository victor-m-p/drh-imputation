import pandas as pd

# load data
drh_t_binary = pd.read_csv("../data/drh_t_binary.csv")

# load all data
drh = pd.read_csv("../data/drh.csv")
drh = drh[["Poll", "Question ID", "Question", "Parent question ID"]].drop_duplicates()
drh = drh[drh["Poll"] == "Religious Group (v6)"]
drh["Parent question ID"] = drh["Parent question ID"].fillna(0).astype(int)

# compute "question level"
from preprocessing_functions import (
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
drh_questions_int = [int(x) for x in drh_questions]
drh_subset = drh[drh["Question ID"].isin(drh_questions_int)]

### first add nan to all levels ###
### then check whether we have impossible case ###
df_MAR = pd.read_csv("output/study1/additional_NA/NA_MAR_10_3.csv")

# Check the modified DataFrame (looks good)
df_wide_nan_updated = update_child_questions(df_MAR, drh_subset)
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

df_wide_nan_updated_subset.to_csv("df.csv", index=False)

#### now run imputation on this ####
# https://medium.com/@sanjushusanth/missing-value-imputation-techniques-in-python-62aeab65a6a6
# https://scikit-learn.org/stable/auto_examples/impute/plot_missing_values.html#sphx-glr-auto-examples-impute-plot-missing-values-py
# https://scikit-learn.org/stable/auto_examples/impute/plot_iterative_imputer_variants_comparison.html#sphx-glr-auto-examples-impute-plot-iterative-imputer-variants-comparison-py
from imputation_functions import knn_imputation

df_wide_imputed = knn_imputation(df_wide_nan_updated_subset)

import numpy as np
import pandas as pd

from sklearn.ensemble import RandomForestRegressor
from sklearn.experimental import enable_iterative_imputer  # noqa
from sklearn.impute import IterativeImputer, KNNImputer, SimpleImputer
from sklearn.model_selection import cross_val_score
from sklearn.pipeline import make_pipeline
from sklearn.neighbors import KNeighborsRegressor


estimators = [
    RandomForestRegressor(
        # check up on hyper-parameters
        n_estimators=4,
        max_depth=10,
        bootstrap=True,
        max_samples=0.5,
        n_jobs=2,
        random_state=0,
    ),
]

df_wide_nan_updated_subset[question_columns] = df_wide_nan_updated_subset[
    question_columns
].astype("category")
df_wide_nan_updated_subset

imputer = IterativeImputer(random_state=0, estimator=estimators[0], max_iter=25)
x = imputer.fit_transform(df_wide_nan_updated_subset)
df_wide_imputed = pd.DataFrame(x, columns=df_wide_nan_updated_subset.columns)
df_wide_imputed
df_wide_imputed = df_wide_imputed.round().astype(int)
df_wide_imputed


""" notes:
iterative imputer: gaussian outcome variables (so not applicable).
knn imputer: 

"""

# we cannot really do cross validation because we will have
# very few observations that are complete
# especially for high missingness.
# this reflects the real world scenario.
# use heuristic


""" imputing hierarchical data
1. add nan to all levels
2. impute super-questions 
3. if super-question==No then sub-questions==No
4. remove all super-questions that have sub-questions. 
5. impute sub-questions  
"""
