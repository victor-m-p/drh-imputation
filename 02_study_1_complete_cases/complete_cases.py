import numpy as np
import pandas as pd

# load data
drh_t = pd.read_csv("../data/drh_t.csv")

# remove rows with non-binary
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

for col in columns:
    drh_t[col] = drh_t[col].where(drh_t[col].isin([0, 1]), np.nan)
drh_t_binary = drh_t.dropna(subset=columns)
drh_t_binary.to_csv("../data/drh_t_binary.csv", index=False)

# now remove sub-questions
## "Are they written": 4730
## "A supreme high god is present": 4828
sub_questions = ["4730", "4828"]
drh_t_binary = drh_t_binary.drop(columns=sub_questions)
drh_t_binary.to_csv("../data/drh_t_binary_super.csv", index=False)

# remove questions with less than 5% variation
binary_questions = [
    "4745",
    "4954",
    "4654",
    "4729",
    "4827",
    "4776",
    "5226",
    "4780",
    "4983",
    "4794",
    "4676",
    "4821",
    "5132",
]

# calculate 5% variation
minimum_variation = 0.05
n_total = len(drh_t_binary)
n_5 = n_total * minimum_variation

# now remove these cases
value_counts = drh_t_binary[binary_questions].apply(lambda x: x.value_counts()).T
value_counts = value_counts.rename(columns={0.0: "No", 1.0: "Yes"})
value_counts["question"] = value_counts.index
questions_low_variance = value_counts[
    (value_counts["No"] < n_5) | (value_counts["Yes"] < n_5)
]
questions_low_variance = questions_low_variance["question"].tolist()

# remove non-accepted questions
drh_t_binary = drh_t_binary.drop(columns=questions_low_variance)

# save
drh_t_binary.to_csv("../data/drh_t_subset.csv", index=False)
