import pandas as pd

# read data
data = pd.read_csv("../Data/Raw/raw_data.csv")

# rename and select columns
data = data.rename(columns={"value": "answer_value"})
questions = data[
    [
        "poll",
        "entry_id",
        "question_id",
        "question_name",
        "parent_question_id",
        "parent_question",
        "answer_value",
        "answer",
    ]
].drop_duplicates()

# only take answers in (1, 0)
# 1 = yes
# 0 = no
# we will fill the rest of the useful questions with nan.
questions = questions[questions["answer_value"].isin([1, 0])]

# recode parent questions to be 0 if nan
questions["parent_question_id"] = questions["parent_question_id"].fillna(0).astype(int)

### fix relations between polls ###

# load relation data
question_relation = pd.read_csv("../Data/Raw/question_relation.csv")
question_id_poll = questions[["question_id", "poll"]].drop_duplicates()

from functions_answers import map_question_relation

question_relation = map_question_relation(
    question_id_poll, question_relation, "Religious Group (v6)"
)

# select only needed polls
question_relation = question_relation[
    question_relation["poll"].isin(["Religious Group (v6)", "Religious Group (v5)"])
]
questions = questions[
    questions["poll"].isin(["Religious Group (v6)", "Religious Group (v5)"])
]

# create question mapping dict
# 0 -> 0 is necessary for parents
question_mapping_dict = question_relation.set_index("question_id")[
    "related_question_id"
].to_dict()
question_mapping_dict[0] = 0

# map question_id and parent_question_id
questions["question_id"] = questions["question_id"].map(question_mapping_dict)
questions["parent_question_id"] = questions["parent_question_id"].map(
    question_mapping_dict
)

# overview data
question_overview = questions[questions["poll"] == "Religious Group (v6)"]
question_overview = question_overview[
    ["question_id", "question_name"]
].drop_duplicates()
question_overview.to_csv("../Data/Preprocessed/question_overview.csv", index=False)


# only take the questions that appear in both polls
questions_v5 = (
    questions[questions["poll"] == "Religious Group (v5)"]["question_id"]
    .unique()
    .tolist()
)
questions_v6 = (
    questions[questions["poll"] == "Religious Group (v6)"]["question_id"]
    .unique()
    .tolist()
)
questions = questions[
    questions["question_id"].isin(questions_v5)
    & questions["question_id"].isin(questions_v6)
]

# get question names but only for v6 because they are inconsistent with v5
questions_v6 = questions[questions["poll"] == "Religious Group (v6)"][
    ["question_id", "question_name"]
].drop_duplicates()

# remove columns
# drop duplicates because question names differ between polls
questions = questions[
    ["entry_id", "question_id", "parent_question_id", "answer_value", "answer"]
].drop_duplicates()

# re-insert question names
questions = questions.merge(questions_v6, on="question_id", how="inner")

# now we need to clean this
# by data type
drh_v6_poll = pd.read_csv("../data/drh_v6_poll.csv")
drh_v6_poll = drh_v6_poll[["Question ID", "Data Type"]].drop_duplicates()
drh_v6_poll = drh_v6_poll.rename(
    columns={"Question ID": "question_id", "Data Type": "data_type"}
)

questions = questions.merge(drh_v6_poll, on="question_id", how="inner")
questions = questions[
    ~questions["data_type"].isin(["Qualitative", "Nominal - Multiple"])
]

# specific types of questions
questions = questions[
    ~questions["question_name"].str.contains(r"\[specify\]", regex=True)
]
questions = questions[
    ~questions["answer"].str.contains(r"\[specify in comments\]", regex=True)
]


# take random answer if inconsistent (major decision)
questions = (
    questions.groupby(["entry_id", "question_id"])
    .apply(lambda x: x.sample(1))
    .reset_index(drop=True)
)

# write data
questions.to_csv("../Data/Preprocessed/answers.csv", index=False)


# calculate level of question
# find question level for all answers
def find_question_level(question_id, drh):
    computed_levels = (
        {}
    )  # Moved inside the function to reset each call or make it an argument to preserve state across calls

    def inner_find_question_level(question_id):
        # Base case: if the parent question ID is 0, the level is 0
        if question_id == 0:
            return 0
        # If already computed, return the stored level
        if question_id in computed_levels:
            return computed_levels[question_id]

        # Recursive case: find the parent question's ID and level
        parent_id = drh.loc[
            drh["question_id"] == question_id, "parent_question_id"
        ].values[0]
        level = inner_find_question_level(parent_id) + 1
        # Store the computed level in the cache
        computed_levels[question_id] = level
        return level

    return inner_find_question_level(question_id)


question_level = questions[["question_id", "parent_question_id"]].drop_duplicates()
question_level["question_level"] = question_level["question_id"].apply(
    lambda x: find_question_level(x, question_level)
)
question_level.to_csv("../Data/Preprocessed/question_level.csv", index=False)
