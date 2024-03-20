import numpy as np
import pandas as pd

### fix entries ###
entry_metadata = pd.read_csv("../Data/Preprocessed/entry_metadata.csv")
regions_coded = pd.read_csv("../Data/Preprocessed/regions_coded.csv")

# regions to wide dummy format
regions_coded = regions_coded[["entry_id", "world_region"]].drop_duplicates()
regions_wide = regions_coded.pivot_table(
    index="entry_id", columns="world_region", aggfunc=len, fill_value=0
)
regions_wide = regions_wide.reset_index()

# merge with entry metadata
entry_metadata = entry_metadata[["entry_id", "year_from", "year_to", "log_area"]]
entry_metadata = entry_metadata.merge(regions_wide, on="entry_id", how="inner")

# fix column names here
entry_metadata = entry_metadata.rename(
    columns={
        "Africa": "region_africa",
        "Central Eurasia": "region_central_eurasia",
        "East Asia": "region_east_asia",
        "Europe": "region_europe",
        "North America": "region_north_america",
        "Oceania-Australia": "region_oceania_australia",
        "South America": "region_south_america",
        "South Asia": "region_south_asia",
        "Southeast Asia": "region_southeast_asia",
        "Southwest Asia": "region_southwest_asia",
    }
)

### fix answers ###
questions = pd.read_csv("../Data/Preprocessed/answers.csv")


# create a dataset with n=15 most answered questions
def get_count_answers(questions: pd.DataFrame, n: int) -> pd.DataFrame:
    """
    Get the n most answered questions.
    """
    count_answers = questions.groupby("question_id").size().reset_index(name="count")
    count_answers = count_answers.sort_values(by="count", ascending=False)
    count_answers = count_answers.head(n)
    return count_answers


small_dataset = get_count_answers(questions, 15)
large_dataset = get_count_answers(questions, 100)

questions = questions[["entry_id", "question_id", "answer_value"]].drop_duplicates()
questions_small = questions[questions["question_id"].isin(small_dataset["question_id"])]
questions_large = questions[questions["question_id"].isin(large_dataset["question_id"])]


# pivot the questions
def pivot_questions(questions, entry_metadata):
    questions_wide = questions.pivot_table(
        index="entry_id",
        columns="question_id",
        values="answer_value",
        fill_value=np.nan,
    )
    questions_wide = questions_wide.reset_index()
    questions_wide = entry_metadata.merge(questions_wide, on="entry_id", how="inner")
    return questions_wide


pivot_small = pivot_questions(questions_small, entry_metadata)
pivot_large = pivot_questions(questions_large, entry_metadata)


# prefix numeric columns with X
def prefix_column_names(df):
    modified_names = [f"X{col}" if isinstance(col, int) else col for col in df.columns]
    df.columns = modified_names
    return df


pivot_small = prefix_column_names(pivot_small)
pivot_large = prefix_column_names(pivot_large)

# for the small dataset drop nan
pivot_small = pivot_small.dropna()

# save data
pivot_small.to_csv("../Data/Preprocessed/answers_study1.csv", index=False)
pivot_large.to_csv("../Data/Preprocessed/answers_study2.csv", index=False)

# save id vars (same for pivot_small and pivot_large)
id_columns = [col for col in pivot_small.columns if not col.startswith("X")]
file_path = "../Imputation/id_vars.txt"
with open(file_path, "w") as file:
    for col in id_columns:
        file.write(col + "\n")
