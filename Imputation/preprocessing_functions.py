import numpy as np
import pandas as pd


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
            drh["Question ID"] == question_id, "Parent question ID"
        ].values[0]
        level = inner_find_question_level(parent_id) + 1
        # Store the computed level in the cache
        computed_levels[question_id] = level
        return level

    return inner_find_question_level(question_id)


# 1. Add nan to sub-questions
def update_child_questions(df_wide, df_relationships):
    # Create a copy of df_wide to work on, preserving the original DataFrame
    updated_df = df_wide.copy()

    def update_descendants(parent_id, affected_entries):
        children = df_relationships[
            df_relationships["Parent question ID"] == parent_id
        ]["Question ID"]
        for child_id in children:
            child_id_str = str(child_id)
            updated_df.loc[affected_entries, child_id_str] = np.nan
            update_descendants(child_id, affected_entries)

    for _, row in df_relationships[
        df_relationships["Parent question ID"] == 0
    ].iterrows():
        question_id = row["Question ID"]
        question_id_str = str(question_id)
        affected_entries = updated_df[question_id_str].isna()
        update_descendants(question_id, affected_entries)

    return updated_df


# 2. Check impossible cases
def check_impossible_cases(df_wide, df_relationships):
    inconsistencies = []

    # Function to recursively check conditions for child questions
    def check_conditions(parent_id, parent_response, entry_id):
        # Find direct children of the current parent
        children = df_relationships[
            df_relationships["parent_question_id"] == parent_id
        ]["question_id"]

        for child_id in children:
            child_id_str = str(child_id)
            child_response = df_wide.loc[
                df_wide["ID"] == entry_id, child_id_str
            ].values[0]

            # Check the conditions
            # Condition 1: Child cannot be "Yes" if parent is "No" or NaN
            if parent_response in [0, np.nan] and child_response == 1:
                inconsistencies.append(
                    f'Entry {entry_id}: Child {child_id} is "Yes" when Parent {parent_id} is {parent_response}'
                )

            # Condition 2: Child must be NaN if parent is NaN
            if pd.isna(parent_response) and not pd.isna(child_response):
                inconsistencies.append(
                    f"Entry {entry_id}: Child {child_id} should be NaN when Parent {parent_id} is NaN, found {child_response}"
                )

            # Recurse to check conditions for the child's children, if any
            check_conditions(child_id, child_response, entry_id)

    # Iterate through each entry in df_wide
    for _, entry in df_wide.iterrows():
        entry_id = entry["entry_id"]
        # Start with base questions (those with a 'parent_question_id' of 0) and their responses
        base_questions = df_relationships[df_relationships["parent_question_id"] == 0][
            "question_id"
        ]

        for question_id in base_questions:
            question_id_str = str(question_id)
            response = entry[question_id_str]
            check_conditions(question_id, response, entry_id)

    return inconsistencies


def generate_datasets(df, max_level):
    datasets = {}

    # for level 1 simply take this data
    df_level_1 = df[df["question_level"] == 1]
    datasets[1] = df_level_1

    # find all level 2 questions
    encountered_parents = []
    for i in range(2, max_level + 1):
        level_i_questions = df[df["question_level"] == i]["question_id"]

        # Find the parents of level i questions
        parents_of_level_i = (
            df[df["question_id"].isin(level_i_questions)]["parent_question_id"]
            .unique()
            .tolist()
        )

        # add to list of encountered parents
        encountered_parents.extend(parents_of_level_i)

        # For the second dataset, exclude the parents that are only parents to level 2 questions
        level_i_and_below = df[
            ~df["question_id"].isin(encountered_parents) & (df["question_level"] <= i)
        ]
        datasets[i] = level_i_and_below

    return datasets
