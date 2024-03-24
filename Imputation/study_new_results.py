import numpy as np
import pandas as pd

# question level
question_level = pd.read_csv("../Data/Preprocessed/question_level_study2.csv")

#
df_complete = pd.read_csv("../Data/Preprocessed/answers_study2.csv")
question_columns = [col for col in df_complete.columns if col.startswith("X")]
df_complete = df_complete[question_columns]
df_complete.mean()
