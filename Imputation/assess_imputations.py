import numpy as np
import pandas as pd

d_clean = pd.read_csv("../Data/Preprocessed/answers_study1.csv")
d_imputed = pd.read_csv("output/study1/missForest/mf_MAR_20_2.csv")
d_missing = pd.read_csv("output/study1/additional_NA/NA_MAR_20_2.csv")

# assess imputation quality
question_columns = [col for col in d_clean.columns if col.startswith("X")]
d_clean[question_columns] = d_clean[question_columns].astype(int)

# Step 2: Perform the comparison
differences = d_clean != d_imputed

# Step 3: Count the number of differences
num_differences = differences.sum().sum()
print(f"Total number of differing elements: {num_differences}")

# Identify the columns with differences
# okay, why does log_area have difference????
columns_with_differences = differences.any()
diff_columns = differences.columns[columns_with_differences]
print(f"Columns with differences: {list(diff_columns)}")

# Optional: Get the number of differences per column
diff_per_column = differences.sum()
print(diff_per_column[diff_per_column > 0])  # some much easier to predict;

# Find the difference for log_area
d_clean_area = d_clean[["entry_id", "log_area"]]
d_imp_area = d_imputed[["entry_id", "log_area"]]
d_test_area = d_clean_area.merge(d_imp_area, on="entry_id", how="inner")
d_test_area[d_test_area["log_area_x"] != d_test_area["log_area_y"]]
