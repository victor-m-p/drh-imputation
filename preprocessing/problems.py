import pandas as pd

data = pd.read_csv("../data/raw/raw_data.csv")
data[(data["entry_id"] == 2074) & (data["question_id"] == 4827)]

# this one should just be "Yes" it seems from website?
