import pandas as pd
import numpy as np

import os
import re
import warnings

warnings.filterwarnings("ignore")  # this makes sense here for "precision" warning
from evaluation_functions import calculate_metrics

### run simple evaluation ###

# paths to nan files
nan_path = f"output/study2/additional_NA"
nan_files = os.listdir(nan_path)

# paths to match the imputed data
data_path = "output/study2"
from constants import method_grid

# get the complete data and columns
df_complete = pd.read_csv("../Data/Preprocessed/answers_study2.csv")
question_columns = [col for col in df_complete.columns if col.startswith("X")]
df_complete = df_complete[question_columns]

data_path = "output/study2"
data_list = []
