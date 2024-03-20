import numpy as np
import pandas as pd

from sklearn.ensemble import RandomForestRegressor
from sklearn.experimental import enable_iterative_imputer  # noqa
from sklearn.impute import IterativeImputer, KNNImputer, SimpleImputer
from sklearn.model_selection import cross_val_score
from sklearn.pipeline import make_pipeline


def knn_imputation(df, k=0):
    # if k not supplied use sqrt(n)
    if k == 0:
        k = int(round(np.sqrt(len(df)), 0))
    imputer = KNNImputer(n_neighbors=k)
    x = imputer.fit_transform(df)
    df_wide_imputed = pd.DataFrame(x, columns=df.columns)
    df_wide_imputed = df_wide_imputed.round().astype(int)
    return df_wide_imputed


def iterative_imputer(df, estimator, max_iter=25):
    imputer = IterativeImputer(random_state=0, estimator=estimator, max_iter=max_iter)
    x = imputer.fit_transform(df)
    df_imputed = pd.DataFrame(x, columns=df.columns)
    df_imputed = df_imputed.round().astype(int)
    return df_imputed
