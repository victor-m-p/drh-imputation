import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from evaluation_functions import grid_plot

# overall metrics 
df_metrics = pd.read_csv("evaluation/metrics_study1.csv")
grid_plot(df=df_metrics, metric="Accuracy", hue="method", grid="type", sharey='all')
grid_plot(df=df_metrics, metric="Mean Percent Bias", hue="method", grid="type", sharey='all')
grid_plot(df=df_metrics, metric="Matthews Correlation", hue="method", grid="type", sharey='all')

df_long = pd.melt(df_metrics, id_vars=['question', 'method', 'type', 'percent', 'iter'],
                  value_vars=['Accuracy', 'Mean Percent Bias', 'Matthews Correlation'],
                  var_name='metric', value_name='values')

# this probably is the plot we should save for now (actually). 
grid_plot(df=df_long, metric="values", hue="method", grid="metric")

""" 
Accuracy: missForest (good), mode (good), mice (bad)
Mean Percent Bias: missForest (good), mode (terrible), mice (bad)
Matthews Correlation: missForest (good), mode (terrible), mice (okay)

Overall: 
Unclear why MICE is so bad.
In general I thought we would do better. 
"""

# question variation
grid_plot(df=df_metrics, metric="Accuracy", hue="question", grid="method", color='tab:grey', legend=False, sharey='all')
grid_plot(df=df_metrics, metric="Matthews Correlation", hue='question', grid = 'method', color='tab:grey', legend=False, sharey='all')
grid_plot(df=df_metrics, metric='Mean Percent Bias', hue='question', grid='method', color='tab:grey', legend=False, sharey='all')

""" 
Large variability in our ability to predict specific questions. 
"""

# explaining question variation 
# other explanation should be strong correlations between variables 
df_overall = pd.read_csv("../Data/Preprocessed/answers_study1.csv")
columns = [x for x in df_overall.columns if x.startswith("X")]
df_overall = df_overall[columns]
df_global_average = df_overall.mean().reset_index(name="Global Average").sort_values("Global Average")
df_mean_metrics = df_metrics.groupby(["question", 'method'])[["Accuracy", "Mean Percent Bias", "Matthews Correlation"]].agg('mean').reset_index()
df_mean_metrics = df_mean_metrics.merge(df_global_average, left_on="question", right_on="index")

def plot_scatter(df, x, y, jitter=0): 
    fig, ax = plt.subplots()
    df[x] = df[x] + np.random.normal(0, jitter, len(df))
    df[y] = df[y] + np.random.normal(0, jitter, len(df))
    sns.scatterplot(data=df, x=x, y=y, hue='method')
    plt.show()

plot_scatter(df_mean_metrics, "Global Average", "Accuracy", jitter=0.01)
plot_scatter(df_mean_metrics, "Global Average", "Mean Percent Bias", jitter=0.01)
plot_scatter(df_mean_metrics, "Global Average", "Matthews Correlation", jitter=0.01)

""" 
Accuracy: higher for extreme (almost all 1 or 0) across methods. 
Mean Percent Bias: lower for extreme (almost all 1 or 0) -- MICE stable. 
Matthews Correlation: lower for extreme (almost all 1 or 0) across methods. 
"""

# correlations between variables
pairwise_corr = pd.read_csv("evaluation/correlations_study1.csv")
pairwise_corr['corr_delta'] = pairwise_corr['corr_complete'] - pairwise_corr['corr_imputed']
pairwise_corr['corr_delta_abs'] = abs(pairwise_corr['corr_delta'])
pairwise_corr['corr_delta_sq'] = pairwise_corr['corr_delta'] ** 2
pairwise_corr['pairs'] = pairwise_corr['var_x'] + " " + pairwise_corr['var_y']

# overall plot #
pairwise_corr = pairwise_corr.rename(columns={'corr_delta_sq': 'Squared Difference'})
grid_plot(df=pairwise_corr, grid='type', metric="Squared Difference", hue="method", legend=True, figsize=(7, 7), sharey='all')
grid_plot(df=pairwise_corr, grid='method', metric='Squared Difference', hue='pairs', legend=False, figsize=(7, 7), alpha=0.5, color='tab:grey', sharey='all')

""" notes and todo:
We need to save the plots that we need. 
We need to make better plots for supplementary (e.g., the scatterplot). 
We need to troubleshoot MICE (why is it so bad?).
We need to figure out whether we want to try other methods (e.g., xgboost). 
--> here we should reference other papers that have done similar things. 
We need to ensure that the pairwise differences make sense. 
"""






### study 2 ###
df_metrics = pd.read_csv("evaluation/metrics_study2.csv")

# check grid plots
grid_plot(df=df_metrics, metric="accuracy", hue="method", grid="type")
grid_plot(df=df_metrics, metric="mpb", hue="method", grid="type")
grid_plot(df=df_metrics, metric="matthews_corr", hue="method", grid="type")

"""
MissForest doing much better than other methods. 
MICE doing worse than mode on some metrics. 
We should look into whether we can "do better" with MICE. 
"""

# check by method
simple_plot(df=df_metrics, metric="rmse", hue="method")
simple_plot(df=df_metrics, metric="mpb", hue="method")
simple_plot(df=df_metrics, metric="accuracy", hue="method")
simple_plot(df=df_metrics, metric="f1", hue="method")
simple_plot(df=df_metrics, metric="precision", hue="method")
simple_plot(df=df_metrics, metric="recall", hue="method")
simple_plot(df=df_metrics, metric="matthews_corr", hue="method")



### systematically over-under ###
worst_skews = corr_missForest[corr_missForest['percent']==50]
worst_skews[worst_skews["pairs"] == "X4676 X5226"]['corr_delta'].mean()
worst_skews[worst_skews["pairs"] == "X4745 X4794"]['corr_delta'].mean()

""" 
in both cases we systematically overestimate the correlation
"""

### is this a general thing? ###
pairwise_corr.groupby('method')['corr_delta'].mean()

"""
There is not a strong overall tendency to over- or underestimate correlations.
"""


## NB: 
## not sure that we trust the aggregated values right now.
## we should really think through this process. 
## when are we taking the square, etc. 
## I think the "single" approach is what I believe in currently. 
## Not clear to me currently why we would have much larger values for 
## the previous approach ...

# now we have missForest doing better than either mode or missForest. 



# where are the largest differences ? 
pairwise_sub = pairwise_var[pairwise_var["type"] == "MCAR"]
pairwise_sub = pairwise_sub[pairwise_sub["method"] == "missForest"]
pairwise_sub['col_combinations'] = pairwise_sub['column_x'] + " - " + pairwise_sub['column_y']
simple_plot(df=pairwise_sub, metric="mae", hue="col_combinations", legend=False)

worst_skew = pairwise_sub.groupby('col_combinations')['mae'].mean().reset_index(name='avg_mae')
worst_skew = worst_skew.sort_values('avg_mae', ascending=False)

# check a case just to see 
question_overview = pd.read_csv("../Data/Preprocessed/question_overview.csv")
question_overview[question_overview["question_id"].isin([4776, 4780])]

# are we systematically over or undershooting?
df_complete = pd.read_csv("../Data/Preprocessed/answers_study1.csv")
df_complete = df_complete[["X4776", "X4780"]]
complete_corr =df_complete.corr() # .275 (much less than I thought)
complete_np = complete_corr.to_numpy()
complete_np = complete_np[np.triu_indices(2, k = 1)][0]
complete_np

complete_np 
mask = np.triu(np.ones_like(complete_corr, dtype=bool))
flat = complete_corr.where(mask).stack().reset_index()
flat


# take a random 

"""

# more clever evaluation #


### check by question ###
plot_evaluation(df=df_metrics, metric="mpb", hue="question", legend=False)

"""
Mean percent bias: 
check highest (consistently).

"""


plot_evaluation(df=df_metrics, metric="accuracy", hue="question", legend=False)
"""
Again, lowest around 65-70% we should just check this. 
"""


plot_evaluation(df_metrics, "f1", hue="question", legend=False)
plot_evaluation(df_metrics, "precision", hue="question", legend=False)
plot_evaluation(df_metrics, "recall", hue="question", legend=False)
"""
Some clear outliers that are just 0. 
We should again make sure to check up on this. 
"""

# this means that we are not doing particularly well...
plot_evaluation(df_metrics, "matthews_corr", hue="question", legend=False)
"""
Many questions that do poorly. 
Check up on why this is. 
Also gives us better performance in some cases for more missingness...
"""

plot_evaluation(df_metrics, "rmse", hue="type")
plot_evaluation(df_metrics, "mpb", hue="type")
plot_evaluation(df_metrics, "accuracy", hue="type")
plot_evaluation(df_metrics, "f1", hue="type")
plot_evaluation(df_metrics, "precision", hue="type")
plot_evaluation(df_metrics, "recall", hue="type")
plot_evaluation(df_metrics, "matthews_corr", hue="type")

"""
Definitely harder to predict for the MCAR case. 
But also less bias in these cases. 
Would be misleading just to show MNAR as being "easier" to predict.
In some sense it is--but should also be "worse" for actual imputation.
"""


# check whether harder or easier by how strongly "Yes" or "No" is represented
def plot_mean_eval(df_metrics, df_mean, metric):
    df_metric = df_metrics.groupby("question")[metric].mean().reset_index(name=metric)
    df_mean_metric = df_mean.merge(df_metric, left_on="index", right_on="question")
    fig, ax = plt.subplots(figsize=(10, 6))
    plt.scatter(df_mean_metric["mean"], df_mean_metric[metric])
    plt.xlabel("Global mean")
    plt.ylabel(metric)
    plt.show()


""" NB: 
X4699 has 100% "YES" so we cannot calculate some metrics here (e.g. Matthew).
"""

# prepare plot
df_overall = pd.read_csv("../Data/Preprocessed/answers_study2.csv")
columns = [x for x in df_overall.columns if x.startswith("X")]
df_overall = df_overall[columns]
df_mean = df_overall.mean().reset_index(name="mean").sort_values("mean")


plot_mean_eval(df_metrics, df_mean, "matthews_corr")
"""
For Matthews corr really hard for the almost exclusively 1/0.
"""

plot_mean_eval(df_metrics, df_mean, "accuracy")
"""
For accuracy easier for the almost exclusively 1/0.
This will be the inverse for RMSE. 
"""

plot_mean_eval(df_metrics, df_mean, "f1")
plot_mean_eval(df_metrics, df_mean, "precision")
plot_mean_eval(df_metrics, df_mean, "recall")
"""
For all of these metrics easier for higher "Yes". 
"""

plot_mean_eval(df_metrics, df_mean, "mpb")
"""
MPB is also easier for more extreme (there we do not see as strong bias). 
If it is always 1 we always predict 1. 
"""

### check for levels
question_levels = pd.read_csv("../Data/Preprocessed/question_level_study2.csv")
question_levels = question_levels[["question_id", "question_level"]]
question_levels["question_id"] = [f"X{x}" for x in question_levels["question_id"]]
df_metrics_levels = df_metrics.merge(
    question_levels, left_on="question", right_on="question_id"
)

### why are we doing so much worse for the lower levels?
plot_evaluation(df_metrics_levels, "matthews_corr", hue="question_level")
plot_evaluation(df_metrics_levels, "accuracy", hue="question_level")
plot_evaluation(df_metrics_levels, "f1", hue="question_level")

""" 
Okay, this is very murky.
We do not have a lot of observations for question level == 3. 
Overall, we have pretty terrible matthews correlation for levels 2, 3.
We have realaly high accuracy for level 3 though, so perhaps these 
are just really highly "Yes"/"No" questions. 
"""

question_levels["question_level"].value_counts()
