import pandas as pd
from evaluation_functions import multiple_lineplots, single_lineplot

# setup
outpath = "../figures/study1"
df_metrics = pd.read_csv("evaluation/metrics_study1.csv")
df_long = pd.melt(
    df_metrics,
    id_vars=["Question", "Method", "Type", "Percent", "Iter"],
    value_vars=["Accuracy", "Mean Percent Bias", "Matthews Correlation"],
    var_name="metric",
    value_name="values",
)

# overall metrics plot #
multiple_lineplots(
    df=df_long,
    metric="values",
    hue="Method",
    grid="metric",
    ncol_legend=4,
    outpath=outpath,
    outname="overall_metrics.png",
)

""" 
MICE (standard) surprisingly bad (MPB, Accuracy). 
missForest best across all metrics. 
"""

# correlations between variables plot #
pairwise_corr = pd.read_csv("evaluation/correlations_study1.csv")
pairwise_corr["corr_delta"] = (
    pairwise_corr["corr_complete"] - pairwise_corr["corr_imputed"]
)
pairwise_corr["corr_delta_abs"] = abs(pairwise_corr["corr_delta"])
pairwise_corr["corr_delta_sq"] = pairwise_corr["corr_delta"] ** 2
pairwise_corr["pairs"] = pairwise_corr["var_x"] + " " + pairwise_corr["var_y"]
pairwise_corr = pairwise_corr.rename(columns={"corr_delta_sq": "Squared Difference"})
single_lineplot(
    df=pairwise_corr,
    metric="Squared Difference",
    hue="Method",
    ncol_legend=4,
    outpath=outpath,
    outname="correlation_difference.png",
)

""" 
miceRF (best), miceCART (second), missForest (third) 
MICE (standard) doing worse than mode is surprising.
"""

# correlation by missingness mechanism
multiple_lineplots(
    df=pairwise_corr,
    grid="Type",
    metric="Squared Difference",
    hue="Method",
    ncol_legend=4,
    outpath=outpath,
    outname="correlation_difference_missingness.png",
)

"""
Surprisingly similar.
"""

# prediction by question (for missForest)
df_long_missforest = df_long[df_long["Method"] == "missForest"]

# map questions
df_questions = pd.read_csv("../data/preprocessed/question_overview.csv")
from constants import question_mapping_study1

df_question_names = pd.DataFrame(
    question_mapping_study1.items(), columns=["question_id", "question_name_short"]
)
df_questions = df_questions.merge(df_question_names, on="question_id", how="inner")
df_questions["question_id"] = df_questions["question_id"].astype(str)
df_questions["question_id"] = "X" + df_questions["question_id"]
df_questions = df_questions.rename(columns={"question_id": "Question"})

# merge with missforest and select questions to display
df_long_missforest = df_long_missforest.merge(df_questions, on="Question", how="inner")
df_long_missforest[["Question", "question_name_short"]].drop_duplicates()

selected_questions = [
    "Afterlife belief",
    "Spirit-body distinction",
    "Supernatural beings",
    "Supreme high god",
    "Supernatural monitoring",
    "Sacrifice adults",
    "Written language",
    "Scriptures",
]

df_long_missforest = df_long_missforest[
    df_long_missforest["question_name_short"].isin(selected_questions)
]

multiple_lineplots(
    df=df_long_missforest,
    metric="values",
    hue="question_name_short",
    grid="metric",
    ncol_legend=2,
    outpath=outpath,
    outname="questions_missforest.png",
)

""" 
Some are almost always yes (supernatural beings) giving:
- no bias
- perfect accuracy
- worst possible MCC (consider just not including these for MCC).
"""

# understanding why some are great and some are bad.
answers_study1 = pd.read_csv("../data/preprocessed/answers_study1.csv")
question_columns = df_questions["Question"].unique().tolist()
answers_study1 = answers_study1[question_columns]
fraction_yes = answers_study1.mean().reset_index(name="fraction_yes")
fraction_yes = fraction_yes.rename(
    columns={"index": "Question", "fraction_yes": "Fraction Yes"}
)
fraction_yes = fraction_yes.merge(df_questions, on="Question", how="inner")
fraction_yes = fraction_yes.sort_values("Fraction Yes", ascending=False)

""" 
some really hard because almost always yes: 
- supernatural beings present: 99.63% yes 
- belief in afterlife: 96.75% yes
- spirit-body distinction: 96.03% yes

some really hard because almost always no:
- adult sacrifice: 2.89% yes 
"""
