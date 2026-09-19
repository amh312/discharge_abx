#W. BOW_overall&Access.py

#This python script trains and tests a bag-of-words (BOW) model with TF-IDF vectorisation and logistic regression as a comparator to DistilBERT. Given the much
#faster runtime, both models are trained in the same script using scikit-learn and hyperparameter tuning with random search cv is also performed. As for other
#model scripts, outputs are dataframes with paired predictions and outcome labels.

###############################################
###############################################

##Packages

###sklearn version 1.8.0
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
from sklearn.model_selection import RandomizedSearchCV

###scipy version 1.17.1
from scipy.stats import loguniform

###pandas version 3.0.1
import pandas as pd

###numpy version 1.26.4
import numpy as np

###joblib version 1.5.3
import joblib

import random
from datetime import datetime
import os

###############################################
###############################################

##Initialise script timer

start_time = datetime.now()

###############################################
###############################################

##Functions

###Cleaning
def dfcleanconv(df):

    #rename columns
    df2 = df.rename(columns={"ab_on_disc": "label"})
    df2 = df2.rename(columns={"pt_text": "text"})

    #drop rows with missing text
    df2 = df2.dropna(subset=['text'])

    #reset index
    df2 = df2.reset_index(drop=True)

    return df2

###Build train/test dataframes matching a DistilBERT run split
def build_matched_split(train_ref_df, bert_preds_df, full_df):

    #rename train reference dataframe to match full dataframe
    train_texts = train_ref_df.rename(columns={train_ref_df.columns[0]: "text"})

    #merge train reference dataframe with full dataframe to get labels for training
    train_df = pd.merge(train_texts, full_df[['text', 'label']], on='text', how='left')

    #drop train rows with missing labels and reset index
    train_df = train_df.dropna(subset=['label']).reset_index(drop=True)

    #label to integer type (train)
    train_df['label'] = train_df['label'].astype(int)

    #create test dataframe, dropping duplicates and resetting index
    test_df = bert_preds_df[['text', 'label']].drop_duplicates(subset=['text']).reset_index(drop=True)

    #label to integer type (test)
    test_df['label'] = test_df['label'].astype(int)

    return train_df, test_df

###Model training
def bow_trainer(train_df, tune=False, param_distributions=None, n_iter=30, cv=5, scoring="roc_auc", n_jobs=-1, random_state=123):

    #outcome label
    y_train = train_df['label'].astype(int)

    #no hyperparameter tuning option with standard settings
    if not tune:

        #TF-IDF vectoriser and logistic regression model
        vectoriser = TfidfVectorizer(max_features=20000, min_df=5)

        #features
        X_train = vectoriser.fit_transform(train_df['text'])

        #LR model
        mod = LogisticRegression(max_iter=1000)
        mod.fit(X_train, y_train)

        return mod, vectoriser, None

    ###Default search space: TF-IDF vocabulary/n-gram settings and logistic

    #set default hyperparameter distributions
    if param_distributions is None:
        param_distributions = {

            #max number of features in the TF-IDF vocabulary
            "tfidf__max_features": [10000, 20000, 40000],

            #minimum document frequency for a token to be included
            "tfidf__min_df": [2, 5],

            #n-gram range (unigrams or unigrams+bigrams)
            "tfidf__ngram_range": [(1, 1), (1, 2)],

            #LR regularisation strength (log uniform to explore 0.01 to 100)
            "clf__C": loguniform(1e-3, 1e2),

            #class weighting
            "clf__class_weight": [None, "balanced"],
        }

    #TF-IDF and LR pipeline for cross-validation
    pipeline = Pipeline([
        ("tfidf", TfidfVectorizer()),
        ("clf", LogisticRegression(max_iter=1000)),
    ])

    #randomised search cross-validation
    search = RandomizedSearchCV(
        pipeline,
        param_distributions=param_distributions,
        n_iter=n_iter,
        cv=cv,
        scoring=scoring,
        n_jobs=n_jobs,
        refit=True,
        random_state=random_state,
    )
    search.fit(train_df['text'], y_train)

    #cv performance and best hyperparameters
    print(f"Best {scoring} (CV): {search.best_score_:.4f}")
    print(f"Best params: {search.best_params_}")

    #update best TF-IDF/LR pipeline and extract model and vectoriser
    best_pipeline = search.best_estimator_
    mod = best_pipeline.named_steps["clf"]
    vectoriser = best_pipeline.named_steps["tfidf"]

    #create dataframe of the cross-validation results
    cv_results_df = pd.DataFrame(search.cv_results_)

    return mod, vectoriser, cv_results_df

###Feature importance
def bow_feature_importance(mod, vectoriser, top_n=30):

    #get feature names and coefficients
    feature_names = vectoriser.get_feature_names_out()
    coefs = mod.coef_[0]

    #create dataframe of feature importance
    importance_df = pd.DataFrame({
        'feature': feature_names,
        'coefficient': coefs,
    }).sort_values('coefficient', ascending=False).reset_index(drop=True)

    #get top positive and negative features
    top_positive = importance_df.head(top_n).reset_index(drop=True)
    top_negative = importance_df.tail(top_n).sort_values('coefficient').reset_index(drop=True)

    return importance_df, top_positive, top_negative

###Model testing
def bow_predict(mod, vectoriser, test_df):

    #transform test data using the fitted vectoriser
    X_test = vectoriser.transform(test_df['text'])

    #make predictions and get probabilities
    preds = mod.predict(X_test)
    probs = mod.predict_proba(X_test)[:, 1]

    #create dataframe of predictions, probabilities, and true labels
    labels = test_df['label'].astype(int).to_numpy()
    perfdf = pd.DataFrame({
        'pred': preds,
        'prob': probs,
        'label': labels,
    })

    return perfdf

###############################################
###############################################

##Seeds

###Random
random.seed(123)
np.random.seed(123)

###############################################
###############################################

##Read in

###Original (all-antibiotics-eligible) cohort
disc_df = pd.read_csv("pt_orig.csv")
train_ref = pd.read_csv("train_ref.csv")
bert_preds = pd.read_csv("bert_preds.csv")

###Access-only cohort
disc_access_df = pd.read_csv("pt_access_only.csv")
ac_train_ref = pd.read_csv("ac_train_ref.csv")
access_bert_preds = pd.read_csv("access_bert_preds.csv")

###############################################
###############################################

##Preprocessing

###Clean
disc_bowdf = dfcleanconv(disc_df)
disc_access_bowdf = dfcleanconv(disc_access_df.rename(columns={"access_only": "label"}))

###Build train/test splits matching BERT_discharges.py and BERT_access.py
train_disc_bowdf, test_disc_bowdf = build_matched_split(train_ref, bert_preds, disc_bowdf)
train_ac_bowdf, test_ac_bowdf = build_matched_split(ac_train_ref, access_bert_preds, disc_access_bowdf)

###############################################
###############################################

##Model training and predictions

###Hyperparameter tuning on
TUNE_HYPERPARAMETERS = True

###Overall model

####Training
bowmodel, bow_vectoriser, bow_cv_results = bow_trainer(train_disc_bowdf, tune=TUNE_HYPERPARAMETERS)
if bow_cv_results is not None:
    bow_cv_results.to_csv("bow_cv_results.csv", index=False)

####Predictions
preds_perf_df = bow_predict(bowmodel, bow_vectoriser, test_disc_bowdf)
preds_perf_df['text'] = test_disc_bowdf['text']
preds_perf_df.to_csv("bow_preds.csv", index=False)

####Save model and vectoriser
savdirec = "./pt_disc_bow"
os.makedirs(savdirec, exist_ok=True)
joblib.dump(bowmodel, os.path.join(savdirec, "model.joblib"))
joblib.dump(bow_vectoriser, os.path.join(savdirec, "vectoriser.joblib"))

####Feature importance
bow_importance_df, bow_top_positive, bow_top_negative = bow_feature_importance(bowmodel, bow_vectoriser)
bow_importance_df.to_csv("bow_feature_importance.csv", index=False)
bow_top_positive.to_csv("bow_feature_importance_top_positive.csv", index=False)
bow_top_negative.to_csv("bow_feature_importance_top_negative.csv", index=False)

###Access model

####Training
ac_bowmodel, ac_bow_vectoriser, ac_bow_cv_results = bow_trainer(train_ac_bowdf, tune=TUNE_HYPERPARAMETERS)
if ac_bow_cv_results is not None:
    ac_bow_cv_results.to_csv("access_bow_cv_results.csv", index=False)

####Predictions
ac_preds_perf_df = bow_predict(ac_bowmodel, ac_bow_vectoriser, test_ac_bowdf)
ac_preds_perf_df['text'] = test_ac_bowdf['text']
ac_preds_perf_df.to_csv("access_bow_preds.csv", index=False)

####Save model and vectoriser
ac_savdirec = "./pt_disc_bow_access"
os.makedirs(ac_savdirec, exist_ok=True)
joblib.dump(ac_bowmodel, os.path.join(ac_savdirec, "model.joblib"))
joblib.dump(ac_bow_vectoriser, os.path.join(ac_savdirec, "vectoriser.joblib"))

####Feature importance
ac_bow_importance_df, ac_bow_top_positive, ac_bow_top_negative = bow_feature_importance(ac_bowmodel, ac_bow_vectoriser)
ac_bow_importance_df.to_csv("access_bow_feature_importance.csv", index=False)
ac_bow_top_positive.to_csv("access_bow_feature_importance_top_positive.csv", index=False)
ac_bow_top_negative.to_csv("access_bow_feature_importance_top_negative.csv", index=False)

###############################################
###############################################

##Record time taken to run the script
end_time = datetime.now()
time_taken = end_time - start_time
time_taken = time_taken.total_seconds()
time_df1 = pd.DataFrame({"Script": ["W. BOW_overall&Access.py"], "Time (secs)": [time_taken]})
time_df = pd.read_csv("script_times.csv")
time_df = pd.concat([time_df, time_df1], ignore_index=True)
time_df.to_csv("script_times.csv", index=False)

###Check time taken to run a single prediction on the longest text in the (original cohort) test set
bowmodel = joblib.load(os.path.join(savdirec, "model.joblib"))
bow_vectoriser = joblib.load(os.path.join(savdirec, "vectoriser.joblib"))
sample_row = test_disc_bowdf.copy()
sample_row['text_len'] = sample_row['text'].str.len()
sample_row = sample_row.sort_values('text_len', ascending=False).head(1).drop(columns=['text_len'])
predict_start_time = datetime.now()
test_model = bow_predict(bowmodel, bow_vectoriser, sample_row)
predict_end_time = datetime.now()
time_taken = predict_end_time - predict_start_time
print("Time taken for a single prediction (seconds): ", time_taken.total_seconds())
time_df2 = pd.DataFrame({"Script": ["BOW single prediction"], "Time (secs)": [time_taken.total_seconds()]})
time_df2.to_csv("bow_overall_single_prediction.csv", index=False)
