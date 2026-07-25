#Exercise feature and SHAP analysis (Access)

from transformers import DistilBertTokenizer, DistilBertForSequenceClassification, DistilBertConfig, set_seed
import torch
from datasets import Dataset
import pandas as pd
import numpy as np
import torch.nn.functional as F
import shap
import random
from datetime import datetime

##Initialise script timer

start_time = datetime.now()

##Functions

###Tokeniser function for mapping
def tokener(examples):
    return tokeniser(examples["text"], padding="max_length", truncation=True)

###Train-test split
def ttsplit(token_data,testsize):
    split_disc_bertdf = token_data.train_test_split(test_size=testsize)
    traindf = split_disc_bertdf['train']
    testdf = split_disc_bertdf['test']
    return traindf, testdf

###Cleaning
def dfcleanconv(df):
    df2 = df.rename(columns={"access_only": "label"})
    df2 = df2.rename(columns={"pt_text": "text"})
    df2 = df2.dropna(subset=['text'])
    df2 = Dataset.from_pandas(df2)
    return df2

###SHAP value calculation
def shapmaker(model_location,testdf):

    # mps if mac, otherwise run on cpu
    device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")

    # read in model config
    disc_config = DistilBertConfig.from_pretrained(model_location, output_attentions=True)

    # initialise tokeniser and model
    tokeniser = DistilBertTokenizer.from_pretrained(model_location)
    discmodel = DistilBertForSequenceClassification.from_pretrained(model_location, config=disc_config)
    discmodel.to(device)

    # model to evaluation mode
    discmodel.eval()

    #initialise probability prediction function
    def predict_proba(texts):
        # convert discharge summaries to list
        if isinstance(texts, np.ndarray):
            texts = texts.tolist()

        # iterate over list of discharge summaries
        texts = [str(t) for t in texts]

        encodings = tokeniser(
            texts,
            truncation=True,
            padding=True,
            max_length=512,
            return_tensors="pt"
        )
        encodings = {k: v.to(device) for k, v in encodings.items()}

        with torch.no_grad():
            outputs = discmodel(**encodings)
            probs = F.softmax(outputs.logits, dim=-1)
        return probs.cpu().numpy()

    # initialise token masker
    masker = shap.maskers.Text(tokeniser)

    # initialise shap explainer
    explainer = shap.Explainer(predict_proba, masker)

    # remove torch formatting from df for explainer
    testdf.set_format(type=None)

    # convert df to list of discharge summaries for explainer
    testdf2 = [testdf[i]['text'] for i in range(len(testdf))]

    # run explainer to get shap values
    shap_values = explainer(testdf2)

    # put test df back into torch format
    testdf.set_format(type='torch', columns=['input_ids', 'attention_mask', 'label'])

    return shap_values, testdf

###SHAP value extraction to df
def shap_extractor(shapdf):
    token_importance = {}
    for i in range(len(shapdf)):
        print(i)

        tokens = shapdf[i].data
        vals = shapdf[i].values

        abs_vals = np.abs(vals).sum(axis=1)

        for token, val in zip(tokens, abs_vals):
            token_importance[token] = token_importance.get(token, 0) + val

    token_df = pd.DataFrame(list(token_importance.items()), columns=['token', 'total_abs_shap'])
    token_df = token_df.sort_values(by='total_abs_shap', ascending=False)

    return token_df

###Fictional discharge summary text plot generator
def madeup_shap(text_madeup,savfile_name):

    ###Fictional discharge letter
    madeup_text = text_madeup

    ###Tokenise discharge letter
    madeup_df = pd.DataFrame({"text": [madeup_text], "label": [0]})
    madeup_dataset = Dataset.from_pandas(madeup_df)
    madeup_dataset = madeup_dataset.map(tokener, batched=True)
    madeup_dataset.set_format(type="torch", columns=["input_ids", "attention_mask", "label"])

    ###Get SHAP values from fictional discharge letter
    madeup_shapvalues, _ = shapmaker(savdirec, madeup_dataset)

    ###Text plot
    html_str = shap.plots._text.text(madeup_shapvalues[0][:, 1], display=False)

    ###Save to html file
    with open(savfile_name, "w") as f:
        f.write(html_str)

##Seeds

###Random
random.seed(123)
np.random.seed(123)

###Pytorch
torch.manual_seed(123)
torch.cuda.manual_seed_all(123)
torch.backends.cudnn.deterministic = True
torch.backends.cudnn.benchmark = False

###Other
set_seed(123)

##Read in

disc_df = pd.read_csv("pt_access_only.csv")

###Filter

###Model location
savdirec = "./pt_access_disc_dbert"

##Preprocessing

###Clean and convert to Pytorch dataset
disc_bertdf = dfcleanconv(disc_df)

###Tokenise dataset
tokeniser = DistilBertTokenizer.from_pretrained(savdirec)
discdf_tokenised = disc_bertdf.map(tokener, batched=True)
discdf_tokenised.set_format(type='torch', columns=['input_ids', 'attention_mask', 'label'])

###Train-test split
train_disc_bertdf, test_disc_bertdf = ttsplit(discdf_tokenised,0.2)

##Get SHAP values from discharge letters used in questionnaire exercise

###Make blank df for storing shap values for all examples
shapvalues_full = []

###Iterate over clinician questionnaires
for j in range(6):

    print(j)

    ###Read in questionnaire df
    filename = f"ac_r_{j+1}.csv"
    r_df = pd.read_csv(filename)

    ###Filter test dataset to letters in questionnaire
    r_texts = set(r_df['question'])
    shap_bertdf = test_disc_bertdf.filter(lambda x: x['text'] in r_texts)

    ###SHAP value calculation
    shapvalues, shap_bertdf = shapmaker(savdirec,shap_bertdf)

    ##Append shap values from this questionnaire to full list
    shapvalues_full.append(shapvalues)

    ###Text plots for questionnaire discharge letters
    for i in range(len(shapvalues)):
        html_str = shap.plots._text.text(shapvalues[i][:, 1], display=False)
        filename2 = f"ac_shap_text_plot_{filename}_{i+1}.html"
        with open(filename2, "w") as f:
            f.write(html_str)

    ###Extract all SHAP values across questionnaires to df for bar charts
    disc_shapdf1 = shap_extractor(shapvalues)
    disc_shapdf1.to_csv(f'access_shaptokens_df_{j}.csv', index=False)

##Fictional discharge summaries for manuscript text plot figures

###Appropriate Watch antibiotic
madeup_shap("Dear Mr ___, it was a pleasure looking after you during your hospital stay. You were admitted with cough, chest pain, and shortness of breath after your recent knee surgery. You were diagnosed with an irregular heart beat, a heart attack and pneumonia. You underwent a procedure to unblock your heart arteries and were given blood thinners and an antibiotic, which you should continue to take at home to complete the course. We have temporarily discontinued your atorvastatin, which you should restart in 1 week. You should attend your orthopaedic appointment in 2 weeks and your cardiology appointment in 1 month. Please contact your GP if you develop any new symptoms or have any concerns. We wish you a speedy recovery.", "shap_text_plot_madeup_ac.html")

###Inappropriate Watch antibiotic
madeup_shap("Dear Mr ___, it was a pleasure looking after you during your hospital stay. You were admitted with chest pain and leg swelling after your recent knee surgery. You were diagnosed with an irregular heart beat, a heart attack and a skin infection. You underwent a procedure to unblock your heart arteries and were given blood thinners and an antibiotic, which you should continue to take at home to complete the course. We have temporarily discontinued your atorvastatin, which you should restart in 1 week. You should attend your orthopaedic appointment in 2 weeks and your cardiology appointment in 1 month. Please contact your GP if you develop any new symptoms or have any concerns. We wish you a speedy recovery.", "shap_text_plot_madeup2_ac.html")

##Record time taken to run the script

end_time = datetime.now()
time_taken = end_time - start_time
time_taken = time_taken.total_seconds()
time_df1 = pd.DataFrame({"Script": ["BERT_SHAP_Access.py"], "Time (secs)": [time_taken]})
time_df = pd.read_csv("script_times.csv")
time_df = pd.concat([time_df, time_df1], ignore_index=True)
time_df.to_csv("script_times.csv", index=False)
