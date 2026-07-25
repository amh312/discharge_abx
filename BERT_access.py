#BERT discharge antibiotic prediction

##Packages

from transformers import DistilBertTokenizer, DistilBertForSequenceClassification, set_seed
import torch
from datasets import Dataset
import pandas as pd
import numpy as np
from torch.utils.data import DataLoader
from torch.optim import AdamW
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

###Removal of pts in the test set from the training set
def remove_testpatients(train_df, test_df,key_df):
    train_disc_bertdf2 = train_df.to_pandas()
    test_disc_bertdf2 = test_df.to_pandas()
    train_texts = train_disc_bertdf2['text']
    test_texts = test_disc_bertdf2['text']
    key_df2 = key_df.rename(columns={"pt_text": "text"})
    train_subjects = pd.merge(train_texts, key_df2, on='text', how='left')
    test_subjects = pd.merge(test_texts, key_df2, on='text', how='left')
    train_subjects_filtered = train_subjects[~train_subjects['subject_id'].isin(test_subjects['subject_id'])]
    train_subjects_filtered_texts = train_subjects_filtered['text']
    train_subjects_lost = train_subjects[train_subjects['subject_id'].isin(test_subjects['subject_id'])]
    train_subjects_lost_texts = train_subjects_lost['text']
    train_disc_bertdf2 = train_df.to_pandas()
    train_disc_bertdf_filtered = train_disc_bertdf2[train_disc_bertdf2['text'].isin(train_subjects_filtered_texts)]
    train_disc_bertdf2 = Dataset.from_pandas(train_disc_bertdf_filtered)
    train_disc_bertdf2.set_format(type='torch', columns=['input_ids', 'attention_mask', 'label'])
    return train_disc_bertdf2, train_subjects_lost_texts

###Cleaning
def dfcleanconv(df):
    df2 = df.rename(columns={"access_only": "label"})
    df2 = df2.rename(columns={"pt_text": "text"})
    df2 = df2.dropna(subset=['text'])
    df2 = Dataset.from_pandas(df2)
    return df2

###Model training
def bert_trainer(mod, epochs, opt):

    #ensure MPS being used
    mod=mod.to(device)
    mod.train()

    #iterate over epochs
    for epoch in range(epochs):

        #update message
        print(f"\n{'='*70}")
        print(f"Epoch {epoch + 1}/{epochs}")
        print(f"{'='*70}")

        #baseline loss
        total_loss = 0

        #iterate over batches
        for batch_idx, batch in enumerate(train_loader):

            #reset loss gradient
            opt.zero_grad()

            #get input ids of this batch
            input_ids = batch['input_ids'].to(device)

            #get mask tokens to ignore (e.g., padding)
            attention_mask = batch['attention_mask'].to(device)

            #get actual outcome labels
            label = batch['label'].to(device)

            #model predictions
            outputs = mod(input_ids, attention_mask=attention_mask, labels=label)

            #prediction loss
            loss = outputs.loss

            #backpropagate to calc gradient
            loss.backward()

            #update params
            opt.step()

            #calc and show average loss for batch
            total_loss += loss.item()
            if (batch_idx + 1) % 100 == 0:
                avg_loss_so_far = total_loss / (batch_idx + 1)
                print(f"  Batch {batch_idx + 1:5d}/{len(train_loader)} | Loss: {loss.item():.4f} | Avg Loss: {avg_loss_so_far:.4f}")

        #calc and show average loss for epoch
        avg_loss = total_loss / len(train_loader)
        print(f"\nEpoch {epoch + 1} Complete - Avg Loss: {avg_loss:.4f}\n")

    return mod

###Model testing
def bert_predict(mod):
    # model to evaluation mode
    mod.eval()

    # empty lists
    all_preds = []
    all_labels = []
    all_probs = []

    # turn off gradient tracking
    with torch.no_grad():
        # loop over batches
        for batch in test_loader:
            # get input ids
            input_ids = batch['input_ids'].to(device)

            # get masked tokens
            attention_mask = batch['attention_mask'].to(device)

            # get actual labels
            labels = batch['label'].to(device)

            # get outputs
            outputs = mod(input_ids, attention_mask=attention_mask)

            # get predictions and predicted probabilities
            logits = outputs.logits
            preds = torch.argmax(logits, dim=-1)
            probs = torch.softmax(logits, dim=-1)

            # append to lists
            all_preds.extend(preds.cpu().numpy())
            all_probs.extend(probs.cpu().numpy())
            all_labels.extend(labels.cpu().numpy())

    # convert to arrays
    all_preds = np.array(all_preds)
    all_probs = np.array(all_probs)
    all_labels = np.array(all_labels)

    perfdf = pd.DataFrame(np.column_stack((all_preds, all_probs[:, 1], all_labels)),
                          columns=['pred', 'prob', 'label'])

    return perfdf

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
disc_subjectkey = pd.read_csv("pt_access_only_key.csv")

##Preprocessing

###Clean and convert to Pytorch dataset
disc_bertdf = dfcleanconv(disc_df)

###Tokenise dataset
tokeniser = DistilBertTokenizer.from_pretrained('distilbert-base-uncased')
discdf_tokenised = disc_bertdf.map(tokener, batched=True)
discdf_tokenised.set_format(type='torch', columns=['input_ids', 'attention_mask', 'label'])

###Train-test split
train_disc_bertdf, test_disc_bertdf = ttsplit(discdf_tokenised,0.2)

###Remove test patients from training set
train_disc_bertdf, train_removed = remove_testpatients(train_disc_bertdf, test_disc_bertdf, disc_subjectkey)
train_ref = train_disc_bertdf.to_pandas()
train_ref = train_ref['text']
train_ref.to_csv("ac_train_ref.csv", index=False)
train_removed.to_csv("ac_train_removed.csv", index=False)

###Data loaders
train_loader = DataLoader(train_disc_bertdf, batch_size=16, shuffle=True,num_workers=6)
test_loader = DataLoader(test_disc_bertdf, batch_size=16,num_workers=6)
torch.set_num_threads(10)

##BERT prep

###Set to run on MPS if mac, otherwise run on CPU
device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")

###Load-in pre-trained DistilBERT model and assign to MPS or CPU
discmodel = DistilBertForSequenceClassification.from_pretrained("distilbert-base-uncased", num_labels=2)
discmodel.to(device)

###Set learning rate on ADAMW optimiser
disc_optimiser = AdamW(discmodel.parameters(), lr=2e-5)

##Model training and predictions

###Training
discmodel = bert_trainer(discmodel,3,disc_optimiser)

###Predictions
preds_perf_df = bert_predict(discmodel)
test_disc_bertdf2 = test_disc_bertdf
test_disc_bertdf2.reset_format()
preds_perf_df['text'] = test_disc_bertdf2['text']
preds_perf_df.to_csv("access_bert_preds.csv", index=False)

##Save model and tokeniser

savdirec = "./pt_access_disc_dbert"
discmodel.save_pretrained(savdirec)
tokeniser.save_pretrained(savdirec)

##Record time taken to run the script

end_time = datetime.now()
time_taken = end_time - start_time
time_taken = time_taken.total_seconds()
time_df1 = pd.DataFrame({"Script": ["BERT_access.py"], "Time (secs)": [time_taken]})
time_df = pd.read_csv("script_times.csv")
time_df = pd.concat([time_df, time_df1], ignore_index=True)
time_df.to_csv("script_times.csv", index=False)

###Check time taken to run a single prediction
discmodel = DistilBertForSequenceClassification.from_pretrained("./pt_access_disc_dbert", num_labels=2)
discmodel.to(device)
sample_row = test_disc_bertdf.shuffle(seed=123).select(range(1))
test_loader = DataLoader(sample_row, batch_size=1)
predict_start_time = datetime.now()
test_model = bert_predict(discmodel)
predict_end_time = datetime.now()
time_taken = predict_end_time - predict_start_time
print("Time taken for a single prediction (seconds): ", time_taken.total_seconds())
