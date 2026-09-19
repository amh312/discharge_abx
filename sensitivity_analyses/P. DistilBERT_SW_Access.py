#P. DistilBERT_SW_Access.py

#This is a duplicate of DistilBERT_SW_overall.py but for the Access model. As for that script, a prediction-outcome
#dataframe is produced is used by a companion performance assessment script R. DistilBERT_SW_Access_perf.R.

###############################################
###############################################

##Packages

###transformers v5.4.0
from transformers import DistilBertTokenizer, DistilBertForSequenceClassification, set_seed

###datasets v4.8.4
from datasets import Dataset

###pandas v3.0.1
import pandas as pd

###numpy v1.26.4
import numpy as np

###torch v2.11.0
import torch
from torch.utils.data import DataLoader
from torch.optim import AdamW

import random
from datetime import datetime

###############################################
###############################################

##Initialise script timer

start_time = datetime.now()

###############################################
###############################################

##Functions

###Tokeniser function for mapping (whole-document, used only for truncation counting)
def tokener(examples):
    return tokeniser(examples["text"], padding="max_length", truncation=True)

###Truncation count
def count_tokens_untruncated(examples):
    return {'num_tokens': [len(ids) for ids in tokeniser(examples['text'], truncation=False)['input_ids']]}

###Chunking tokeniser - splits documents > 512 tokens into overlapping windows
def chunk_tokener(examples, stride=64, max_length=512):
    tokenised = tokeniser(
        examples["text"],
        truncation=True,
        max_length=max_length,
        stride=stride,
        return_overflowing_tokens=True,
        padding="max_length",
    )
    sample_map = tokenised.pop("overflow_to_sample_mapping")
    return {
        "input_ids": tokenised["input_ids"],
        "attention_mask": tokenised["attention_mask"],
        "label": [examples["label"][i] for i in sample_map],
        "doc_id": [examples["doc_id"][i] for i in sample_map],
        "text": [examples["text"][i] for i in sample_map],
    }

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

###Model testing - chunk level (one row per chunk, not per document)
def bert_predict(mod, loader):
    # model to evaluation mode
    mod.eval()

    # empty lists
    all_preds = []
    all_labels = []
    all_probs = []

    # turn off gradient tracking
    with torch.no_grad():
        # loop over batches
        for batch in loader:
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

###Aggregate chunk-level predictions to document level using max probability
def aggregate_max_prob(chunk_perf_df, doc_order, doc_text_lookup):
    chunk_perf_df = chunk_perf_df.copy()
    idx = chunk_perf_df.groupby('doc_id')['prob'].idxmax()
    agg_df = chunk_perf_df.loc[idx].set_index('doc_id')
    agg_df = agg_df.reindex(doc_order)
    agg_df['text'] = [doc_text_lookup[d] for d in doc_order]
    agg_df = agg_df.reset_index(drop=True)
    agg_df = agg_df[['pred', 'prob', 'label', 'text']]
    return agg_df

###############################################
###############################################

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

###############################################
###############################################

##Read in

disc_df = pd.read_csv("pt_access_only.csv")
disc_subjectkey = pd.read_csv("pt_access_only_key.csv")

###############################################
###############################################

##Preprocessing

###Clean and convert to Pytorch dataset
disc_bertdf = dfcleanconv(disc_df)

###Assign a stable per-document id before any splitting/chunking to allow tracing to parent doc
disc_bertdf = disc_bertdf.add_column("doc_id", list(range(len(disc_bertdf))))

###Tokeniser
tokeniser = DistilBertTokenizer.from_pretrained('distilbert-base-uncased')

###Count truncations (document level, pre-split, for reporting only)
discdf_token_no = disc_bertdf.map(count_tokens_untruncated, batched=True)
discdf_token_no = discdf_token_no.to_pandas()
discdf_token_no = discdf_token_no[['text', 'num_tokens']]
discdf_token_no['truncated'] = discdf_token_no['num_tokens'] > 512
discdf_token_no.to_csv("access_disc_token_no.csv", index=False)
truncation_count = discdf_token_no['truncated'].sum()
print(truncation_count)

###Train-test split - performed on whole (unchunked) documents to avoid leakage
train_disc_bertdf, test_disc_bertdf = ttsplit(disc_bertdf,0.2)

###Remove test patients from training set (still at document level)
train_disc_bertdf, train_removed = remove_testpatients(train_disc_bertdf, test_disc_bertdf, disc_subjectkey)
train_ref = train_disc_bertdf.to_pandas()
train_ref = train_ref['text']
train_ref.to_csv("ac_train_ref.csv", index=False)
train_removed.to_csv("ac_train_removed.csv", index=False)

###Document-level order/labels of the test set, used later to reassemble
test_doc_order = test_disc_bertdf['doc_id']
test_doc_text_lookup = dict(zip(test_disc_bertdf['doc_id'], test_disc_bertdf['text']))

###Chunk documents into overlapping 512-token windows
train_disc_bertdf_chunked = train_disc_bertdf.map(
    chunk_tokener, batched=True, remove_columns=train_disc_bertdf.column_names
)
test_disc_bertdf_chunked = test_disc_bertdf.map(
    chunk_tokener, batched=True, remove_columns=test_disc_bertdf.column_names
)

###Keep a record of which doc_id each test chunk belongs to
test_chunk_doc_ids = list(test_disc_bertdf_chunked['doc_id'])

###Confirm no document's chunks leaked across the train/test boundary
train_chunk_doc_ids = set(train_disc_bertdf_chunked['doc_id'])
test_chunk_doc_ids_set = set(test_chunk_doc_ids)
overlap_doc_ids = train_chunk_doc_ids & test_chunk_doc_ids_set
if overlap_doc_ids:
    raise ValueError(
        f"Data leakage detected: {len(overlap_doc_ids)} doc_id(s) present in "
        f"both train and test chunk sets: {sorted(overlap_doc_ids)}"
    )
print(f"Leakage check passed: no doc_id overlap between train ({len(train_chunk_doc_ids)} docs) "
      f"and test ({len(test_chunk_doc_ids_set)} docs) chunk sets.")

###Restrict tensor columns used by the model/data loaders
train_disc_bertdf_chunked.set_format(type='torch', columns=['input_ids', 'attention_mask', 'label'])
test_disc_bertdf_chunked.set_format(type='torch', columns=['input_ids', 'attention_mask', 'label'])

###Data loaders
train_loader = DataLoader(train_disc_bertdf_chunked, batch_size=16, shuffle=True,num_workers=6)
test_loader = DataLoader(test_disc_bertdf_chunked, batch_size=16,num_workers=6)
torch.set_num_threads(10)

###############################################
###############################################

##BERT prep

###Set to run on MPS if mac, otherwise run on CPU
device = torch.device("mps" if torch.backends.mps.is_available() else "cpu")

###Load-in pre-trained DistilBERT model and assign to MPS or CPU
discmodel = DistilBertForSequenceClassification.from_pretrained("distilbert-base-uncased", num_labels=2)
discmodel.to(device)

###Set learning rate on ADAMW optimiser
disc_optimiser = AdamW(discmodel.parameters(), lr=2e-5)

###############################################
###############################################

##Model training and predictions

###Training (each chunk is treated as an independent training example inheriting parent doc label
discmodel = bert_trainer(discmodel,3,disc_optimiser)

###Chunk-level predictions
chunk_preds_perf_df = bert_predict(discmodel, test_loader)
chunk_preds_perf_df['doc_id'] = test_chunk_doc_ids

###Aggregate chunks back to one row per document (max-probability rule), same order as original test set
preds_perf_df = aggregate_max_prob(chunk_preds_perf_df, test_doc_order, test_doc_text_lookup)
preds_perf_df.to_csv("access_bert_preds_chunked.csv", index=False)

###Save model and tokeniser

savdirec = "./pt_access_disc_dbert_chunked"
discmodel.save_pretrained(savdirec)
tokeniser.save_pretrained(savdirec)

###############################################
###############################################

##Record time taken to run the script

end_time = datetime.now()
time_taken = end_time - start_time
time_taken = time_taken.total_seconds()
time_df1 = pd.DataFrame({"Script": ["P. DistilBERT_SW_Access.py"], "Time (secs)": [time_taken]})
time_df = pd.read_csv("script_times.csv")
time_df = pd.concat([time_df, time_df1], ignore_index=True)
time_df.to_csv("script_times.csv", index=False)

###Check time taken to run a single prediction on the longest chunk in the test set
discmodel = DistilBertForSequenceClassification.from_pretrained("./pt_access_disc_dbert_chunked", num_labels=2)
discmodel.to(device)
sample_row = (
    test_disc_bertdf_chunked
    .map(lambda x: {"input_len": len(x["input_ids"])})
    .sort("input_len", reverse=True)
    .select(range(1))
    .remove_columns("input_len")
)
sample_row.set_format(type="torch", columns=["input_ids", "attention_mask", "label"])
sample_loader = DataLoader(sample_row, batch_size=1)
predict_start_time = datetime.now()
test_model = bert_predict(discmodel, sample_loader)
predict_end_time = datetime.now()
time_taken = predict_end_time - predict_start_time
print("Time taken for a single prediction (seconds): ", time_taken.total_seconds())
time_df2 = pd.DataFrame({"Script": ["Access model single prediction (chunked)"], "Time (secs)": [time_taken.total_seconds()]})
time_df2.to_csv("access_single_prediction_chunked.csv", index=False)
