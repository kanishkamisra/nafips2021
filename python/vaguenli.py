import csv
import os
import argparse

from tqdm import tqdm

import torch
from torch.utils.data import DataLoader

from minicons.minicons import supervised

parser = argparse.ArgumentParser()
parser.add_argument("--dataset", type = str)
parser.add_argument("--model", default = 'roberta-large-mnli', type = str)
parser.add_argument("--batchsize", default = 64, type = int)
parser.add_argument("--device", default = 'cpu', type = str)

args = parser.parse_args()

inpath = args.dataset
model_name = args.model
batch_size = args.batchsize
device = args.device

components = inpath.split("/")
data_dir = "/".join(components[0:-1])
dataset_name = components[-1].split(".")[0]

dataset = []
premises = []
hypotheses = []
with open(args.dataset, "r") as f:
    reader = csv.DictReader(f)
    column_names = reader.fieldnames
    for row in reader:
        premises.append(row['premise'])
        hypotheses.append(row['hypothesis'])
        dataset.append(list(row.values()))

stimuli = [f"{p} </s></s> {h}" for p, h in zip(premises, hypotheses)]
stimuli_loader = DataLoader(stimuli, batch_size = batch_size)

model = supervised.SupervisedHead(model_name, device)

if "/" in model_name:
    model_name = model_name.replace("/", "_")

results_file = f"{data_dir}/results/{model_name}__{dataset_name}.csv"

contradiction = []
neutral = []
entailment = []
for batch in tqdm(stimuli_loader):
    logits = model.logits(batch, verbose = True)
    contradiction.extend(logits['CONTRADICTION'])
    neutral.extend(logits['NEUTRAL'])
    entailment.extend(logits['ENTAILMENT'])

dataset = list(zip(*dataset))
dataset.append(entailment)
dataset.append(contradiction)
dataset.append(neutral)
dataset.append([model_name] * len(contradiction))

column_names += ["entailment", "contradiction", "neutral", "model"]

with open(results_file, "w") as f:
    writer = csv.writer(f)
    writer.writerow(column_names)
    writer.writerows(list(zip(*dataset)))


