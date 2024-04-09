#import sys
#print(f"Python values Arg6: {sys.argv[6][0] + sys.argv[6][1:len(sys.argv[6])].lower()}, Arg7: {sys.argv[7][0] + sys.argv[7][1:len(sys.argv[7])].lower()}")
#print(f"Python types Arg6: {type(eval(sys.argv[6][0] + sys.argv[6][1:len(sys.argv[6])].lower()))}, Arg7: {type(eval(sys.argv[7][0] + sys.argv[7][1:len(sys.argv[7])].lower()))}")
#raise Exception("stop!")
import pandas as pd
import pyreadr
import os
import time
import numpy as np
import json
from keras.models import Sequential
from keras.layers import Dense
from sklearn.preprocessing import LabelEncoder
from keras.utils import to_categorical
from sklearn.metrics import accuracy_score, classification_report
import pickle
from  keras.callbacks import TensorBoard, LearningRateScheduler
from keras.optimizers import SGD, Adam
import math
import sys

# Seting up input and output
#model_name: str = "nn_600_300_np"
#output_path: str = "../OldDataOutput"
#data_path: str = "../OldData"

model_name: str = sys.argv[1] # Second parameter when executed from the terminal. (the first is the the name fo the file being executed)
output_path: str = sys.argv[2]
data_path: str = sys.argv[3]
newas = eval(sys.argv[4])
N = eval(sys.argv[5])
original_test_classes = eval(sys.argv[6][0] + sys.argv[6][1:len(sys.argv[6])].lower())
original_test = ["A IDH", "A IDH, HG", "ANA PA", "ATRT, MYC", "ATRT, SHH", "ATRT, TYR", "CHORDM", "CN", "CNS NB, FOXR2", "DLGNT", "DMG, K27", "ENB, A", "EPN, MPE", "EPN, PF A", "EPN, PF B", "EPN, RELA", "EPN, SPINE", "ETMR", "EWS", "GBM, G34", "GBM, MES", "GBM, MID", "GBM, MYCN", "GBM, RTK I", "GBM, RTK II", "GBM, RTK III", "HGNET, BCOR", "HGNET, MN1", "HMB", "IHG", "LGG, DNT", "LGG, GG", "LGG, MYB", "LGG, PA MID", "LGG, PA PF", "LGG, PA/GG ST", "LGG, RGNT", "LGG, SEGA", "LIPN", "LYMPHO", "MB, G3", "MB, G4", "MB, SHH CHL AD", "MB, SHH INF", "MB, WNT", "MELAN", "MELCYT", "MNG", "O IDH", "PGG, nC", "PIN T, PB B", "PIN T, PPT", "PITAD, ACTH", "PITAD, FSH LH", "PITAD, STH DNS B", "PITAD, TSH", "PITUI", "PLEX, AD", "PLEX, PED A", "PLEX, PED B", "PTPR, A", "PTPR, B", "PXA", "SCHW", "SCHW, MEL", "SFT HMPC", "SUBEPN, PF", "SUBEPN, SPINE", "SUBEPN, ST"]
save_models = eval(sys.argv[7][0] + sys.argv[7][1:len(sys.argv[7])].lower())

# Seting up model
epochs=60
learning_rate = 0.001
#decay_rate = learning_rate / epochs
#momentum = 0.8

# Setting optimizer
#sgd = SGD(learning_rate=learning_rate) # decay=decay_rate,  momentum=momentum,

# Dfining balenced sampling method
from collections import Counter

def balanced_sample(input_vect, n, residual_method="small_nb_first"):
    nb_classes = len(set(input_vect))

    if nb_classes > n:
        np.random.seed(1)
        return np.random.choice(np.arange(len(input_vect)), n, replace=False)

    take_nb = n // nb_classes
    out = []
    free_space = n - take_nb * nb_classes

    if residual_method == "small_nb_first":
        class_order = sorted(Counter(input_vect).items(), key=lambda x: x[1])

    out = []
    for class_name, count in class_order:
        indexes = np.where(np.array(input_vect) == class_name)[0]

        if len(indexes) >= take_nb:
            out.extend(indexes[:take_nb])
        else:
            out.extend(indexes)
            free_space += take_nb - len(indexes)

    #free_space += sum((take_nb - len(np.where(np.array(input_vect) == class_name)[0])) for class_name, count in class_order)

    remaining_indexes = [i for i in range(len(input_vect)) if i not in out]
    np.random.shuffle(remaining_indexes)

    for i in remaining_indexes:
        if free_space == 0:
            break
        if input_vect[i] not in input_vect[out]:
            out.append(i)
            free_space -= 1

    return out





# Setting learning rate
# Modified from https://keras.io/api/callbacks/learning_rate_scheduler/
def scheduler(epoch, lr):
    return lr * math.exp(-0.1)


lrSheduler = LearningRateScheduler(scheduler)


model_params = {"epochs":epochs, "callbacks":[lrSheduler]}


out = {}
#newas = [10, 20, 30, 50, 100, 200, 300, 500, 1000, 2000, 3000, 5000, 10000]
#newas = eval(sys.argv[4])

#N = [10, 20, 30, 50, 100, 200, 300, 500, 1000, 1600]
#N = eval(sys.argv[5])

 # Set model name
out[model_name] = model_name

 # The N sequence to use as reference
out['n'] = N

if not os.path.isdir(output_path):
   os.mkdir(output_path)

if not os.path.isdir(f"{output_path}/{model_name}"):
   os.mkdir(f"{output_path}/{model_name}")

if not os.path.isdir(f"{output_path}/{model_name}/Models") and save_models:
   os.mkdir(f"{output_path}/{model_name}/Models")


for i in newas:
  res_pred_train = []
  res_pred_test = []
  res_time = {}

  # Setting encoders
  labEncode = LabelEncoder()
  
  # Geting training data
  train = pyreadr.read_r(f"{data_path}/df_newas{i}_train.rds")[None]

  # Used to balenced sample selection
  train_lab_ref = train["meth_class"].copy(deep = True)

  # Geting testting data
  test = pyreadr.read_r(f"{data_path}/df_newas{i}_test.rds")[None]

  # Remove classes that are not present in both datasets
  if len(set(train['meth_class'])) != len(set(test['meth_class'])):
    shared_classes = set(train['meth_class']).intersection(set(test['meth_class']))

    train = train[train['meth_class'].isin(shared_classes)]
    test = test[test['meth_class'].isin(shared_classes)]

  if original_test_classes:
     train = train[train['meth_class'].isin(original_test)]
     test = test[test['meth_class'].isin(original_test)]


  # Formating labels for training set
  label_train = to_categorical(labEncode.fit_transform(train["meth_class"]))
  train = train.drop("meth_class", axis=1)

  # Formating labels for test set
  label_test = to_categorical(labEncode.fit_transform(test["meth_class"]), num_classes = label_train.shape[1])
  test = test.drop("meth_class", axis=1)

  
  
  for n in N:
    print("newas:", i, " | N:", n, end="")     

    # Genrate semi random number out of nb observation in train, sample n of them
    np.random.seed(1)
    n = min(n, train.shape[0])
    rand = np.random.choice(train.shape[0],n,replace=False)
    #rand = balanced_sample(train_lab_ref, n)

    # Creating the training set of size n
    sample_train = train.iloc[rand]
    sample_label_train = label_train[rand]

    # Formating the labels in dummy matrix
    #label_train = to_categorical(labEncode.fit_transform(sample_train["meth_class"]), num_classes=87)
    #sample_train = sample_train.drop("meth_class", axis=1)

    model = Sequential()
    model.add(Dense(600)) #, input_dim = len(x_train.columns)
    model.add(Dense(300))
    model.add(Dense(label_train.shape[1], activation = 'softmax')) #87
    model.compile(loss = 'categorical_crossentropy' , optimizer = Adam(learning_rate=learning_rate) , metrics = ['accuracy'])

    # Training model
    start_time = time.time()
    history = model.fit(sample_train, sample_label_train, **model_params)
    end_time = time.time()

    # Save model
    if save_models:
        pickle.dump(model, open(f"{output_path}/{model_name}/Models/{model_name}_newas{i}_n{n}.pkl", 'wb'))

    # Predicting
    scores = model.evaluate(test, label_test)
    test_labs = labEncode.inverse_transform(np.argmax(label_test, axis=1))
    pred_labs = labEncode.inverse_transform(np.argmax(model.predict(test), axis=1))

    res_pred_train.append(history.history["accuracy"][epochs-1]*100)
    res_pred_test.append(scores[1]*100)
    res_time["n"+str(n)] = {"system": end_time - start_time}
    

  # Formating history
    lr = []
    for lr_i in history.history["lr"]:
       lr.append(float(lr_i))
   
    acc = []
    for acc_i in history.history["accuracy"]:
       acc.append(float(acc_i*100))

  out["newas"+str(i)] = {"acc_train": res_pred_train,
                        "acc_test": res_pred_test,
                        "time": res_time,
                        "results": {"test_labs": test_labs.tolist(),
                                    "pred_labs": pred_labs.tolist()},
                        "history": {"loss": history.history["loss"],
                                    "accuracy": acc,
                                    "lr": lr}}
  #raise("stop!")

  json.dump(out, open(f"{output_path}/{model_name}/{model_name}_res.json", 'w'))