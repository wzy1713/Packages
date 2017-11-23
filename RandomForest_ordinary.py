# -*- coding: utf-8 -*-
"""
Created on Thu Nov 23 10:11:57 2017

@author: wangzhiyu
"""

import tensorflow as tf
import numpy as np
import pandas as pd
import math
import os
from glob import glob
import time
from sklearn.model_selection import train_test_split
import tensorflow.contrib.tensor_forest.python.tensor_forest as tensor_forest
tf.logging.set_verbosity(tf.logging.DEBUG)
path = "F:/zy_Books/tensorflow"

if os.path.exists(path + "/rf") == False:
    os.makedirs(path + "/rf")

os.chdir(path + "/rf")

all_data = pd.read_csv("../final_model_data_comb.csv")
X = all_data.drop(['response'], axis=1).astype(np.float32).values
y = all_data['response'].astype(np.int).values
 
np.random.seed(123)                
x_train, x_test, y_train, y_test = train_test_split(X, y,
                                                    stratify=y, 
                                                    test_size=0.3,
                                                    random_state = 1234)

print("Training features =")
print(x_train[:5])

print("Training labels =")
print(y_train[:5])
print(np.average(y_train))


print("x_test = ")
print(x_test[:5])

print("test labels =")
print(y_test[:5])
print(np.average(y_test))


#ROC Curve and AUC Function
def roc(y_pred, response, method):
    from sklearn.metrics import roc_curve
    from sklearn.metrics import auc
    import matplotlib.pyplot as plt 
    fpr, tpr, thresholds = roc_curve(response.astype(int), y_pred.astype(float))
    #Calculate the AUC
    roc_auc = auc(fpr, tpr)  
    # Plot the ROC curve
    plt.plot(fpr, tpr, label= method + ' ROC curve (area = %0.3f)' % roc_auc)
    plt.plot([0, 1], [0, 1], 'k--')  # random predictions curve
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.0])
    plt.xlabel('False Positive Rate or (1 - Specifity)')
    plt.ylabel('True Positive Rate or (Sensitivity)')
    plt.title('Receiver Operating Characteristic')
    plt.legend(loc="lower right")
    return(roc_auc)

#label_map = {'setosa': 0, 'versicolor': 1, 'virginica': 2}
#y_train = train['Species'].map(label_map).astype(np.int).values
               
s1 = time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time()))
nclass = 2
n_features = x_train.shape[1]
n_trees = 500
m_nodes = 1000
s_after_samples = 200

params = tensor_forest.ForestHParams(num_classes=nclass, 
                                     num_features=n_features, 
                                     num_trees=n_trees, 
                                     max_nodes=m_nodes, 
                                     split_after_samples=s_after_samples).fill()

print("Params =")
print(vars(params))


# Remove previous checkpoints so that we can re-run this step if necessary.
for f in glob("./*"):
    os.remove(f)
classifier = tf.contrib.tensor_forest.client.random_forest.TensorForestEstimator(params, model_dir="./")
classifier.fit(x=x_train, y=y_train)

y_out = list(classifier.predict(x=x_test))

print(y_out[:5])

n = len(y_test)
out_soft = list(y['classes'] for y in y_out)
out_hard = list(y['probabilities'] for y in y_out)

print("Soft predictions:")
print(out_soft[:5])
print("Hard predictions:")
print(out_hard[:5])

soft_zipped = list(zip(y_test, out_soft))
hard_zipped = list(zip(y_test, out_hard))

num_correct = sum(1 for p in hard_zipped if p[0] == np.argmax(p[1]))
print("Accuracy = %s" % (num_correct / n))


test_ps = list(p[1][int(p[0])] for p in hard_zipped)
print("Probs of real label:")
print(test_ps[:5])
total_log_loss = sum(math.log(p) for p in test_ps)
print("Average log loss = %s" % (total_log_loss / n))


confusion = {x: soft_zipped.count(x) for x in set(soft_zipped)}
print ("Confusion matrix:")
print (confusion)

pred_y = np.array([p[1] for p in out_hard])
roc(pred_y, y_test, "TensorFlow")

e1 = time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time()))
print("Starting time:"+s1)
print("Starting time:"+e1)

#RandomForest using sklearn
s2 = time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time()))
from sklearn.ensemble import RandomForestClassifier
clf = RandomForestClassifier(n_estimators=n_trees,
                             max_leaf_nodes = m_nodes,
                             n_jobs = -1,
                             random_state=0)
rf_fit = clf.fit(x_train, y_train)

y_out2 = list(rf_fit.predict_proba(x_test))
print("Predict of sklearn:")
print(y_out2[:5])

pred_y2 = np.array([p[1] for p in y_out2])
roc(pred_y2, y_test,"sklearn")

e2 = time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time()))

print("Starting time:"+s2)
print("Starting time:"+e2)
