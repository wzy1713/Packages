# -*- coding: utf-8 -*-
"""
Created on Tue May 15 17:55:40 2018

@author: wangzhiyu
"""

# -*- coding: utf-8 -*-
"""
Created on Mon May 14 11:31:11 2018

@author: wangzhiyu
"""

import os
os.chdir(r"D:\ProgramData\Anaconda3\Scripts")

import input_data
import pandas as pd
import numpy as np
import tensorflow as tf

os.chdir(r"H:\客户画像\CVM\苏宁\一期\02_Data")
mnist = pd.read_csv(r"final_model_data_comb.csv", sep = ',')
mnist.x = mnist.iloc[:,2:].values
mnist.y = mnist.iloc[:,:2].values

def roc(y_pred, response, label):
    from sklearn.metrics import roc_curve
    from sklearn.metrics import auc
    import matplotlib.pyplot as plt 
    fpr, tpr, thresholds = roc_curve(response.astype(int), y_pred.astype(float))
    #Calculate the AUC
    roc_auc = auc(fpr, tpr)  
    # Plot the ROC curve
    lb = ': AUC = %0.3f' % roc_auc 
    plt.plot(fpr, tpr, label = label.capitalize() + lb)
    plt.plot([0, 1], [0, 1], 'k--')  # random predictions curve
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.0])
    plt.xlabel('False Positive Rate or (1 - Specifity)')
    plt.ylabel('True Positive Rate or (Sensitivity)')
    plt.title('Receiver Operating Characteristic')
    plt.legend(loc="lower right")
    return(roc_auc)


# Parameters
learning_rate = 0.001
training_iters = 200000
batch_size = 5000
display_step = 20

# Network Parameters
n_input = 42 # MNIST data input (img shape: 28*28)
n_classes = 2 # MNIST total classes (0-9 digits)
dropout = 1 # Dropout, probability to keep units

# tf Graph input
x = tf.placeholder(tf.float32, [None, n_input])
y = tf.placeholder(tf.float32, [None, n_classes])
keep_prob = tf.placeholder(tf.float32) # dropout (keep probability)

def dnn(_X, _weights, _biases, _dropout):
    # Reshape input picture
    _X = tf.nn.dropout(_X, _dropout) #//这里可以让dropout都不同 我就一样了
    d1 = tf.nn.sigmoid(tf.nn.bias_add(tf.matmul(_X,_weights['wd1']),_biases['bd1']), name="d1")
  
    d2x = tf.nn.dropout(d1, _dropout)
    d2 =  tf.nn.sigmoid(tf.nn.bias_add(tf.matmul(d2x,_weights['wd2']),_biases['bd2']), name="d2")
    
    dout =tf.nn.dropout(d2,_dropout)
    # Output, class prediction
    out = tf.matmul(dout, _weights['out']) + _biases['out']
    return out

# Store layers weight & bias
weights = {'wd1': tf.Variable(tf.random_normal(shape = [42,100], 
                                               stddev=0.01, 
                                               seed = 1234)),
           'wd2': tf.Variable(tf.random_normal(shape = [100,50], 
                                               stddev=0.01, 
                                               seed = 1234)),
           'out': tf.Variable(tf.random_normal(shape = [50, 2],
                                               seed = 1234))
          }
biases = {'bd1': tf.Variable(tf.random_normal(shape = [100],
                                              seed = 1234)),
          'bd2': tf.Variable(tf.random_normal(shape = [50],
                                              seed = 1234)),
          'out': tf.Variable(tf.random_normal(shape = [2],
                                              seed = 1234))
         }

# Construct model
pred = dnn(x, weights, biases, keep_prob)
predict_op = tf.nn.softmax(pred)

# Define loss and optimizer
cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits = pred, 
                                                              labels = y))
optimizer = tf.train.AdamOptimizer(learning_rate=learning_rate).minimize(cost)
#optimizer = tf.train.GradientDescentOptimizer(learning_rate=learning_rate).minimize(cost)


# Evaluate model
correct_pred = tf.equal(tf.argmax(pred,1), tf.argmax(y,1))
accuracy = tf.reduce_mean(tf.cast(correct_pred, tf.float32))


# Initializing the variables
init = tf.global_variables_initializer()

# Launch the graph
with tf.Session() as sess:
    sess.run(init)
    sess.run(optimizer, feed_dict={x: mnist.x, 
                                   y: mnist.y, 
                                   keep_prob: dropout})
        
    # Calculate batch accuracy
    acc = sess.run(accuracy, feed_dict={x: mnist.x, 
                                        y: mnist.y, 
                                        keep_prob: 1.})
    # Calculate batch loss
    loss = sess.run(cost, feed_dict={x: mnist.x, 
                                     y: mnist.y,
                                     keep_prob: 1.})
    print("Iter, Minibatch Loss= " + 
          "{:.6f}".format(loss) + ", Training Accuracy= " + "{:.5f}".format(acc))
           
    # pred
    pred_ = sess.run(predict_op, feed_dict = {x: mnist.x, keep_prob: 1.})
    #print(pred_)
    roc(pred_[:,0], mnist.y[:,0], label ='test')
            
    print("Optimization Finished!")
    # Calculate accuracy for 256 mnist test images
    print("Testing Accuracy:", sess.run(accuracy, 
                                        feed_dict={x: mnist.x, y: mnist.y, keep_prob: 1.}))

    