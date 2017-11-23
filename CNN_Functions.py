# -*- coding: utf-8 -*-
"""
Created on Tue Nov 21 09:14:02 2017

@author: wangzhiyu
"""
import tensorflow as tf
#case 2
input = tf.Variable(tf.random_normal([1,3,3,1]))
filter = tf.Variable(tf.random_normal([1,2,1,7]))

op2 = tf.nn.conv2d(input, filter, strides=[1, 1, 1, 1], padding='VALID')
init = tf.global_variables_initializer()
with tf.Session() as sess:
    sess.run(init)
    print(sess.run(input))
    print(sess.run(filter))
    print("case 2")
    print(sess.run(op2))
    
#case 3
input = tf.Variable(tf.random_normal([1,3,3,5]))
filter = tf.Variable(tf.random_normal([3,3,5,1]))

op3 = tf.nn.conv2d(input, filter, strides=[1, 1, 1, 1], padding='VALID')
init = tf.global_variables_initializer()
with tf.Session() as sess:
    sess.run(init)
    print(sess.run(input))
    print(sess.run(filter))
    print("case 3")
    print(sess.run(op3))


#case 4
input = tf.Variable(tf.random_normal([1,5,5,5]))
filter = tf.Variable(tf.random_normal([3,3,5,1]))

op4 = tf.nn.conv2d(input, filter, strides=[1, 1, 1, 1], padding='VALID')

init = tf.global_variables_initializer()
with tf.Session() as sess:
    sess.run(init)
    print(sess.run(input))
    print(sess.run(filter))
    print("case 4")
    print(sess.run(op4))

#case 5
input = tf.Variable(tf.random_normal([1,5,5,5]))
filter = tf.Variable(tf.random_normal([3,3,5,1]))

op5 = tf.nn.conv2d(input, filter, strides=[1, 1, 1, 1], padding='SAME')

init = tf.global_variables_initializer()
with tf.Session() as sess:
    sess.run(init)
    print(sess.run(input))
    print(sess.run(filter))
    print("case 5")
    print(sess.run(op5))



#case 6
input = tf.Variable(tf.random_normal([1,5,5,5]))
filter = tf.Variable(tf.random_normal([3,3,5,7]))

op6 = tf.nn.conv2d(input, filter, strides=[1, 1, 1, 1], padding='SAME')

init = tf.global_variables_initializer()
with tf.Session() as sess:
    sess.run(init)
    print(sess.run(input))
    print(sess.run(filter))
    print("case 6")
    print(sess.run(op6))
    
#case 7
input = tf.Variable(tf.random_normal([1,5,5,5]))
filter = tf.Variable(tf.random_normal([3,3,5,7]))

op7 = tf.nn.conv2d(input, filter, strides=[1, 2, 2, 1], padding='SAME')

init = tf.global_variables_initializer()
with tf.Session() as sess:
    sess.run(init)
    print(sess.run(input))
    print(sess.run(filter))
    print("case 7")
    print(sess.run(op7))
    
#case 8
input = tf.Variable(tf.random_normal([10,5,5,5]))
filter = tf.Variable(tf.random_normal([3,3,5,7]))

op8 = tf.nn.conv2d(input, filter, strides=[1, 2, 2, 1], padding='SAME')

init = tf.global_variables_initializer()
with tf.Session() as sess:
    sess.run(init)
    print(sess.run(input))
    print(sess.run(filter))
    print("case 8")
    print(sess.run(op8))

    

#池化
import tensorflow as tf

a=tf.constant([
        [[1.0,2.0,3.0,4.0],
        [5.0,6.0,7.0,8.0],
        [8.0,7.0,6.0,5.0],
        [4.0,3.0,2.0,1.0]],
        [[4.0,3.0,2.0,1.0],
         [8.0,7.0,6.0,5.0],
         [1.0,2.0,3.0,4.0],
         [5.0,6.0,7.0,8.0]]
    ])

a=tf.reshape(a,[1,4,4,2])

pooling1=tf.nn.max_pool(a,[1,2,2,1],[1,1,1,1],padding='VALID')
pooling2=tf.nn.max_pool(a,[1,2,2,1],[1,1,1,1],padding='SAME')
with tf.Session() as sess:
    print("image:")
    image=sess.run(a)
    print (image)
    print("reslut1:")
    result1=sess.run(pooling1)
    print (result1)
    print("reslut2:")
    result2 = sess.run(pooling2)
    print(result2)











