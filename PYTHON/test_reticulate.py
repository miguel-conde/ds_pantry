import numpy as np

np_A = [1,2,3]
np_B = [10, 10, 10]

np_C = np_A + np_B

print(np_C)

np_C = np.add(np_A, np_B)

print(np_C)

import tensorflow as tf

t = tf.constant([[[1], [2], [3], [4], [5], [6], [7], [8]]])

t.shape

print(t)

print(tf.reshape(t, [1, 8]))

def some_method(a, b):
  s = (tf.cast(a, tf.float32) + tf.cast(b, tf.float32))
  
  return(tf.sqrt(tf.matmul(s, tf.transpose(s))))
  
with tf.Session() as sess:
  fake_a = tf.constant([
    [5.0, 3.0, 7.1],
    [2.3, 4.1, 4.8]
    ])
    
  fake_b = tf.constant([
    [2, 4, 5],
    [2, 8, 7]
    ])
  
  print(sess.run(some_method(fake_a, fake_b)))

print(some_method(fake_a, fake_b))


tf.constant(5).shape

tf.constant([5]).shape

tf.constant([2, 3, 5]).shape
