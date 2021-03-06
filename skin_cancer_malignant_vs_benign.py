# -*- coding: utf-8 -*-
"""skin-cancer-malignant-vs-benign.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1O8CLbzbWlh-5F8HkEhQRN9PIgd3HEGU0

**Import some modules**
"""

# Commented out IPython magic to ensure Python compatibility.
import numpy as np
# %matplotlib inline
import matplotlib.image as mpimg
import matplotlib.pyplot as plt

"""**Setting Directory**"""

train_dir ="C:\\Users\\Nilesh\\Desktop\\dl\\skin-cancer-malignant-vs-benign\\train"
test_dir ="C:\\Users\\Nilesh\\Desktop\\dl\\skin-cancer-malignant-vs-benign\\test"

"""**Images of Benign Type**"""

plt.figure(figsize=(12, 5))
sp = plt.subplot(2, 5, 1)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\19.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 2)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\20.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 3)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\32.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 4)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\33.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 5)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\34.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 6)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\35.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 7)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\524.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 8)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\525.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 9)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\526.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 10)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\benign\\528.jpg")
plt.imshow(img)

"""**Images of Malignant**"""

plt.figure(figsize=(12, 5))
sp = plt.subplot(2, 5, 1)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\2.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 2)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\5.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 3)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\6.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 4)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\7.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 5)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\9.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 6)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\10.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 7)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\11.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 8)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\12.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 9)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\14.jpg")
plt.imshow(img)
sp = plt.subplot(2, 5, 10)
sp.axis('Off')
img = mpimg.imread(train_dir+"\\malignant\\15.jpg")
plt.imshow(img)

import tensorflow as tf

train_dir ="C:\\Users\\Nilesh\\Desktop\\dl\\skin-cancer-malignant-vs-benign\\train"
test_dir ="C:\\Users\\Nilesh\\Desktop\\dl\\skin-cancer-malignant-vs-benign\\test"

from tensorflow.keras.preprocessing.image import ImageDataGenerator

# All images will be rescaled by 1./255.
train_datagen = ImageDataGenerator( rescale = 1.0/255. )
test_datagen  = ImageDataGenerator( rescale = 1.0/255. )

# --------------------
# Flow training images in batches of 20 using train_datagen generator
# --------------------
train_generator = train_datagen.flow_from_directory(train_dir,
                                                    batch_size=20,
                                                    class_mode='binary',
                                                    target_size=(150, 150))     
# --------------------
# Flow validation images in batches of 20 using test_datagen generator
# --------------------
validation_generator =  test_datagen.flow_from_directory(test_dir,
                                                         batch_size=20,
                                                         class_mode  = 'binary',
                                                         target_size = (150, 150))

"""**Formation OF CNN**"""

model = tf.keras.models.Sequential([
    tf.keras.layers.Conv2D(16, (3,3), activation='relu', input_shape=(150, 150, 3)),
    tf.keras.layers.MaxPooling2D(2,2),
    tf.keras.layers.Conv2D(32, (3,3), activation='relu'),
    tf.keras.layers.MaxPooling2D(2,2), 
    tf.keras.layers.Conv2D(64, (3,3), activation='relu'), 
    tf.keras.layers.MaxPooling2D(2,2),
    tf.keras.layers.Flatten(), 
    tf.keras.layers.Dense(512, activation='relu'), 
    tf.keras.layers.Dropout(0.1, seed=2019),
    tf.keras.layers.Dense(200, activation='relu'),
    tf.keras.layers.Dropout(0.2, seed=2020),
    tf.keras.layers.Dense(1, activation='sigmoid')  
])

model.summary()

from tensorflow.keras.optimizers import RMSprop
from tensorflow.keras.callbacks import EarlyStopping
monitor = EarlyStopping(monitor='val_loss', min_delta=1e-3, patience=5, 
                        verbose=1, mode='auto',restore_best_weights=True)
model.compile(optimizer=RMSprop(lr=0.001),
              loss='binary_crossentropy',
              metrics = ['acc'])

"""**Fitting the model**"""

history = model.fit_generator(train_generator,
                              validation_data=validation_generator,
                              steps_per_epoch=100,epochs=15,
                              validation_steps=50,
                              verbose=2, callbacks=[monitor])

testing_datagen  = ImageDataGenerator( rescale = 1.0/255. )
testing_dir = "C:\\Users\\Nilesh\\Desktop\\dl\\skin-cancer-malignant-vs-benign\\Testing"
testing_generator =  testing_datagen.flow_from_directory(testing_dir,
                                                    batch_size=6,
                                                    class_mode  = None,
                                                    target_size = (150, 150),
                                                    shuffle=False)

y_prob = model.predict_generator(testing_generator,callbacks=[monitor])
y_pred = ["benign" if probs > 0.5 else "malignant" for probs in y_prob]

y_prob

y_pred

