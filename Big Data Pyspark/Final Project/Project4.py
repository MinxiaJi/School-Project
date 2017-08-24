
# coding: utf-8

# In[1]:

import pandas as pd
import numpy as np
from sklearn.cross_validation import train_test_split
from sklearn.neighbors import KNeighborsRegressor
from pyspark.mllib.classification import SVMWithSGD, SVMModel
from pyspark.mllib.regression import LabeledPoint
from pyspark.mllib.tree import GradientBoostedTrees, GradientBoostedTreesModel
from pyspark.mllib.util import MLUtils
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import GradientBoostingRegressor


# In[3]:

sc = SparkContext.getOrCreate()


# In[ ]:

###############
#  Question 1 #
###############


# In[6]:

# Question 1 partition data
# load option data name as 'option'
option = pd.read_csv('Option_Data_2000.csv')
option.head()
# train80% test20%
x_train,x_test,y_train,y_test = train_test_split(option.drop('Implied Volatility',axis = 1),
                                                 option['Implied Volatility'],test_size = 0.2,random_state=42)


# In[ ]:

###############
#  Question 2 #
###############


# In[ ]:

###################
#   sklearn KNN   #
###################


# In[9]:

# Models under sklearn
# build k-nn classifier(clf) model
# prevent valueError : unknown label error
clf = KNeighborsRegressor()
clf.fit(x_train,y_train)
# save predictions to KNNpreds
KNNpreds = clf.predict(x_test)
# convert KNNpreds to series data
KNNpreds = pd.DataFrame(KNNpreds)

# add actual IV, implied volatility to GBpreds
# to form a dataframe with column one:IV column two:predict IV
y_test.reset_index(drop=True, inplace=True)
KNNcompare = pd.concat([KNNpreds,y_test],axis = 1)
# To array and convert to RDD
KNNRDD = KNNcompare.as_matrix()
KNNRDD = sc.parallelize(KNNRDD)

# x[0]:actual IV, x[1] predict IV
KNNError = KNNRDD.map(lambda x:abs(x[0]-x[1]))
# get x_test index as RDD
indexRDD = sc.parallelize(x_test.index)
# combine indexRDD with ErrorRDD
indexedKNNError = indexRDD.zip(KNNError)


# In[17]:

# get samples of top 10% error
reversedKNNError = indexedKNNError.takeOrdered(int(0.1*len(x_test)))
reversedKNNError


# In[19]:

# get samples of bottom 10% error
sortedKNNError = indexedKNNError.sortBy(lambda x:x[1])
sortedKNNError.take(int(0.1*len(x_test)))


# In[ ]:

###################
#   sklearn GB    #
###################


# In[20]:

# build GB model
params = {'n_estimators':1000,'max_depth':6,'min_samples_split':2,'learning_rate':0.01,'loss':'ls'}
gb = GradientBoostingRegressor(**params)
gb.fit(x_train,y_train)
# save predictions to GBpreds
GBpreds = gb.predict(x_test)

# convert GBpreds to series data
GBpreds = pd.DataFrame(GBpreds)
# add actual IV, implied volatility to GBpreds
# to form a dataframe with column one:IV column two:predict IV
y_test.reset_index(drop=True, inplace=True)
GBcompare = pd.concat([GBpreds,y_test],axis = 1)
# To array and convert to RDD
GBRDD = GBcompare.as_matrix()
GBRDD = sc.parallelize(GBRDD)

# x[0]:actual IV, x[1] predict IV
GBError = GBRDD.map(lambda x:abs(x[0]-x[1]))
# get x_test index as RDD
indexRDD = sc.parallelize(x_test.index)
# combine indexRDD with ErrorRDD
indexedGBError = indexRDD.zip(GBError)


# In[24]:

# get samples of top 10% error
reversedGBError = indexedGBError.takeOrdered(int(0.1*len(x_test)))
reversedGBError


# In[25]:

# get samples of bottom 10% error
sortedGBError = indexedGBError.sortBy(lambda x:x[1])
sortedGBError.take(int(0.1*len(x_test)))


# In[26]:

# compare MSE of gradientboosting and KNN
GBMSE = GBRDD.map(lambda v: (v[0] - v[1])**2).sum() / float(y_test.count())
KNNMSE = KNNRDD.map(lambda v: (v[0] - v[1])**2).sum() / float(y_test.count())
GBMSE


# In[27]:

KNNMSE


# In[ ]:

###############
#  Question 4 #
###############


# In[117]:

# SVM and GB under Spark
# train80% test20%
trainData, testData = train_test_split(option,test_size=0.2,random_state=42)
train = trainData.as_matrix()
test = testData.as_matrix()
def parsePoint(line):
    return LabeledPoint(line[7],line[0:7])
# create RDD
trainRDD = sc.parallelize(train)
testRDD = sc.parallelize(test)
trainLP = trainRDD.map(parsePoint)
testLP = testRDD.map(parsePoint)


# In[122]:

# build GB model
GBmodel = GradientBoostedTrees.trainRegressor(trainLP,
                                            categoricalFeaturesInfo={5:2}, numIterations=3)
predictions = GBmodel.predict(testLP.map(lambda x: x.features))
sparkGBError = testLP.map(lambda lp: lp.label).zip(predictions)
# compute MSE
testMSE = sparkGBError.map(lambda v: (v[0] - v[1])**2).sum() / float(testLP.count())


# In[124]:

testMSE


# In[111]:

# build SVM model
from pyspark.mllib.classification import SVMWithSGD, SVMModel


# In[115]:

clfSVM = SVMWithSGD.train(trainLP, iteration=100)
svmLabelAndPreds = testLP.map(lambda p: (p.label, clfSVM.predict(p.features)))


# In[116]:

trainLP.take(5)


# In[ ]:



