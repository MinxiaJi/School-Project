
# coding: utf-8

# In[1]:

#########################################################
####### simple demo for kmeans and bisecting kmeans model 
####### Data:pm2.5 in Beijing from 2010 to 2014
####### Minxia Ji

import pandas as pd
import numpy as np
sc = SparkContext.getOrCreate()


# In[3]:

#load and clean data of pm2.5 in Beijing
data = pd.read_csv("PM2.5_Beijing_2010_2014.csv",header = 'infer')
data.head()
clean_data = data.dropna(how = 'any')
clean_data.to_csv('clean_pm2.5_data.csv')
clean_data.head()
clean_data.head()


# In[ ]:

import time
start_time = time.time()
summarys(2010)
print("--- %s seconds ---" % (time.time() - start_time))


# In[70]:

summarys(2010)
summarys(2011)
summarys(2012)
summarys(2013)
summarys(2014)


# In[61]:

# data visualization
def visual(year,mu,sigma):
    x = predata.loc[year,'pm2.5']
    ax = plt.subplot(111)
    ax.spines["top"].set_visible(False)  
    ax.spines["right"].set_visible(False) 
    ax.get_xaxis().tick_bottom()  
    ax.get_yaxis().tick_left()  

    num_bins = 30
# the histogram of the data
    n, bins, patches = plt.hist(x, num_bins, normed=1, facecolor='white')
# add a 'best fit' line
    y = mlab.normpdf(bins, mu, sigma)
    plt.plot(bins, y, 'r--')
    plt.xlabel('pm2.5')
    plt.ylabel('frequency')
    plt.title(year)
    plt.savefig("year", bbox_inches="tight")
# Tweak spacing to prevent clipping of ylabel
    return plt.show()

visual(2014,97,93)


# In[36]:

# cluster pm2.5
from pyspark.mllib.clustering import BisectingKMeans, BisectingKMeansModel
from pyspark.mllib.clustering import KMeans


# In[58]:

# data preparation for kmeans, drop unnecessary features
clfdata = clean_data.drop(['Is','Ir','cbwd','No','month','hour','day'],axis = 1)
clfdata = clfdata.set_index('year')
clfdata.loc[2010].tail()


# In[80]:

# run two kmeans to get 3 clusters for pm2.5 data
def doublekmeans(data,year):
    data = data.loc[year,'pm2.5']
    #kmeans
    data = sc.parallelize(data)
    cluster_no = 2
    maxIter = 30
    clusters = KMeans.train(data,cluster_no,maxIter)
    #find 1.0 labels
    tdata = data.collect()
    cluster_info = np.zeros(len(tdata))
    label = []
    for i in range(0,len(tdata)):
        cluster_info[i]=clusters.predict(np.array(tdata[i]))
        if cluster_info[i]==1.0:
            label.append(i)
    #selecting 1.0 data and preparing the data      
    data1 = clean_data.drop(['Is','Ir','cbwd','No','month','year','hour','day'],axis = 1)
    data2 = data1.iloc[label]
    data2 = sc.parallelize(data2.as_matrix())
    
    #bisecting kmeans
    data2.collect()
    cluster_no = 2
    maxiter = 30
    model = BisectingKMeans.train(data2,cluster_no,maxiter)
    
    return clusters.centers,model.centers, model.computeCost(data2)


# In[75]:

# modified the doublekmeans function a little bit to check if the standardized data will come with smaller SSE
from sklearn.preprocessing import StandardScaler
standard_scaler = StandardScaler()
stddata = standard_scaler.fit_transform(clfdata)
#compare two use the whole data
doublekmeans(stddata)
doublekmeans(clfdata)


# In[ ]:

#######################
#   credit card       #              
#######################


# In[3]:

credit = pd.read_csv('credit_data_.csv',header = None)
credit.head()
# add labels column
credit['label'] =1 
credit['label'][1540:] = 0


# In[4]:

#split training and testing
#training
good = credit[0:1000]
bad = credit[1540:1620]
x_train = pd.merge(good,bad,how='outer')
x_train.drop('label',axis = 1)
y_train = x_train['label']
y_train.shape
#testing
good_test = credit[1000:1540]
bad_test = credit[1620:1671]
x_test = pd.merge(good_test,bad_test,how='outer')
x_test.drop('label',axis = 1)
y_test = x_test['label']


# In[5]:

from sklearn import svm


# In[6]:

clfsvm = svm.SVC(kernel = 'rbf',  gamma = 0.5 , C=1)
clfsvm.fit(x_train,y_train)
prediction = clfsvm.predict(x_test)


# In[7]:

clfsvm = svm.SVC(kernel = 'linear',  gamma = 0.5 , C=1)
clfsvm.fit(x_train,y_train)
prediction = clfsvm.predict(x_test)

clfsvm = svm.SVC(kernel = 'poly',  gamma = 0.5 , C=1)
clfsvm.fit(x_train,y_train)
prediction = clfsvm.predict(x_test)

clfsvm = svm.SVC(kernel = 'sigmoid',  gamma = 0.5 , C=1)
clfsvm.fit(x_train,y_train)
prediction = clfsvm.predict(x_test)

