#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Wed Jun 28 10:21:47 2017

@author: yuhan
"""

from pyspark import SparkContext

sc =SparkContext()


from pyspark.sql import *

from datetime import datetime,timedelta

from pyspark.sql.window import Window

from pyspark.sql.functions import udf
from pyspark.sql import SparkSession
from pyspark.sql.types import StringType
from pyspark.sql.functions import col,lag,log1p,sqrt


interval=30
#interval time
time_interval=list()
start='9:30:01'
end='16:00:00'

start_t=datetime.strptime(start, '%H:%M:%S')
end_t=datetime.strptime(end, '%H:%M:%S')
end_point=start_t
number_interval=(end_t-start_t).total_seconds()/60/interval

for i in range(int(number_interval)+1):
    time_interval.append(end_point)
    end_point=end_point+timedelta(minutes = interval)
    
#split time into different intervals stored in the time_interval list)
    


spark=SparkSession.builder.master('local').getOrCreate()
df = spark.read.csv('/Users/yuhan/Dropbox/big_data_analytics/Final/HFT_baby_data.csv', header=True)
df2 = df.select(df._c0.alias("KEY"),df.SYMBOL,df.DATE,df.TIME,df.PRICE.cast("float"),df.SIZE.cast("int"),(df.SIZE*df.PRICE).alias('TOTAL_PRICE'))


def getInterval(time):
    convert = datetime.strptime(time, '%H:%M:%S')
    for i in range (len(time_interval)):
        if convert<time_interval[i]:
            break
    return str(i-1)



getIntervalUdf = udf(getInterval,StringType())


df3 = df2.withColumn("Interval",getIntervalUdf("TIME"))




f={'PRICE':'mean','SIZE':'sum', 'TOTAL_PRICE': 'sum', 'KEY':'count','DATE':'count'}
df4 = df3.groupby(["SYMBOL","DATE","Interval"]).agg(f)


df5=df4.withColumn("mean_TOTAL_PRICE",col("sum(TOTAL_PRICE)")/col("sum(SIZE)"))
df5=df5.withColumn("mean_SIZE",col("sum(SIZE)")/col("count(KEY)"))
df5 = df5.withColumn("Interval_int", df5["Interval"].cast("double"))

df6=df5.select('SYMBOL','DATE','Interval_int','avg(PRICE)','mean_TOTAL_PRICE','mean_SIZE')


df6= df6.orderBy(["SYMBOL","DATE","Interval_int"], ascending=[1, 1])
w=Window().partitionBy([col("SYMBOL"),col("DATE")]).orderBy([col("SYMBOL"),col("DATE"),col("Interval_int")])
df7=df6.select("*", lag("avg(PRICE)").over(w).alias("avg(PRICE)_previous"))


    
df8=df7.withColumn("U_sequence",log1p(col("avg(PRICE)")/col("avg(PRICE)_previous")))
df_Usequence=df8.select("U_sequence")
#df_Usequence.write.csv('/Users/yuhan/Dropbox/big_data_analytics/Final/data_save_30',header=True)



df8=df8.withColumn("U_sequence_square",col("U_sequence")*col("U_sequence"))




f_section={"U_sequence_square":'sum', "U_sequence":'sum',"Interval_int":'count',"avg(PRICE)":'mean'}

df_section=df8.groupby(["SYMBOL","DATE"]).agg(f_section)

df_section=df_section.withColumnRenamed('avg(avg(PRICE))', 'daily_average_price')
df_section=df_section.withColumnRenamed('count(Interval_int)', 'n')
df_section=df_section.withColumnRenamed('sum(U_sequence)', 'U_sum')
df_section=df_section.withColumnRenamed('sum(U_sequence_square)', 'U_squre_sum')



df_section=df_section.withColumn("Section_volatility",sqrt(col('U_squre_sum')/col('n')) - col('U_sum')*col('U_sum')/(col('n')*(col('n')-1)))

df_section=df_section.withColumnRenamed('U_squre_sum', 'Realized_volatility')


import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.pylab as pylab



company_list=df_section.select(df_section.SYMBOL).distinct().collect()

company_list_panda=pd.DataFrame(company_list)
company_plot=dict()
date_plot=dict()


headers=['SYMBOL','DATE','U_sum','Realized_volatility','n','daily_average_price','Section_volatility']

for i in range (len(company_list_panda)):
   df_section_sample=df_section.where(df_section.SYMBOL==company_list_panda[0][i]).rdd.takeSample(False, 100, 1)
   company_plot[company_list_panda[0][i]]=pd.DataFrame(df_section_sample,columns=headers).sort_values(['DATE'])
   date_plot[company_list_panda[0][i]]=pd.to_datetime(company_plot[company_list_panda[0][i]]['DATE'], format='%Y%m%d')




fig = pylab.figure(figsize = (10,8))

for k in company_plot:
  pylab.subplot(2,1,1)
  pylab.plot(date_plot[k],company_plot[k]['Section_volatility'],label= k, linewidth=1.5)
  pylab.ylabel('Section_volatility')
  pylab.grid('on')
  pylab.subplot(2,1,2)
  pylab.plot(date_plot[k],company_plot[k]['daily_average_price'],label= k, linewidth=1.5)
  pylab.ylabel('daily_average_price')
  pylab.grid('on')

plt.title("Section_volatility and daily_average_price")
plt.legend()
pylab.show()








fig = pylab.figure(figsize = (10,8))

for k in company_plot:
  pylab.subplot(2,1,1)
  pylab.plot(date_plot[k],company_plot[k]['Realized_volatility'],label= k, linewidth=1.5)
  pylab.ylabel('Realized_volatility')
  pylab.grid('on')
  pylab.subplot(2,1,2)
  pylab.plot(date_plot[k],company_plot[k]['daily_average_price'],label= k, linewidth=1.5)
  pylab.ylabel('daily_average_price')
  pylab.grid('on')

plt.title("Realized_volatility and daily_average_price")
plt.legend()
pylab.show()

