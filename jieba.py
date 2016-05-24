# -*- coding: utf-8 -*-
"""
Created on Mon May 23 16:05:56 2016

@author: Administrator
"""
import nltk
from nltk.book import *
import jieba as jb
import pandas as pd
import os
from pandas import  DataFrame,Series
from functools import reduce
#%%
os.chdir('E:\Ruitian')
crh_f = pd.read_excel('2016年动车组故障汇总.xls',usecols = [1,7,9,10],header = 1,encoding = 'utf-8')
#%%
test = crh_f.stack()
test = test.apply(jb.cut)
test = test.apply(list)

#%%
fdist = test.map(FreqDist)
freq = fdist.sum()