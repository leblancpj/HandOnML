#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 10 04:39:52 2018

@author: pleblanc
"""

import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn import linear_model

def prepare_country_stats():
    lifs = pd.DataFrame(oecd_bli.loc[(oecd_bli['INDICATOR'] == "SW_LIFS") &
                                     (oecd_bli['INEQUALITY'] == "TOT"), 
                                     ["Country","Value"]])
    gdp = pd.DataFrame(gdp_per_capita.loc[:,["Country", "2015"]])
    result = gdp.merge(lifs)
    result.columns = ["Country","GDP Per Capita","Life Satisfaction"]
    return result

# Load the data
oecd_bli = pd.read_csv('oecd_bli_2015.csv', thousands = ",")
gdp_per_capita = pd.read_csv('gdp_per_capita2.csv', thousands = ',', delimiter='\t',
                             encoding = 'latin1', na_values = "n/a")

# Prepare the data
country_stats = prepare_country_stats()
x = np.c_[country_stats["GDP Per Capita"]]
y = np.c_[country_stats["Life Satisfaction"]]

# Visualize the data
country_stats.plot(kind='scatter', x='GDP Per Capita', y = 'Life Satisfaction',
                   ylim=[0,10])
plt.show()

# Select a linear model
model = linear_model.LinearRegression()

# Train the model
model.fit(x,y)

# Make a prediction for Cyprus
X_new = [[22587]]
print(model.predict(X_new))