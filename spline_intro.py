# -*- coding: utf-8 -*-
# <nbformat>3.0</nbformat>

# <headingcell level=1>

# Splines

# <markdowncell>

# <codecell>
ipython --pylab
import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
import matplotlib.pyplot as plt

size=[1.42,1.58,1.78,1.99,1.99,1.99,2.13,2.13,2.13, 2.32,2.32,2.32,2.32,2.32,2.43,2.43,2.78,2.98,2.98]
wear=np.array([4.0,4.2,2.5,2.6,2.8,2.4,3.2,2.4,2.6,4.8,2.9, 3.8,3.0,2.7,3.1,3.3,3.0,2.8,1.7])
x=size-np.min(size)
x=x/np.max(x)


%matplotlib inline
plt.scatter(x, wear)



# Now I want to create the class for making a spline


class simple_spline(object):
    
    def __init__(self, data_x, data_y, knots):
        self.x = data_x
        self.k = knots
        self.y = data_y
        
    def basis_poly(self, x, z):
        return ((z-0.5)**2-1/12)*((x-0.5)**2-1/12)/4 - ((abs(x-z)-0.5)**4-(abs(x-z)-0.5)**2/2+7/240)/24
    
    def spline_model(self):
        q=len(self.k)+2
        n=len(self.x)
        X=np.ones((n,q))
        X[:, 1] = self.x
        
        temp_X = []
        for i in self.x:
            for j in self.k:
                temp_X.append(self.basis_poly(i, j))
        temp_X=np.matrix(temp_X).reshape((n, q-2))
        X[:,2:q] = temp_X
        return(X)
        
        

k=[i/5.0 for i in range(1,5)]
spline1=simple_spline(x,wear,k)

X_spline = spline1.spline_model()
results = sm.OLS(wear, X_spline).fit()
results.summary()

xp = [i/100.0 for i in range(1,100)]
Xp=simple_spline(xp, wear, k).spline_model()
plot()

y_hat = np.dot(Xp,results.params)



# for penalized least squares

def basis_poly(x, z):
    return ((z-0.5)**2-1/12)*((x-0.5)**2-1/12)/4 - ((abs(x-z)-0.5)**4-(abs(x-z)-0.5)**2/2+7/240)/24

def spl_S():
    """
    Set up the penalized regression spline penalty matrix given knot sequence k
    """

    q=len(k)+2
    S=np.zeros((q,q))
    for i in k:
        for j in k:
            S[i,j]=basis_poly(k,k)











