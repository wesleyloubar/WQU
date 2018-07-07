"""
Created on Sat Jun  9 22:30:54 2018

@author: Wesley Lourenco Barbosa
"""
from IPython import get_ipython
get_ipython().magic('reset -sf')
from IPython import get_ipython
def __reset__(): get_ipython().magic('reset -sf')
  

import matplotlib.pyplot as plt
import fix_yahoo_finance as yf  
from scipy import interpolate
import numpy as np
from scipy.optimize import curve_fit
import pylab
from numpy import random , histogram , arange , sqrt , exp , nonzero, array 
from scipy.optimize import leastsq
import algopy

# TASKS 01 AND 02
valid_stock = False
stock_data = []
while(valid_stock==False):
    try:
        print('\n#########  ##########\n',
              'Type the symbol of the stock you want to analyse:')
        ticker = str(input())
        ticker = ticker.replace(' ','').upper()
        start_date = '2018-05-08'
        end_date = '2018-06-08'
        stock_data = yf.download(ticker,start_date,end_date)
        if(len(stock_data)>5):
            valid_stock = True
            print('Stock Succesfully Downloaded')
            break
        else:
            print('Invalid Stock Symbol')
            print('\nChoose a valid option.')
            input("Press Enter") 
    except:
        print('Invalid Stock Symbol')
        print('\nChoose a valid option.')
        input("Press Enter") 
#Ploting Closing Values
stock_data.Close.plot()
plt.title('Stock Price - 1 month')
plt.show()


#TASK 03
#Quadratic Line
x = stock_data.index
x = np.arange(0,len(x))
y = stock_data['Adj Close'].values
f = interpolate.interp1d(x, y,kind='quadratic')
xnew = np.arange(min(x),max(x), 0.08)
ynew = f(xnew)   # use interpolation function returned by `interp1d`
plt.title('Quadratic Line')
plt.plot(x, y,'o', xnew, ynew, '-')
plt.show()


#TASK 04
#Approach 01 using polyfit
new_x = np.linspace(min(x), max(x), num=np.size(x))
coefs = np.polyfit(x,y,2)
new_line = np.polyval(coefs, new_x)
plt.title('Polyfit')
plt.scatter(x,y)
plt.scatter(new_x,new_line,c='g', marker='^', s=5)
plt.xlim(min(x)-0.00001,max(x)+0.00001)
plt.xticks(rotation=90)
plt.tight_layout()
plt.show()

z = np.polyfit(x,y,2)
p = np.poly1d(z)
def func(x, a, b, intercept):   
    return a*(x**2) + b*x + intercept
#My initial values for my quadratic equation
init_vals = [1, 2, 3]
best_vals, covar = curve_fit(func, x, y, p0=init_vals)


xs = [1*i for i in range(len(x))]
ys = [p(x) for x in xs]
plt.title('Polyfit')
pylab.plot(x,y,'d')
pylab.plot(xs,ys,'d')
pylab.show()



#Quadratic Equation values
print(best_vals)
print('Quadratic Equation: ',best_vals[0],
      'x2  ' ,best_vals[1],'x + ',best_vals[2])


#Approach 02 using leastsq
y_data = np.array(y)
t = np.array(x)

def fitfunc(p, t):
    """This is the equation"""
    return p[0]*np.power(t,2) + p[1]*t + p[2]

def errfunc(p, t, y):
    return fitfunc(p,t) -y
    
def jac_errfunc(p, t, y):
    ap = algopy.UTPM.init_jacobian(p)
    return algopy.UTPM.extract_jacobian(errfunc(ap, t, y))
    
#Initial Guess
guess = np.array([1, max(y_data), 0.5])

#Two approaches. The first one using a speed up jacobian method to minimize the number of interactions. 
# The second one is the regular one.
p2, C, info, msg, success = leastsq(errfunc, guess, args=(t, y_data), Dfun = jac_errfunc, full_output=1)
print('Estimates from leastsq \n', p2,success)
print('number of function calls =', info['nfev'])

p3, C, info, msg, success = leastsq(errfunc, guess, args=(t, y_data), full_output=1)
print('Estimates from leastsq \n', p3,success)
print('number of function calls =', info['nfev'])

pylab.plot(t, y_data, 'd')     # Fit
ys = [fitfunc(p2,x) for x in x]
plt.title('Leastsq Fit line')
pylab.plot(x,ys,'d')
pylab.show()


#TASK 05

diff = []
new_y = []
for i in range(0,len(t)):
    aux = fitfunc(p3,t[i])
    new_y.append(aux)

for i in range(0,len(new_y)):
    diff.append(new_y[i] - y_data[i])
  
print(diff)    
yerr = diff                 
plt.title('Leastsq Fit line with error bars')  
pylab.plot(t, y_data, 'o')
pylab.plot(x,ys,'d')
plt.errorbar(t, y_data, yerr=yerr, fmt='k.')  # Data
#pylab.plot(x,y,'d')
#pylab.errorbar(t,y_data,yerr=yerr,fmt='3')
pylab.show()
