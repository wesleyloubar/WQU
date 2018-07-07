#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May 27 14:30:30 2018

@author: w0l
"""
import math
from scipy.stats import norm 

def black_scholes_model(current_underlying_price,strike_price,t,implied_volatility,r):
    lnSK = math.log(current_underlying_price/strike_price)
    #print('lnSK: ',lnSK)
    rs2 = (r+(math.pow(implied_volatility,2)/2))
    #print('rs2: ',rs2)
    d1 = ((lnSK+rs2)*t)/(implied_volatility*math.sqrt(t))
    print('D1: ',d1)
    
    d2 = d1-implied_volatility*math.sqrt(t)
    print('D2: ',d2)
    c = current_underlying_price*norm.cdf(d1)-norm.cdf(d2)*strike_price*math.exp(-r*t)
    print('Call Option Price : $',c)
    p = (norm.cdf(-d2)*strike_price*math.exp(-r*t))-(norm.cdf(-d1)*current_underlying_price)
    print('Put Option Price : $',p)
    
    opt = 0
    return opt


opt = 0
while(opt!=3):
    while(opt!=1 and opt!=2 and opt!=3):
        try:
            print('\n############ Black-Scholes Model ############\n',
                  'Do you want to type in the values or use the final project ones?',
                    '\n1 - Type in',
                    '\n2 - Use the Final Project Values',
                    '\n3 - Exit')
            opt = int(input())
            if(opt!=1 and opt!=2 and opt!=3):
                print('\n\nChoose a valid option')
        except ValueError:
            print('\n\nChoose a valid option')
    
    current_underlying_price = 49
    strike_price = 49
    t = 1
    implied_volatility = 25/100
    r = 0.1/100

    if(opt==2):
        opt = black_scholes_model(current_underlying_price,strike_price,t,implied_volatility,r)
        input("Press Enter\n")
        
        current_underlying_price = 45
        strike_price = 50
        print('For Strike Price: $50 and Underlying Price: $45')
        opt = black_scholes_model(current_underlying_price,strike_price,t,implied_volatility,r)
        input("Press Enter")
        
    elif(opt==1):
        print('Enter the current uderlying stock price:')
        current_underlying_price = float(input('$'))
        print('Enter the strike price: ')
        strike_price = float(input('$'))
        print('Enter the time to expiry: (years)')
        t = float(input(''))
        print('Enter the volatility rate: (%)')
        implied_volatility = float(input(''))/100
        print('Enter the risk-free rate: (%)')
        r = float(input(''))/100
        opt = black_scholes_model(current_underlying_price,strike_price,t,implied_volatility,r)
        input("Press Enter")
    else:
        print('Program Ended!')
        pass;