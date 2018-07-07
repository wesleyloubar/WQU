#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May 24 21:04:54 2018

@author: Wesley Lourenco Barbosa
"""

opt = 0

while(opt!=3):
    while(opt!=1 and opt!=2 and opt!=3):
        try:
            print('\n######### Dividend Discount Model ##########\n',
                  'Do you want to type in the values or use the final project ones?',
                    '\n1 - Type in',
                    '\n2 - Use the Final Project Values',
                    '\n3 - Exit')
            opt = int(input())
            if(opt!=1 and opt!=2 and opt!=3):
                print('\n\nChoose a valid option')
                input("Press Enter")
        except ValueError:
            print('\n\nChoose a valid option')
            input("Press Enter")
    
    do = 100
    g = 2/100
    r = 4/100
    if(opt==2):
        d1 = do*(1+g)
        value_of_stock = d1/(r-g)
        print('\nStock Value: $',value_of_stock)
        input("Press Enter")
        opt = 0
    #D1 = D0 x (1 + g)  
    #Price per Share = D1 / (r - g)  
    elif(opt==1):
        print('Enter the Dividend Values:')
        do = float(input('$'))
        print('Enter the expected constant growth rate: (%)')
        g = float(input(''))/100
        print('Enter the constant cost of equity capitals: (%)')
        r = float(input(''))/100
        d1 = do*(1+g)
        value_of_stock = d1/(r-g)
        print('\nStock Value: $',value_of_stock)
        opt = 0
        input("Press Enter")
    else:
        print('Program Ended!')
        pass;
