#!/usr/bin/env python2.7
# encoding: utf-8

'''
target_algorithm -- script to emulate a target algorithm (only for testing purpose)

@author:     Marius Lindauer
@copyright:  2018 ML4AAD. All rights reserved.
@license:    BSD
@contact:    lindauer@informatik.uni-freiburg.de
'''

import sys, time

# instance format: inst:<hardness>
inst_ = sys.argv[1]
hardness = int(inst_.split(":")[1])

# seed 
seed = int(sys.argv[2])

params = iter(sys.argv[3:])

str_dict = {"str1": 1,
            "str2": 2}
x1, x2, x3 = 0, 0, 0

while True:
    try:
        name = next(params)
    except StopIteration:
        break
    value = next(params)
    
    if name == "int_param":
        x1 = int(value)
        
    elif name == "float_param":
        x2 = float(value)
        
    elif name == "str_param":
        x3 = str_dict.get(value, 0)
        
time.sleep(seed/100)

print("%f" %((x1+x2+x3)*hardness))

