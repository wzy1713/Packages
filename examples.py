# -*- coding: utf-8 -*-
"""
Created on Mon Mar 19 15:13:18 2018

@author: wangzhiyu
"""
def ti(j):
    import time
    print(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime()))
    product  = 1
    for i in range(1, int(j)):
        product *=i
    print(product)
    print(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime()))
    
    print(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime()))
    from functools import reduce
    print(reduce(lambda x,y: x*y, range(1,int(j))))
    print(time.strftime("%Y-%m-%d %H:%M:%S", time.localtime()))

import sys
n = sys.argv[1] 
print(n)
ti(n)