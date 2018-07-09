# -*- coding: utf-8 -*-
"""
Created on Mon Jul  9 11:20:47 2018

@author: wangzhiyu
"""

#Read data with the following format in each row:
# 1 2:1 9:1 10:1 20:1 29:1 33:1 35:1 39:1 40:1 52:1 57:1 64:1 68:1 76:1 85:1 87:1 91:1 94:1 101:1 104:1 116:1 123:1
def read_data(path):
    import numpy as np
    import scipy
    y = []
    row = []
    col = []
    values = []
    r = 0       # 首行
    for d in open(path):
        d = d.strip().split()      # 以空格分开
        y.append(int(d[0]))
        d = d[1:]
        for c in d:
            key, value = c.split(':')
            row.append(r)
            col.append(int(key))
            values.append(float(value))
        r += 1
    x = scipy.sparse.csr_matrix((values, (row, col))).toarray()
    y = np.array(y)
    return x, y
