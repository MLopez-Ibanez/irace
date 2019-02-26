#!/usr/bin/env python2.7
# encoding: utf-8

'''
sgd_ta -- script to fit and evaluate SGD wrt a given configuration

@author:     Marius Lindauer
@copyright:  2016 ML4AAD. All rights reserved.
@license:    BSD
@contact:    lindauer@informatik.uni-freiburg.de
'''

import sys

from sklearn.linear_model import SGDClassifier
from sklearn.datasets import load_iris, load_digits
from sklearn.model_selection import train_test_split, KFold

data = load_iris()
#data = load_digits()

if sys.argv[1] == "train":
    X_train_f, X_test, y_train_f, y_test = train_test_split(data.data, data.target, test_size=0.25, random_state=0)
    X_train, X_valid, y_train, y_valid = train_test_split(X_train_f, y_train_f, test_size=0.25, random_state=0)
elif sys.argv[1] == "test":
    X_train_f, X_test, y_train_f, y_test = train_test_split(data.data, data.target, test_size=0.25, random_state=0)
elif sys.argv[1][:2] == "cv":
    X_train_f, X_test, y_train_f, y_test = train_test_split(data.data, data.target, test_size=0.25, random_state=0)
    kf = KFold(n_splits=10, shuffle=True, random_state=1)
    kfs = [kf for kf in kf.split(X_train_f)]
    cv = int(sys.argv[1][2:]) - 1
    X_train = X_train_f[kfs[cv][0]]
    y_train = y_train_f[kfs[cv][0]]
    X_valid = X_train_f[kfs[cv][1]]
    y_valid = y_train_f[kfs[cv][1]]

params = iter(sys.argv[2:])

while True:
    try:
        name = next(params)
    except StopIteration:
        break
    value = next(params)
    
    if name == "random_state":
        random_state = int(value)+1
        
    elif name == "loss":
        loss = str(value)
        
    elif name == "penalty":
        penalty = str(value)
        
    elif name == "alpha":
        alpha = float(value)
        
    elif name == "learning_rate":
        learning_rate = str(value)
        
    elif name == "eta0":
        eta0 = float(value)
        
    elif name == "n_iter":
        n_iter = int(value)
    
sgd = SGDClassifier(random_state=random_state,
                    loss=loss,
                    penalty=penalty,
                    alpha=alpha,
                    learning_rate=learning_rate,
                    eta0=eta0,
                    max_iter=n_iter)    
    
#print(sgd.loss, sgd.penalty, sgd.alpha, sgd.learning_rate, sgd.eta0)
        
if sys.argv[1] == "train" or sys.argv[1][:2] == "cv":
    sgd.fit(X_train,y_train)
    print(-1 * sgd.score(X_valid, y_valid))
else:
    sgd.fit(X_train_f,y_train_f)
    print(-1 * sgd.score(X_test, y_test))


