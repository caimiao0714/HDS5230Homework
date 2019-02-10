#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb  4 18:38:40 2019

@author: miaocai
"""

import pandas as pd
import re
import itertools

t = pd.read_csv("healthcare2/Clinic.csv")

import glob
csvFiles = glob.glob("healthcare2/" + "/*.csv")
csvNames = [re.findall(r"^.*/(.*)\.csv$", csvfile) for csvfile in csvFiles]
csvNames = list(itertools.chain(*csvNames))

datlist = [pd.read_csv(i) for i in csvFiles]
print([x.shape for x in datlist])# look at each element in the list



