#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb  4 18:38:40 2019

@author: miaocai
"""

########### Problem 1 ###########

import numpy as np
import pandas as pd

Patient = pd.read_csv("healthcare2/Patient.csv")

Patient['Gender'].replace(['male', 'female', 'MISSING'], 
       ['Male', 'Female', 'Other'], inplace = True)
Patient.fillna('Other', inplace=True)

Mortality = pd.read_csv("healthcare2/Mortality.csv")
p1 = pd.merge(Patient, Mortality, on = 'PatientID', how = 'left')

p1.groupby('Gender')['DateOfDeath'].apply(lambda x: x.notnull().sum()/len(x))
# test significance using logistic regression
p1['death'] = np.where(p1['DateOfDeath'].isnull(), 0, 1)
p1logit = p1.join(pd.get_dummies(p1['Gender'], prefix = 'dum'))

import statsmodels.api as sm
logit_model = sm.Logit(p1logit.death, p1logit[['dum_Female', 'dum_Male']])
print(logit_model.fit().summary2())




########### Problem 2 ###########
import numpy as np
import pandas as pd

OutpatientVisit = pd.read_csv("healthcare2/OutpatientVisit.csv")
DiseaseMap = pd.read_csv("healthcare2/DiseaseMap.csv")
Patient = pd.read_csv("healthcare2/Patient.csv")

Patient['Gender'].replace(['male', 'female', 'MISSING'], ['Male', 'Female', 'Other'], inplace = True)
Patient.fillna('Other', inplace=True)
denom = Patient.groupby('Gender').size().reset_index(name = 'denominator')

OutpatientVisitlong = pd.melt(OutpatientVisit, id_vars = 'PatientID', 
        var_name = 'DiagNum', value_name = 'ICD10', 
        value_vars = ['ICD10_1', 'ICD10_2', 'ICD10_3'])
patdiseasemap = pd.merge(OutpatientVisitlong, DiseaseMap, on = 'ICD10', how = 'left')


patcount = patdiseasemap.groupby(['PatientID', 'Condition']).size().reset_index(name = "n")
p2 = pd.merge(patcount, Patient, on = 'PatientID', how = 'left')
q2_1 = p2.groupby(['Gender', 'Condition']).size().reset_index(name = "Ncond")
q2_1 = pd.merge(q2_1, denom, on = 'Gender', how = 'left')
q2_1['mortality'] = q2_1.Ncond/q2_1.denominator


q2_2 = p2.groupby('Condition').size().reset_index(name = 'Ncond')
q2_2['Gender'] = 'Overall'
q2_2['denominator'] = Patient.shape[0]
q2_2['mortality'] = q2_2.Ncond/q2_2.denominator

q2_final = q2_1.append(q2_2, ignore_index = True)
q2_final = q2_final[['Condition', 'Gender', 'mortality']]
q2_final.pivot(index='Condition', columns='Gender', values='mortality')



########### Problem 3 ###########
import numpy as np
import pandas as pd

outpat = pd.read_csv("healthcare2/OutpatientVisit.csv")
outpat['VisitDate'] =  outpat['VisitDate'].astype('datetime64[ns]')

from itertools import product
outpatID = outpat[outpat.PatientID.notnull()].PatientID.unique()
year=list(range(2005, 2019))
patient_years = pd.DataFrame(list(product(outpatID, year)), columns = ['PatientID', 'year'])

pat_min_vis = outpat[outpat.VisitDate.notnull()].groupby(['PatientID']).agg({'VisitDate':'min'}).reset_index()
pat_min_vis['min_vis'] = pat_min_vis['VisitDate'].dt.year
del pat_min_vis['VisitDate']
patient_years = pd.merge(patient_years, pat_min_vis, on = 'PatientID', how = 'left')

Mortality = pd.read_csv("healthcare2/Mortality.csv")
patient_years = pd.merge(patient_years, Mortality, on = 'PatientID', how = 'left')
patient_years['deathyear'] = patient_years['DateOfDeath'].str.slice(0, 4)
patient_years['deathyear'] = patient_years['deathyear'].astype(float)
del patient_years['DateOfDeath']

patient_years['dead'] = np.where(patient_years['year'] >= patient_years['deathyear'], 1, 0)
patient_years['atrisk'] = np.where((patient_years['year'] >= patient_years['min_vis']) & ((patient_years['year'] <= patient_years['deathyear'])|(patient_years['deathyear'].isnull())), "yes", "no")

patient_years.loc[patient_years.atrisk == "yes"].groupby('year')['dead'].agg({'n_at_risk':'count', 'n_dead':'sum', 'mortality_rate':'mean'})







































































v






























































































































































v
























































v












































v


