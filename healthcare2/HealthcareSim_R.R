##################################################################################
## Simulate HealthCare Dataset
## Author:Evan Carey
##################################################################################
# The dataset represents the results of typical retail banking transactions.
##################################################################################
library(zoo)
library(data.table)
library(survsim)
set.seed(50)
#### Generate Person List ####
fake_names <- 
  fread('C:/Users/evancarey/Dropbox/Work/BHAnalytics/Data_Sims/common/FakeNameList.csv')
fake_names[,EmailAddress2:=paste0(GivenName,'.',Surname,'@company.com')]
## handle duplicates
fake_names[,RepeatUserID:=(1:.N),
           by=EmailAddress2]
fake_names[RepeatUserID==1,EmailAddress3:=paste0(GivenName,'.',Surname,'@company.com')]
fake_names[RepeatUserID>1,EmailAddress3:=paste0(GivenName,'.',Surname,RepeatUserID,'@company.com')]
## check for unique people
fake_names[,uniqueN(EmailAddress3)]
## Fix Date of Birth Distribution
## Up-weight later dates of birth for employement data!
fake_names[,DateOfBirth:=as.Date(Birthday,format='%m/%d/%Y')]
fake_names[,hist(DateOfBirth,200)]
fake_names[,summary(as.numeric(DateOfBirth))]
## Create new distribution of ages
dt1 <- data.table(rgam=rgamma(1000000,7.5,1))
dt1[,hist(rgam,200)]
dt1[rgam >= 5 & rgam <= 14 ,hist(rgam,200)]
dt1[rgam >= 5 & rgam <= 14 ,hist(round(50*((rgam-5)/9)+18,1),200)]
dt1[rgam >= 5 & rgam <= 14 ,age2:=round(50*((rgam-5)/9)+18,1)]
dt1[,DateOfBirth := as.Date('2018-01-01') - age2*365]
dt1[!is.na(DateOfBirth)][sample(.N,10,replace=T)]
fake_names[,DateOfBirth2:=sample(dt1[!is.na(DateOfBirth),DateOfBirth],
                                 .N,replace = T)]
fake_names[,hist(DateOfBirth2,200)]
## simulate distribution for hire date:
dt2 <- data.table(rgam=rgamma(1000000,1.8,.3))
dt2[,hist(rgam,200)]
dt2[rgam<25,HireDate:=as.Date('2015-01-01')-(rgam*365.25)]
fake_names[,HireDate:=sample(dt2[!is.na(HireDate),HireDate],
                             .N,replace = T)]
## ensure hire date is at least age 16
fake_names[HireDate<(DateOfBirth2+16*325.25),
           HireDate:=DateOfBirth2+16*325.25]

#### Create Patient Table ####
N_patients <- 20000
Patient <- 
  fake_names[RepeatUserID==1][sample(.N,N_patients),
             list(PatientID=1:N_patients,
                  FirstName=GivenName,
                  MiddleInitial,
                  Gender,
                  LastName=Surname,
                  DateOfBirth,
                  StreetAddress,
                  City,
                  State,
                  Zipcode=ZipCode,
                  CountryCode='USA')]

# Set some middle names to missing
Patient[sample(.N,.N*0.3),MiddleInitial:=NA]

# Make some date of births dirty
Patient[,DateOfBirthDirty:=DateOfBirth]
Patient[sample(.N,.N*0.05),DateOfBirthDirty:=NA]
Patient[sample(.N,.N*0.06),
        DateOfBirthDirty:=DateOfBirthDirty - 365*100]
# Add race
Patient[,Race:=sample(c('white','black','hispanic','other'),
                      replace = T,size = .N,prob = c(6,1.5,2,1))]
# Add some missing patterns to race
Patient[,RaceDirty:=Race]
Patient[sample(.N,.N*0.05),
        RaceDirty:=NA]
Patient[sample(.N,.N*0.05),
        RaceDirty:='Unknown']
Patient[sample(.N,.N*0.1),
        RaceDirty:='Missing']

## add some missing patterns to Gender
Patient[,GenderDirty:=Gender]
Patient[sample(.N,.N*0.03),
        GenderDirty:=NA]
Patient[sample(.N,.N*0.05),
        GenderDirty:='MISSING']

####  Clinic table  ####
n_clinics = 60
Clinic <- 
  data.table(ClinicCode=1:n_clinics)
Clinic[,ClinicDescription:= sample(c('Primary Care','Specialty Care','Emergency Department'),size = .N,replace = T,
                                   prob = c(10,3,2))]

#### Create Staff Table ####
## sample names
N_employees <- 50
Staff <- 
  fake_names[RepeatUserID==1][sample(.N,N_employees),
                              list(StaffID=1:N_employees,
                                   FirstName=GivenName,
                                   LastName=Surname,
                                   Birthdate=DateOfBirth,
                                   Hiredate=HireDate,
                                   Gender)]
## Title
Staff[,Title:=sample(c('Physician','Nurse','Physician Assistant'),
                        replace = T,size = .N,
                        prob = c(4,8,2))]
Staff[1:2,Title:='Clinic Manager']
Staff[,.N,by=Title]
## Reports To
Staff[Title == 'Nurse',
         ReportsTo:=sample(Staff[Title == 'Physician',StaffID],
                           size=.N,replace = T)]
Staff[Title == 'Physician Assistant',
         ReportsTo:=sample(Staff[Title == 'Physician',StaffID],
                           size=.N,replace = T)]
Staff[Title == 'Physician',
         ReportsTo:=sample(Staff[Title == 'Clinic Manager',StaffID],
                           size=.N,replace = T)]

## Pay
Staff[,PayType:=sample(c('Salary','Hourly'),
                          replace = T,size = .N,
                          prob = c(5,4))]
Staff[Title=='Clinic Manager',PayType:='Salary']
Staff[Title=='Physician',PayType:='Salary']

## Salaries
Staff[PayType=='Salary' & Title=='Physician',
         Salary:=round(rnorm(.N,150000,sd = 35000))]
Staff[PayType=='Salary' & Title=='Clinic Manager',
         Salary:=round(rnorm(.N,110000,sd = 9000))]
Staff[PayType=='Salary' & Title=='Physician Assistant',
         Salary:=round(rnorm(.N,70000,sd = 5000))]
Staff[PayType=='Salary' & Title=='Nurse',
         Salary:=round(rnorm(.N,60000,sd = 15000))]
## Hourly
Staff[PayType=='Hourly' & Title=='Nurse',
         HourlyRate:=round(rnorm(.N,15,sd = 1))]
Staff[PayType=='Hourly' & Title=='Physician Assistant',
         HourlyRate:=round(rnorm(.N,20,sd = 2))]
## Add Clinic
Staff[,ClinicCode:=sample(Clinic$ClinicCode,replace=T)]

Staff$Salary[!is.na(Staff$Salary)][3:4] <- c(1,999999)

####  Diagnosis table  ####
library(icd)
length(icd10_map_quan_deyo)
DiseaseMap <-
  rbindlist(lapply(seq_along(icd10_map_quan_deyo),
       function(x) data.table(Condition=names(icd10_map_quan_deyo[x]),
                              ICD10=icd10_map_quan_deyo[[x]])))
DiseaseMap[Condition == 'MI', Condition := 'Myocardial_infarction']
DiseaseMap[Condition == 'CHF', Condition := 'Congestive_heart_failure']
DiseaseMap[Condition == 'PVD', Condition := 'Peripheral_vascular_disease']
DiseaseMap[Condition == 'DM', Condition := 'Diabetes_without_complications']
DiseaseMap[Condition == 'DMcx', Condition := 'Diabetes_with_complications']
DiseaseMap[Condition == 'Mets', Condition := 'Metastatic_solid_tumour']
DiseaseMap[Condition == 'PUD', Condition := 'Peptic_ulcer_disease']
## Add in a few mental health comorbidities
DiseaseMap2 <-
  rbindlist(lapply(seq_along(icd10_map_ahrq[c('Obesity','Depression','HTN','Drugs','Alcohol')]),
                   function(x) data.table(Condition=names(icd10_map_ahrq[c('Obesity','Depression','HTN','Drugs','Alcohol')][x]),
                                          ICD10=icd10_map_ahrq[c('Obesity','Depression','HTN','Drugs','Alcohol')][[x]])))
DiseaseMap <- rbind(DiseaseMap,DiseaseMap2)
DiseaseMap[Condition == 'HTN', Condition := 'Hypertension']
## Add rowID
DiseaseMap[,DiseaseMapID:=1:.N]
DiseaseMap[,ICD10:=as.character(ICD10)]

## Establish disease probabilities
DiseaseProbs <-
  data.table(expand.grid(Condition=unique(DiseaseMap$Condition),
                         Gender=unique(Patient$Gender),
                         Race=unique(Patient$Race)))

DiseaseProbs[Condition == 'Obesity', 
             Prob := .13]
DiseaseProbs[Condition == 'Obesity' & Gender == 'female', 
             Prob := Prob+0.04]
DiseaseProbs[Condition == 'Obesity' & Race != 'white', 
             Prob := Prob+0.03]
DiseaseProbs[Condition == 'Depression', 
             Prob := .07]
DiseaseProbs[Condition == 'Depression' & Gender == 'female', 
             Prob := Prob + 0.04]
DiseaseProbs[Condition == 'Depression' & Race != 'white', 
             Prob := Prob + 0.03]
DiseaseProbs[Condition == 'Hypertension',
             Prob := .30]
DiseaseProbs[Condition == 'Hypertension' & Gender == 'female', 
             Prob := Prob-0.04]
DiseaseProbs[Condition == 'Hypertension' & Race == 'black', 
             Prob := Prob+0.14]
DiseaseProbs[Condition == 'Drugs', Prob := .04]
DiseaseProbs[Condition == 'Alcohol', Prob := .08]
DiseaseProbs[Condition == 'Myocardial_infarction', 
             Prob := .06]
DiseaseProbs[Condition == 'Myocardial_infarction'& Gender == 'female', 
             Prob := 0.03]
DiseaseProbs[Condition == 'Congestive_heart_failure', 
             Prob := .06]
DiseaseProbs[Condition == 'Congestive_heart_failure' & Gender == 'female', 
             Prob := 0.03]
DiseaseProbs[Condition == 'Peripheral_vascular_disease', Prob := .024]
DiseaseProbs[Condition == 'Stroke', Prob := .03]
DiseaseProbs[Condition == 'Dementia', Prob := .032]
DiseaseProbs[Condition == 'Pulmonary', Prob := .071]
DiseaseProbs[Condition == 'Rheumatic', Prob := .012]
DiseaseProbs[Condition == 'Peptic ulcer Disease', Prob := .01]
DiseaseProbs[Condition == 'LiverMild', Prob := .01]
DiseaseProbs[Condition == 'Diabetes_without_complications', Prob := .1]
DiseaseProbs[Condition == 'Diabetes_with_complications', Prob := .04]
DiseaseProbs[Condition == 'Paralysis', Prob := 0.014]
DiseaseProbs[Condition == 'Renal', Prob := .036]
DiseaseProbs[Condition == 'Cancer', Prob := .05]
DiseaseProbs[Condition == 'LiverSevere', Prob := .05]
DiseaseProbs[Condition == 'Metastatic_solid_tumour', Prob := .032]
DiseaseProbs[Condition == 'HIV', Prob := .006]
DiseaseProbs[Condition == 'Peptic_ulcer_disease', Prob := .009]

## Create an analytic pivot patient file:
PatientAnalytic <- 
  Patient[,list(PatientID,DateOfBirth,Gender,Race)]
## Merge to long patient file
setkey(PatientAnalytic,Gender,Race)
setkey(DiseaseProbs,Gender,Race)
PatientAnalytic2 <- 
  PatientAnalytic[DiseaseProbs,allow.cartesian=T]
## Simulate distribution of diseases for cohort:
PatientAnalytic2[,disease:=rbinom(.N,1,prob=Prob)]
PatientAnalytic2[,mean(disease),by=Condition]
## rotate and merge disease indicators in
PatientAnalytic3 <- 
  dcast(PatientAnalytic2,PatientID + DateOfBirth + Gender + Race ~ Condition,value.var = 'disease')

## simulate patient level ED intensity effect
## older people more likely to use ED
PatientAnalytic[,ED_intensity:=rpois(.N,
                                     3
                                     - scale(DateOfBirth)*1.2)]
## simulate patient level outpatient intensity effect
## older people and women are more likely to go to outpatient doctor
PatientAnalytic[,Outpat_intensity:=rpois(.N,
                                         5
                                         - scale(DateOfBirth)*1
                                         + ifelse(Gender == 'female',1,0) * 3)]
PatientAnalytic[is.na(Outpat_intensity),Outpat_intensity:=3]
#### simulate mortality probability
## calculate X*B
x_b <- PatientAnalytic3[,1.5
                        + 0.7*Myocardial_infarction
                        + 0.4*Depression
                        + 0.4*Drugs
                        + 0.6*Alcohol
                        + 0.2*Obesity
                        + 0.4*Cancer
                        + 0.6*Pulmonary
                        + 0.9* LiverSevere
                        + 0.4*Congestive_heart_failure
                        + 0.8*Peripheral_vascular_disease
                        - as.numeric(scale(DateOfBirth))]
# Weibull latent event times
N=nrow(PatientAnalytic3)
lambda=0.1
rho = 1
beta=1
v <- runif(n=N)
Tlat <- (- log(v) / (lambda * exp(x_b)))^(1 / rho)
mean(Tlat)
# censoring times
rateC = 0.5
C <- rexp(n=N, rate=rateC)
# follow-up times and event indicators
time <- pmin(Tlat, C)
status <- as.numeric(Tlat <= C)
mean(status)
summary(time)
PatientAnalytic3[,time:=time]
PatientAnalytic3[,status:=status]
## Simulate first appointment time for patients
PatientAnalytic3[,First_Appointment_Date := sample(seq.Date(as.Date('2005-01-01'),
                                                            as.Date('2016-06-01'),by='days'),
                                                   size=.N, replace=T)]
## Identify last possible appointment date
PatientAnalytic3[,Last_Appointment_Date := pmin(as.Date('2018-06-01'),
                                                First_Appointment_Date + round(time*365.25*10))]
## Identify date of death
PatientAnalytic3[status==1,
                 DateOfDeath:=First_Appointment_Date + round(time*365.25*10)]
PatientAnalytic3[DateOfDeath > as.Date('2018-06-01'),DateOfDeath:=NA]
PatientAnalytic3[,summary(is.na(DateOfDeath))]

#### Create mortality Table ####
Mortality <- PatientAnalytic3[!is.na(DateOfDeath),list(PatientID,DateOfDeath)]

#### Create Outpatient Visits Table ####
possibleVisitDates <- seq.Date(as.Date('2005-01-01'),as.Date('2018-06-01'),by='days')
setkey(PatientAnalytic3,PatientID)
setkey(PatientAnalytic,PatientID)
Patient_outpat1 <- 
  PatientAnalytic3[PatientAnalytic,
                   list(PatientID,First_Appointment_Date,Last_Appointment_Date,Outpat_intensity)]
Patient_outpat1[,Fup_Years:=as.numeric(Last_Appointment_Date-First_Appointment_Date)/365.25]
Patient_outpat2 <-
  Patient_outpat1[,
                  list(First_Appointment_Date,Last_Appointment_Date,
                       VisitDate=sample(possibleVisitDates,
                                        size = Outpat_intensity*4)),
                  by=PatientID][order(PatientID,VisitDate)][VisitDate > First_Appointment_Date &
                                                              VisitDate <= Last_Appointment_Date]
### Merge in Diagnosis codes
PatientDiseaseLong <- 
  PatientAnalytic2[disease==1,list(PatientID,Condition)]
## join ICD10 codes
setkey(DiseaseMap,Condition)
setkey(PatientDiseaseLong,Condition)
PatientDiseaseLongICD <- 
  DiseaseMap[PatientDiseaseLong,
             list(PatientID,Condition,ICD10),
             allow.cartesian=T]
## Identify only one ICD10 code per patient-condition
PatientDiseaseLongICD[,order:=runif(.N)]
setkey(PatientDiseaseLongICD,PatientID,Condition,order)
PatientDiseaseLongICD2 <-
  PatientDiseaseLongICD[,list(ICD10=ICD10[1]),
                        by=list(PatientID,Condition)]
## Sample from these codes for each patient visit
setkey(Patient_outpat2,PatientID)
setkey(PatientDiseaseLongICD2,PatientID)
Patient_outpat3 <- 
  merge(Patient_outpat2,PatientDiseaseLongICD2,by = 'PatientID', all=T, allow.cartesian=T)
Patient_outpat3[,runi:=runif(.N)]
Patient_outpat3[is.na(ICD10),ICD10:='Z0000']
Patient_outpat3[,count:=1:.N,
                by=list(PatientID,VisitDate)]
setkey(Patient_outpat3,PatientID,VisitDate,runi)
Patient_outpat4 <- 
  Patient_outpat3[,
                  head(.SD,3),
                  by=list(PatientID,VisitDate)]
Patient_outpat4[,count:=1:.N,
                by=list(PatientID,VisitDate)]
Patient_outpat5 <-
  dcast(Patient_outpat4[,list(PatientID,VisitDate,ICD10,count)],
        PatientID + VisitDate ~ count,
        value.var = 'ICD10')
setnames(Patient_outpat5,
         old=3:5,
         new = c('ICD10_1','ICD10_2','ICD10_3'))
### Add Clinic and provider
t1 <- merge(Staff,Clinic,by='ClinicCode',all=F)
## add in primary care for wellness exams
ind1 <- sample(t1[ClinicDescription %in% c('Primary Care'),.N],
               size = Patient_outpat5[ICD10_1 == 'Z0000',.N],
               replace = T)
Patient_outpat5[ICD10_1 == 'Z0000',
                ClinicCode:=t1[ClinicDescription %in% c('Primary Care')][ind1,ClinicCode]]
Patient_outpat5[ICD10_1 == 'Z0000',
                StaffID:=t1[ClinicDescription %in% c('Primary Care')][ind1,StaffID]]
# add in all others
ind2 <- sample(t1[ClinicDescription %in% c('Primary Care','Specialty Care',"Emergency Department"),.N],
               size = Patient_outpat5[ICD10_1 != 'Z0000',.N],
               replace = T)
Patient_outpat5[ICD10_1 != 'Z0000',
                ClinicCode:=t1[ClinicDescription %in% c('Primary Care','Specialty Care',"Emergency Department")][ind2,ClinicCode]]
Patient_outpat5[ICD10_1 != 'Z0000',
                StaffID:=t1[ClinicDescription %in% c('Primary Care','Specialty Care',"Emergency Department")][ind2,StaffID]]
OutpatientVisit <- Patient_outpat5

## Add in Final Numbering
OutpatientVisit[,VisitID := 1:.N]

#### ICD10 ####
ICD10_t <- PatientDiseaseLongICD[,list(ICD10=unique(ICD10))]
ICD10 <- data.table(icd_explain_table(ICD10_t$ICD10))

#### Insurance Type ####
n_insuranceProviders <- 15
InsuranceProvider <- data.table(InsuranceProviderID=1:n_insuranceProviders,
                                InsuranceType=c('Medicaid','Medicare','Uninsured',
                                                rep('Private',n_insuranceProviders-3)))
#### Add in PatientInsurance ####
PatientInsurance1 <- 
  PatientAnalytic3[,list(PatientID,CoverageStartDate=First_Appointment_Date,DateOfBirth)]
PatientInsurance1[,age:=as.numeric(CoverageStartDate-DateOfBirth)/365.25]
PatientInsurance1[age>65,InsuranceProviderID:= 2]
PatientInsurance1[is.na(InsuranceProviderID),
                  InsuranceProviderID:=sample(InsuranceProvider$InsuranceProviderID,replace=T,size=.N)]
## add a few longitudinal changes
PatientInsurance2 <- 
  OutpatientVisit[sample(.N,size=.N*0.08),list(PatientID,CoverageStartDate=VisitDate)]
setkey(PatientInsurance2,PatientID)
setkey(Patient,PatientID)
PatientInsurance2[Patient,DateOfBirth:=i.DateOfBirth]
PatientInsurance2[,age:=as.numeric(CoverageStartDate-DateOfBirth)/365.25]
PatientInsurance2[age>65,InsuranceProviderID:= 2]
PatientInsurance2[is.na(InsuranceProviderID),
                  InsuranceProviderID:=sample(InsuranceProvider$InsuranceProviderID,replace=T,size=.N)]
PatientInsurance <-
  rbind(PatientInsurance1,PatientInsurance2)
PatientInsurance[,PatientInsuranceID:=1:.N]

## Add 'Dirty Race elements file elements
Patient[,RaceDirty2:=RaceDirty]
Patient[sample(.N,size = 0.03*.N),
        RaceDirty2:=ifelse(Gender == 'female',
                           sample(c('999','?'),replace=T,size = .N),
                           RaceDirty)]
Patient[,.N,by=RaceDirty2]
## Add Income
Patient[,Income:=rlnorm(.N,3,0.8)*2.8]
Patient[sample(.N,size = 0.04*.N),Income:=Income + 1000]
Patient[sample(.N,size = 0.07*.N),Income:=NA]

#### Write out Files
write.csv(x = Patient[,list(PatientID,FirstName,LastName,State,ZipCode=Zipcode,
                            DateOfBirth=DateOfBirthDirty,Gender=GenderDirty,Race=RaceDirty2,Income)],
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/Patient.csv',
          row.names = F,na = '')
write.csv(x = PatientInsurance[!is.na(CoverageStartDate),
                               list(PatientID,PatientInsuranceID,InsuranceProviderID,CoverageStartDate)],
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/PatientInsurance.csv',
          row.names = F,na = '')
write.csv(x = InsuranceProvider,
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/InsuranceProvider.csv',
          row.names = F,na = '')
write.csv(x = Staff[,list(StaffID,FirstName,LastName,Gender,HireDate=Hiredate,HourlyRate,Salary,
                          PayType,StaffType=Title,StaffReportsTo=ReportsTo)],
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/Staff.csv',
          row.names = F,na = '')
write.csv(x = OutpatientVisit[,list(VisitID,StaffID,PatientID,VisitDate,ICD10_1,ICD10_2,ICD10_3,ClinicCode)],
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/OutpatientVisit.csv',
          row.names = F,na = '')
write.csv(x = ICD10[,list(ICD10=code,ICD10Descr=short_desc)],
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/ICDCodes.csv',
          row.names = F,na = '')
write.csv(x = Clinic,
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/Clinic.csv',
          row.names = F,na = '')
write.csv(x = Mortality,
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/Mortality.csv',
          row.names = F,na = '')
write.csv(x = DiseaseMap[,list(DiseaseMapID,ICD10,Condition)],
          file = 'C:/Users/evancarey///Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/DiseaseMap.csv',
          row.names = F,na = '')

write.csv(x = PatientAnalytic3[,list(PatientID, DateOfBirth, Gender, Race, Myocardial_infarction, 
                                     Congestive_heart_failure, Peripheral_vascular_disease, Stroke,
                                     Dementia, Pulmonary, Rheumatic, Peptic_ulcer_disease, 
                                     LiverMild, Diabetes_without_complications, Diabetes_with_complications, 
                                     Paralysis, Renal, Cancer, LiverSevere, Metastatic_solid_tumour, 
                                     HIV, Obesity, Depression, Hypertension, Drugs, Alcohol,
                                     First_Appointment_Date, Last_Appointment_Date, DateOfDeath)],
          file = 'C:/Users/evancarey//Dropbox/Work/BHAnalytics/Data_Sims/healthcare2/PatientAnalyticFile.csv',
          row.names = F,na = '')
