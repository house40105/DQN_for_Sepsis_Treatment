library(readr)
library(rio)
library(dplyr)
library("ggplot2")
library(tensorflow)
library(keras)
library(DMwR2)
library(mice)
library(VIM)
library(naniar)
library(class)
library(patchwork)
library(egg)
library(ggpubr)



#Read data from raw data files
ADMISSIONS_RawData<-read.csv("ADMISSIONS.csv",header=T) #患者入院當下資訊
static<-read.csv("PATIENTS.csv",header=T) #患者基本訊息

chartevents<-read.csv("CHARTEVENTS.csv",header=T) #ICU患者生理數值
#itemid可由D_ITEMS.csv或D_LABITEMS.csv查詢


ICUSTAYS_RawData<-read.csv("ICUSTAYS.csv",header=T)#患者入住ICU資訊

#兩系統的給藥資訊
fluids_cv <- read.csv("INPUTEVENTS_CV.csv",header=T)
fluids_mv <- read.csv("INPUTEVENTS_MV.csv",header=T)
#患者用藥紀錄(drug_name_poe.vasopressin)
drugs <- read.csv("PRESCRIPTIONS.csv",header=T)

#GET ICD9 code for sepsis
d_icd_diagnoses <- read.csv("D_ICD_DIAGNOSES.csv",header=T)



#filter all sepsis icd9 via keyword
i_myICDs <- grep("Septic",d_icd_diagnoses[,4])
i_myICDs <- c(i_myICDs,grep("epsis",d_icd_diagnoses[,4]))
i_myICDs <- c(i_myICDs,grep("ystemic inflammatory",d_icd_diagnoses[,4]))
i_myICDs <- c(i_myICDs,grep("septic",d_icd_diagnoses[,4]))

j <- i_myICDs %in% grep("Aseptic",d_icd_diagnoses[,4])
i_myICDs <- i_myICDs[!j]
j <- i_myICDs %in% grep("aseptic",d_icd_diagnoses[,4])
i_myICDs <- i_myICDs[!j]

i_myICDs <- unique(i_myICDs)# get sepsis icd9 code id 

#sepsis icd9 code list
SEPSIS_D_ICD_DIAGNOSES_RawData<-d_icd_diagnoses[i_myICDs,]
export(SEPSIS_D_ICD_DIAGNOSES_RawData, "SEPSIS_D_ICD_DIAGNOSES.csv")#export sepsis icd9 code list and table


#SEPSIS_D_ICD_DIAGNOSES_RawData<-read.csv("SEPSIS_D_ICD_DIAGNOSES.csv",header=T)#Sepsis ICD9 Code
#SIRS, Sepsis, Severe Sepsis, and Septic



#s_myPatients
myICDs <- d_icd_diagnoses[i_myICDs,2]#index to icd9 code

diagnoses_icd <- read.csv("DIAGNOSES_ICD.csv",header=T)#患者ICU診斷ICD9
i <- diagnoses_icd[,5] %in% myICDs
myPatients <- unique(paste(diagnoses_icd[i,2],diagnoses_icd[i,3]))#篩選所有spesis p't入院 base on ICD9 
export(diagnoses_icd[i,], "s_myPatients.csv")#export all sepsis p't list #匯出26名spesis患者名單subject_id&hadm_id
#end s_myPatients

#count Patients' gender
p_sid <-unique(diagnoses_icd[diagnoses_icd[,5] %in% myICDs,2])
p_sid <- p_sid[p_sid[]!="10120"]
p_gender <- static$gender[static$subject_id %in% p_sid]


#myChartevents #篩選所有sepsis患者觀察數據
j <- paste(chartevents[,2],chartevents[,3]) %in% myPatients #2subject id #3hadm id
myChartevents <- chartevents[j,]
export(myChartevents, "myChartevents.csv")
#dim(myChartevents)
#row 267006 #col 15

#end myChartevents

d_items<-read.csv("D_ITEMS.csv",header=T) #itemscode



#1.vital signs(Total:11)
#heart_rate
id_heart_rate <- d_items[grep("Heart Rate",d_items[,3])[c(1,3)],2]

#respiratory rate
id_respiratory_rate <- d_items[grep("Respiratory Rate",d_items[,3])[c(1,3)],2]

#blood pressure
id_blood_pressure_systolic <- d_items[grep("Arterial Blood Pressure",d_items[,3])[c(1)],2]
id_blood_pressure_diastolic <- d_items[grep("Arterial Blood Pressure",d_items[,3])[c(2)],2]
id_blood_pressure_mean <- d_items[grep("Arterial Blood Pressure",d_items[,3])[c(3)],2]

#body temperature
id_body_temperature <- d_items[grep("emperature",d_items[,3])[c(6,7,9)],2]#6=Temperature F,678 #7=Temperature F(calc),679 #9=Temperature Fahrenheit,223761
#Skin Temperature(TEXT),224027

#Weight (KG)
id_weight <- d_items[grep("Weight",d_items[,3])[c(4,13,14,16)],2]
#d_items[ d_items$itemid %in% id_weight,]#print & check item

#Arterial PaCO2
id_PaCO2 <- d_items[grep("Arterial PaCO2",d_items[,3]),2]
d_items[ d_items$itemid %in% id_PaCO2,]#print & check item

#Arterial PaO2
id_PaO2 <- d_items[grep("PaO2",d_items[,3],ignore.case=TRUE),2]
d_items[ d_items$itemid %in% id_PaO2,]#print & check item

#FiO2 (Inspired O2 Fraction)
id_FiO2 <- d_items[grep("Inspired O2 Fraction",d_items[,3]),2]
d_items[ d_items$itemid %in% id_FiO2,]#print & check item

#SpO2
id_spo2 <- grep("SpO2",d_items$label)
id_spo2 <- c(id_spo2,grep("SpO2",d_items$abbreviation))
id_spo2 <- id_spo2[c(1,6)]#SpO2,646,220277
id_spo2 <- d_items[c(id_spo2),2]#index into item_id
#end vital signs (total: 11)


#2.Lab value (total: 19)
category_label <- grep("Labs",d_items[,7])#7=category label

id_WBC <- grep("WBC",d_items[,3],ignore.case=TRUE)[c(1,2,4,5)]#White Blood Cell
id_WBC <- d_items[id_WBC,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_WBC,]#print & check item

id_Bilirubin <- grep("bilirubin",d_items[category_label,3],ignore.case=TRUE)#bilirubin
id_Bilirubin <- d_items[category_label[c(id_Bilirubin)],2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Bilirubin,]#print & check item

id_Platelet <- grep("Platelet Count",d_items[category_label,3],ignore.case=TRUE)[c(2)]
id_Platelet <- d_items[category_label[c(id_Platelet)],2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Platelet,]#print & check item


id_Albumin <- grep("Albumin",d_items[category_label,3],ignore.case=TRUE)[c(2)]
id_Albumin <- d_items[category_label[c(id_Albumin)],2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Albumin,]#print & check item

id_ArterialPH <- grep("Arterial pH",d_items[,3],ignore.case=TRUE)
id_ArterialPH <- d_items[id_ArterialPH,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_ArterialPH,]#print & check item

id_Calcium <- grep("Calcium",d_items[,3],ignore.case=TRUE)[c(16)]
id_Calcium <- d_items[id_Calcium,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Calcium,]#print & check item

id_Glucose <- grep("Glucose",d_items[,3],ignore.case=TRUE)[c(9)]
id_Glucose <- d_items[id_Glucose,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Glucose,]#print & check item

id_Hemoglobin <- grep("Hemoglobin",d_items[,3],ignore.case=TRUE)[c(1,3)]
id_Hemoglobin <- d_items[id_Hemoglobin,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Hemoglobin,]#print & check item

id_Magnesium <- grep("Magnesium",d_items[,3],ignore.case=TRUE)[c(3,9)]
id_Magnesium <- d_items[id_Magnesium,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Magnesium,]#print & check item

id_PTT <- grep("PTT",d_items[,3])[c(2,4)]
id_PTT <- d_items[id_PTT,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_PTT,]#print & check item

id_Potassium <- grep("Potassium",d_items[,3])[c(6)]
id_Potassium <- d_items[id_Potassium,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Potassium,]#print & check item

id_BUN <- grep("BUN",d_items[,3])[c(1,2,5)]
id_BUN <- d_items[id_BUN,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_BUN,]#print & check item

id_Chloride <- grep("Chloride",d_items[,3])[c(7)]
id_Chloride <- d_items[id_Chloride,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Chloride,]#print & check item

id_INR <- grep("INR",d_items[,3])[c(2,6)]
id_INR <- d_items[id_INR,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_INR,]#print & check item

id_Sodium <- grep("Sodium",d_items[,3])[c(7)]
id_Sodium <- d_items[id_Sodium,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Sodium,]#print & check item

id_TCO2 <- grep("TCO2",d_items[,3])[c(7)]
id_TCO2 <- d_items[id_TCO2,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_TCO2,]#print & check item

id_Creatinine <- grep("Creatinine",d_items[,3])[c(3,4)]
id_Creatinine <- d_items[id_Creatinine,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_Creatinine,]#print & check item

id_PT <- grep("PT",d_items[,3])[c(10)]#Prothrombin time
id_PT <- c(id_PT,grep("PT",d_items$abbreviation)[c(12)])
id_PT <- d_items[id_PT,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_PT,]#print & check item

#end Lab value (total: 19)

#GCS (Glasgow Coma Scale)
id_GCS <- grep("GCS",d_items[,3],ignore.case=TRUE)[c(1,5)]
id_GCS <- d_items[id_GCS,2]#category_label index into item_id
d_items[ d_items$itemid %in% id_GCS,]#print & check item

#id_lab_value <- c(id_lab_value,grep("CBC",d_items[category_label,3],ignore.case=TRUE))#CBC #Complete blood count 
#id_lab_value <- c(id_lab_value,grep("Lactate",d_items[category_label,3],ignore.case=TRUE))#Lacate # high level of lactic acid caused by infection
#id_lab_value <- c(id_lab_value,grep("C-Reactive Protein",d_items[category_label,3],ignore.case=TRUE))#CRP
#id_lab_value <- c(id_lab_value,grep("Blood Cultures",d_items[category_label,3],ignore.case=TRUE))
#serious effects on blood clotting
#id_lab_value <- c(id_lab_value,grep("D-Dimer",d_items[category_label,3],ignore.case=TRUE))
#More testing
#id_lab_value <- c(id_lab_value,grep("Endotoxin",d_items[category_label,3],ignore.case=TRUE))#a component of certain bacteria
#id_lab_value <- c(id_lab_value,grep("Procalcitonin",d_items[category_label,3],ignore.case=TRUE))#PCT rises if have infection
#id_lab_value <- c(id_lab_value,grep("creatinine ",d_items[category_label,3],ignore.case=TRUE))#creatinine 

#id_lab_value <- d_items[category_label[c(id_lab_value)],2]#category_label index into item_id
#d_items[ d_items$itemid %in% id_lab_value,]#print & check item




# plots check
vitals <- myChartevents

for (i in 1:length(myPatients)){
	print(length(which(paste(vitals[,2],vitals[,3]) == myPatients[i] & vitals$itemid %in% id_weight)))
}

 
j <- which(paste(vitals[,2],vitals[,3]) == myPatients[37] & vitals$itemid %in% id_heart_rate)
k <- which(paste(vitals[,2],vitals[,3]) == myPatients[37] & vitals$itemid %in% id_blood_pressure_mean)
l <- which(paste(vitals[,2],vitals[,3]) == myPatients[37] & vitals$itemid %in% id_respiratory_rate)
m <- which(paste(vitals[,2],vitals[,3]) == myPatients[37] & vitals$itemid %in% id_weight)
plot(as.POSIXct(vitals$charttime[k], format="%Y-%m-%d %H:%M:%S"),vitals$valuenum[k],ylim=c(0,150))
points(as.POSIXct(vitals$charttime[j], format="%Y-%m-%d %H:%M:%S"),vitals$valuenum[j],col=2)
points(as.POSIXct(vitals$charttime[l], format="%Y-%m-%d %H:%M:%S"),vitals$valuenum[l],col=3)
lines(as.POSIXct(vitals$charttime[m], format="%Y-%m-%d %H:%M:%S"),vitals$valuenum[m],col=4)



windows()
qplot(x=as.character(subject_id),y=valuenum,data=vitals,color= valuenum)

windows()
i <- vitals$itemid %in% id_respiratory_rate
pl <- vitals[i,]#
qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Respiratory Rate')+theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))
#go to print.txt

#windows()
#canvas <- ggplot(data=pl)
#canvas +geom_boxplot(aes(x=as.character(subject_id),y=valuenum)) +facet_grid(.~as.character(subject_id))

range(as.numeric(as.POSIXct(vitals$charttime[j], format="%Y-%m-%d %H:%M:%S")))


#Dopamine,Dobutamine,epinephrine ,Norepinephrine 
for (i in 1:length(myPatients)) {
    j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]), c(grep("Dopamine",drugs$drug,ignore.case=TRUE), grep("Dobutamine",drugs$drug,ignore.case=TRUE), grep("epinephrine",drugs$drug,ignore.case=TRUE), grep("norepinephrine",drugs$drug,ignore.case=TRUE)))
    print(i)
    print(j)
}

tmp <- NULL
for (i in 1:length(myPatients)) {
    j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]),grep("dopamine",drugs$drug,ignore.case=TRUE))
    if (length(j) > 0) tmp <- c(tmp,as.numeric(as.character(drugs$dose_val_rx[j])))
    #print(i)
}
par(mfrow=c(2,2))
hist(tmp,breaks=50,xlim = c(0,800),border = "black",main="Dopamine",xlab = "Dose")

tmp <- NULL
for (i in 1:length(myPatients)) {
    j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]),grep("dobutamine",drugs$drug,ignore.case=TRUE))
    if (length(j) > 0) tmp <- c(tmp,as.numeric(as.character(drugs$dose_val_rx[j])))
    #print(i)
}
hist(tmp,breaks=50,xlim = c(0,800),border = "black",main="Dobutamine",xlab = "Dose")

tmp <- NULL
for (i in 1:length(myPatients)) {
    j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]),grep("epinephrine",drugs$drug,ignore.case=TRUE))
    if (length(j) > 0) tmp <- c(tmp,as.numeric(as.character(drugs$dose_val_rx[j])))
    #print(i)
}
hist(tmp,breaks=50,xlim = c(0,800),border = "black",main="Epinephrine",xlab = "Dose")

tmp <- NULL
for (i in 1:length(myPatients)) {
    j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]),grep("norepinephrine",drugs$drug,ignore.case=TRUE))
    if (length(j) > 0) tmp <- c(tmp,as.numeric(as.character(drugs$dose_val_rx[j])))
    #print(i)
}
hist(tmp,breaks=50,xlim = c(0,800),border = "black",main="Norepinephrine",xlab = "Dose")

tmp_iv <- NULL
for (i in 1:length(myPatients)){
	j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]), grep("IV", drugs$route))#ALL Intravenous therapy (IV, IV DRIP, IV BOLUS, IV PCA )
	if(length(j) > 0) tmp_iv <- c(tmp_iv,as.numeric(as.character(drugs$dose_val_rx[j])))#
}
windows()
hist(tmp_iv,breaks=250,xlim = c(0,6000),border = "black",main="IV",xlab = "Dose")


for (i in 1:length(tmp_iv)){
	if(tmp_iv[i]>6000 & !is.na(tmp_iv[i])) print(paste(drugs$subject_id[i],drugs$drug[i],i,tmp_iv[i]))
}

#remove error valuenum (vitals$error == 1)
error_ID <- which(vitals$error == 0 | is.na(vitals$error))
vitals <- vitals[error_ID,]

# tuples
# St,At,Rt,S't




S_HR  <- NA #Heart Rate
S_RR  <- NA #Respiratory Rate
S_BPs <- NA #Blood Pressure Systolic
S_BPd <- NA #Blood Pressure Diastolic   
S_BPm <- NA #Blood Pressure Mean  
S_T   <- NA #Temperature (F)
S_W   <- NA #Weight (KG)
S_PaCO2 <- NA #Arterial PaCO2
S_PaO2  <- NA #Arterial PaO2
S_FiO2  <- NA #FiO2 (Inspired O2 Fraction)
S_SpO2  <- NA #SpO2
S_GCS <- NA 

S_WBC  <- NA #White Blood Cell
S_TBI  <- NA #Bilirubin total (TBI)
S_PC   <- NA #Platelet Count
S_ALB  <- NA #Albumin
S_PH   <- NA #Arterial pH
S_Ca   <- NA #Calcium
S_AC   <- NA #Glucose
S_Hb   <- NA #Hemoglobin
S_Mg   <- NA #Magnesium
S_PTT  <- NA #Partial Thromboplastin Time
S_K    <- NA #Potassium
S_BUN  <- NA #Blood Urea Nitrogen
S_Cl   <- NA #Chloride
S_INR  <- NA #International Normalized Ratio
S_Na   <- NA #Sodium
S_TCO2 <- NA #TCO2
S_Cr   <- NA #Creatinine
S_PT   <- NA #Prothrombin time



A_IV  <- NA
A_VP  <- NA
R     <- NA

H     <- NA
h     <- 1

S_age <- NA #additional state

for (i in 1:length(myPatients)) {
    patient_a  <- vitals[which(paste(vitals[,2],vitals[,3]) == myPatients[i]),]
	dob <- static$dob[which(static$subject_id == patient_a$subject_id[1])]#age
    dob <- as.numeric(as.POSIXct(dob, format="%Y-%m-%d %H:%M:%S"))
	deathtime <- ADMISSIONS_RawData$deathtime[which(paste(ADMISSIONS_RawData[,2],ADMISSIONS_RawData[,3]) == myPatients[i])]
	deathtime <- as.numeric(as.POSIXct(deathtime, format="%Y-%m-%d %H:%M:%S"))
    icu <- unique(patient_a$icustay_id)
    icu <- icu[!is.na(icu)]
    if (length(icu) > 0) {
     for (l in icu) {
        patient_v  <- patient_a[patient_a[,4] == l,] # events in a paticular icu stay
        patient_d  <- drugs[which(paste(drugs[,2],drugs[,3]) == myPatients[i] & drugs[,4] == l),]#2subject 3hadm 4icu# drugs in a patient admission and icu
        #patient_p  <- procedures_mv[which(paste(procedures_mv[,2],procedures_mv[,3]) == myPatients[i] & procedures_mv[,4] == l),] # procedures in a patient adm and icu
        patient_vt <- as.numeric(as.POSIXct(patient_v$charttime, format="%Y-%m-%d %H:%M:%S")) # times
        patient_dt <- as.numeric(as.POSIXct(patient_d$startdate, format="%Y-%m-%d %H:%M:%S")) # times
        patient_pt <- as.numeric(as.POSIXct(patient_d$enddate, format="%Y-%m-%d %H:%M:%S"))   # times

        startSec <- range(c(patient_vt,patient_dt,patient_pt),na.rm=T)[1]
        endSec   <- range(c(patient_vt,patient_dt,patient_pt),na.rm=T)[2]

        myT <- startSec
        while (myT < endSec) {
			# every hour
			k <- patient_vt >= myT & patient_vt < (myT + 3600)
			
			# states
            j <- which(k & patient_v$itemid %in% id_heart_rate)
            if (length(j) > 0) S_HR <- c(S_HR, mean(patient_v$valuenum[j],na.rm=T))
            else S_HR <- c(S_HR, S_HR[length(S_HR)])
			
			j <- which(k & patient_v$itemid %in% id_respiratory_rate)
            if (length(j) > 0) S_RR <- c(S_RR, mean(patient_v$valuenum[j],na.rm=T))
            else S_RR <- c(S_RR, S_RR[length(S_RR)])
            
			j <- which(k & patient_v$itemid %in% id_blood_pressure_systolic)
            if (length(j) > 0) S_BPs <- c(S_BPs, mean(patient_v$valuenum[j],na.rm=T))
            else S_BPs <- c(S_BPs, S_BPs[length(S_BPs)])
			
			j <- which(k & patient_v$itemid %in% id_blood_pressure_diastolic)
            if (length(j) > 0) S_BPd <- c(S_BPd, mean(patient_v$valuenum[j],na.rm=T))
            else S_BPd <- c(S_BPd, S_BPd[length(S_BPd)])
			
			j <- which(k & patient_v$itemid %in% id_blood_pressure_mean)
            if (length(j) > 0) S_BPm <- c(S_BPm, mean(patient_v$valuenum[j],na.rm=T))
            else S_BPm <- c(S_BPm , S_BPm[length(S_BPm)])
			
			j <- which(k & patient_v$itemid %in% id_body_temperature)
            if (length(j) > 0) S_T <- c(S_T, mean(patient_v$valuenum[j],na.rm=T))
            else S_T <- c(S_T , S_T[length(S_T)])
			
			j <- which(k & patient_v$itemid %in% id_weight)
            if (length(j) > 0) S_W <- c(S_W, mean(patient_v$valuenum[j],na.rm=T))
            else S_W <- c(S_W , S_W[length(S_W)])
			
			j <- which(k & patient_v$itemid %in% id_PaCO2)
            if (length(j) > 0) S_PaCO2 <- c(S_PaCO2, mean(patient_v$valuenum[j],na.rm=T))
            else S_PaCO2 <- c(S_PaCO2 , S_PaCO2[length(S_PaCO2)])
			
			j <- which(k & patient_v$itemid %in% id_PaO2)
            if (length(j) > 0) S_PaO2 <- c(S_PaO2, mean(patient_v$valuenum[j],na.rm=T))
            else S_PaO2 <- c(S_PaO2 , S_PaO2[length(S_PaO2)])
			
			j <- which(k & patient_v$itemid %in% id_FiO2)
            if (length(j) > 0) S_FiO2 <- c(S_FiO2, mean(patient_v$valuenum[j],na.rm=T))
            else S_FiO2 <- c(S_FiO2 , S_FiO2[length(S_FiO2)])
			
			j <- which(k & patient_v$itemid %in% id_spo2)
            if (length(j) > 0) S_SpO2 <- c(S_SpO2, mean(patient_v$valuenum[j],na.rm=T))
            else S_SpO2 <- c(S_SpO2 , S_SpO2[length(S_SpO2)])
			
			j <- which(k & patient_v$itemid %in% id_GCS)
            if (length(j) > 0) S_GCS <- c(S_GCS, mean(patient_v$valuenum[j],na.rm=T))
            else S_GCS <- c(S_GCS, S_GCS[length(S_GCS)])
			
			
			j <- which(k & patient_v$itemid %in% id_WBC)
            if (length(j) > 0) S_WBC <- c(S_WBC, mean(patient_v$valuenum[j],na.rm=T))
            else S_WBC <- c(S_WBC , S_WBC[length(S_WBC)])
			
			j <- which(k & patient_v$itemid %in% id_Bilirubin)
            if (length(j) > 0) S_TBI <- c(S_TBI, mean(patient_v$valuenum[j],na.rm=T))
            else S_TBI <- c(S_TBI , S_TBI[length(S_TBI)])
			
			j <- which(k & patient_v$itemid %in% id_Platelet)
            if (length(j) > 0) S_PC <- c(S_PC, mean(patient_v$valuenum[j],na.rm=T))
            else S_PC <- c(S_PC , S_PC[length(S_PC)])
			
			j <- which(k & patient_v$itemid %in% id_Albumin)
            if (length(j) > 0) S_ALB <- c(S_ALB, mean(patient_v$valuenum[j],na.rm=T))
            else S_ALB <- c(S_ALB , S_ALB[length(S_ALB)])
			
			j <- which(k & patient_v$itemid %in% id_ArterialPH)
            if (length(j) > 0) S_PH <- c(S_PH, mean(patient_v$valuenum[j],na.rm=T))
            else S_PH <- c(S_PH , S_PH[length(S_PH)])
			
			j <- which(k & patient_v$itemid %in% id_Calcium)
            if (length(j) > 0) S_Ca <- c(S_Ca, mean(patient_v$valuenum[j],na.rm=T))
            else S_Ca <- c(S_Ca , S_Ca[length(S_Ca)])
			
			j <- which(k & patient_v$itemid %in% id_Glucose)
            if (length(j) > 0) S_AC <- c(S_AC, mean(patient_v$valuenum[j],na.rm=T))
            else S_AC <- c(S_AC , S_AC[length(S_AC)])
			
			j <- which(k & patient_v$itemid %in% id_Hemoglobin)
            if (length(j) > 0) S_Hb <- c(S_Hb, mean(patient_v$valuenum[j],na.rm=T))
            else S_Hb <- c(S_Hb , S_Hb[length(S_Hb)])
			
			j <- which(k & patient_v$itemid %in% id_Magnesium)
            if (length(j) > 0) S_Mg <- c(S_Mg, mean(patient_v$valuenum[j],na.rm=T))
            else S_Mg <- c(S_Mg , S_Mg[length(S_Mg)])
			
			j <- which(k & patient_v$itemid %in% id_PTT)
            if (length(j) > 0) S_PTT <- c(S_PTT, mean(patient_v$valuenum[j],na.rm=T))
            else S_PTT <- c(S_PTT , S_PTT[length(S_PTT)])
			
			j <- which(k & patient_v$itemid %in% id_Potassium)
            if (length(j) > 0) S_K <- c(S_K, mean(patient_v$valuenum[j],na.rm=T))
            else S_K <- c(S_K , S_K[length(S_K)])
			
			j <- which(k & patient_v$itemid %in% id_BUN)
            if (length(j) > 0) S_BUN <- c(S_BUN, mean(patient_v$valuenum[j],na.rm=T))
            else S_BUN <- c(S_BUN , S_BUN[length(S_BUN)])
			
			j <- which(k & patient_v$itemid %in% id_Chloride)
            if (length(j) > 0) S_Cl <- c(S_Cl, mean(patient_v$valuenum[j],na.rm=T))
            else S_Cl <- c(S_Cl , S_Cl[length(S_Cl)])
			
			j <- which(k & patient_v$itemid %in% id_INR)
            if (length(j) > 0) S_INR <- c(S_INR, mean(patient_v$valuenum[j],na.rm=T))
            else S_INR <- c(S_INR , S_INR[length(S_INR)])
			
			j <- which(k & patient_v$itemid %in% id_Sodium)
            if (length(j) > 0) S_Na <- c(S_Na, mean(patient_v$valuenum[j],na.rm=T))
            else S_Na <- c(S_Na , S_Na[length(S_Na)])
			
			j <- which(k & patient_v$itemid %in% id_TCO2)
            if (length(j) > 0) S_TCO2 <- c(S_TCO2, mean(patient_v$valuenum[j],na.rm=T))
            else S_TCO2 <- c(S_TCO2 , S_TCO2[length(S_TCO2)])
			
			j <- which(k & patient_v$itemid %in% id_Creatinine)
            if (length(j) > 0) S_Cr <- c(S_Cr, mean(patient_v$valuenum[j],na.rm=T))
            else S_Cr <- c(S_Cr , S_Cr[length(S_Cr)])
			
			j <- which(k & patient_v$itemid %in% id_PT)
            if (length(j) > 0) S_PT <- c(S_PT, mean(patient_v$valuenum[j],na.rm=T))
            else S_PT <- c(S_PT , S_PT[length(S_PT)])
			
			S_age <- c(S_age, ((myT + 1800 - dob) %/% (365*24*60*60)))#demographic
			

			#A_VP
			tmp_dp<-0#dopamine
			tmp_db<-0#dobutamine
			tmp_ep<-0#epinephrine
			tmp_np<-0#norepinephrine
			
            k <- patient_dt >= myT & patient_dt < (myT + 3600)
            tmp_vp <- NULL
            j <- intersect(which(k), grep("dopamine", patient_d$drug,ignore.case=TRUE))
            if (length(j) > 0) {
				tmp_vp <- c(tmp_vp, sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T)/4)#max400
				tmp_dp<-sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T)
			}
            j <- intersect(which(k), grep("dobutamine",patient_d$drug,ignore.case=TRUE))
            if (length(j) > 0) {
				tmp_vp <- c(tmp_vp, sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T)/25)#max250
				tmp_db<-sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T)
			}
            j <- intersect(which(k), grep("epinephrine",patient_d$drug,ignore.case=TRUE))
            if (length(j) > 0) {
				tmp_vp <- c(tmp_vp, sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T)/8)#max800
				tmp_ep<-sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T)
				
            }
			j <- intersect(which(k), grep("norepinephrine",patient_d$drug,ignore.case=TRUE))
            if (length(j) > 0) {
				tmp_vp <- c(tmp_vp, sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T)/8)#max800
				tmp_np<-sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T)
				
			}

            if (!is.null(tmp_vp)) A_VP <- c(A_VP, sum(tmp_vp,na.rm=T)/214*100)#normalization
            else A_VP <- c(A_VP, 0)
			
			
			#A_IV
			tmp_iv <- NULL
			j <- intersect(which(k), grep("IV", patient_d$route))#ALL Intravenous therapy (IV, IV DRIP, IV BOLUS, IV PCA )
            if (length(j) > 0) tmp_iv <- c(tmp_iv, sum(as.numeric(as.character(patient_d$dose_val_rx[j])),na.rm=T))#

			
			if (!is.null(tmp_iv)) A_IV <- c(A_IV, sum(tmp_iv,na.rm=T)/(30786.50)*100)#max 30786.50 #normalization 0-100
            else A_IV <- c(A_IV, 0)

 

            #R #SOFA
			T <- length(S_HR)
            tmp_r <- 0
            #if (!is.na(S_GCS[T]))#GCS
               #if (S_GCS[T] >= 15) tmp_r <- tmp_r - 0
			   #else if (S_GCS[T] < 15 & S_GCS[T] >=13) tmp_r <- tmp_r - 1
			   #else if (S_GCS[T] < 13 & S_GCS[T] >=10) tmp_r <- tmp_r - 2
			   #else if (S_GCS[T] < 9 & S_GCS[T] >=6) tmp_r <- tmp_r - 3
			   #else if (S_GCS[T] < 6) tmp_r <- tmp_r - 4
			
			#if (!is.na(S_BPm[T]))#Mean arterial pressure OR administration of vasopressors required (mg)
               #if (S_BPm[T] < 70) tmp_r <- tmp_r - 1
			
			#if (tmp_dp > 0 | tmp_db > 0 | tmp_ep > 0 | tmp_np > 0)
			   #if (tmp_dp <= 5000 | tmp_db > 0) tmp_r <- tmp_r - 2
			   #else if (tmp_dp > 5000 | tmp_ep <= 100 | tmp_np <= 100) tmp_r <- tmp_r - 3
			   #else if (tmp_dp > 15000 | tmp_ep > 100 | tmp_np > 100) tmp_r <- tmp_r - 4
			#print(paste(l,'dp ',tmp_dp,'db ',tmp_db,'ep ',tmp_ep,'np ',tmp_np))
			   
			#if (!is.na(S_PaO2[T]) & !is.na(S_FiO2[T]))#Respiratory system #PaO2/FiO2[mmHg]
               #if ((S_PaO2[T]/S_FiO2[T]) < 400) tmp_r <- tmp_r - 1
			   #else if ((S_PaO2[T]/S_FiO2[T]) < 300) tmp_r <- tmp_r - 2
			   #else if ((S_PaO2[T]/S_FiO2[T]) < 200) tmp_r <- tmp_r - 3
			   #else if ((S_PaO2[T]/S_FiO2[T]) < 100) tmp_r <- tmp_r - 4
			   
			#if (!is.na(S_PC[T]))#Platelets
               #if (S_PC[T] >= 100 & S_PC[T] < 150) tmp_r <- tmp_r - 1
			   #else if (S_PC[T] >= 50 & S_PC[T] < 100) tmp_r <- tmp_r - 2
			   #else if (S_PC[T] >= 20 & S_PC[T] < 50) tmp_r <- tmp_r - 3
			   #else if (S_PC[T] < 20) tmp_r <- tmp_r - 4
			   
			#if (!is.na(S_TBI[T]))#Bilirubin(mg/dl)
               #if (S_TBI[T] >= 1.2 & S_TBI[T] < 2.0 ) tmp_r <- tmp_r - 1
			   #else if (S_TBI[T] >= 2.0 & S_TBI[T] < 6.0 ) tmp_r <- tmp_r - 2
			   #else if (S_TBI[T] >= 6.0 & S_TBI[T] < 12.0 ) tmp_r <- tmp_r - 3
			   #else if (S_TBI[T] >= 12.0 ) tmp_r <- tmp_r - 4
			   
			#if (!is.na(S_Cr[T]))#Renal function #Creatinine(mg/dl)
               #if (S_Cr[T] >= 1.2 & S_Cr[T] < 2.0 ) tmp_r <- tmp_r - 1
			   #else if (S_Cr[T] >= 2.0 & S_Cr[T] < 3.5 ) tmp_r <- tmp_r - 2
			   #else if (S_Cr[T] >= 3.5 & S_Cr[T] < 5.0 ) tmp_r <- tmp_r - 3
			   #else if (S_Cr[T] >= 5.0 ) tmp_r <- tmp_r - 4
			   
			   
			#if (!is.na(S_GCS[T]) & !is.na(S_GCS[T-1]))#GCS improves or deteriorates 
               #if (S_GCS[T] == S_GCS[T-1]) tmp_r <- tmp_r + 0
			   #else if (S_GCS[T] > S_GCS[T-1]) tmp_r <- tmp_r + (abs(S_GCS[T]-S_GCS[T-1])*2)
			   #else if (S_GCS[T] < S_GCS[T-1]) tmp_r <- tmp_r - (abs(S_GCS[T]-S_GCS[T-1])*2)
			   
			#tmp_r <- 0
			#survive
			if (is.na(deathtime) | myT < deathtime){
				if(myT + 3600 >= endSec){ 
					print(paste("+",'i = ', myPatients[i],'myT = ', myT,'deathtime = ',deathtime))
					tmp_r <- tmp_r + 15
				}
				else tmp_r <- tmp_r + 0 #otherwise
				
			}
			if( !is.na(deathtime) & myT + 3600 >= deathtime){ #die
				if(myT + 3600 >= endSec){ 
					print(paste("-",l,'i = ', myPatients[i],'myT = ', myT,'deathtime = ',deathtime))
					tmp_r <- tmp_r - 15
				}
			}

			
            R <- c(R, tmp_r)
			H <- c(H, h)

             
			myT <- myT + 3600
        }#end while #hour
		h <- h + 1
        S_HR  <- c(S_HR,NA) #Heart Rate
		S_RR  <- c(S_RR,NA) #Respiratory Rate
		S_BPs <- c(S_BPs,NA) #Blood Pressure Systolic
		S_BPd <- c(S_BPd,NA) #Blood Pressure Diastolic   
		S_BPm <- c(S_BPm,NA) #Blood Pressure Mean  
		S_T   <- c(S_T,NA) #Temperature (F)
		S_W   <- c(S_W,NA) #Weight (KG)
		S_PaCO2 <- c(S_PaCO2,NA) #Arterial PaCO2
		S_PaO2  <- c(S_PaO2,NA) #Arterial PaO2
		S_FiO2  <- c(S_FiO2,NA) #FiO2 (Inspired O2 Fraction)
		S_SpO2  <- c(S_SpO2,NA) #SpO2
		S_GCS <- c(S_GCS,NA) 

		S_WBC  <- c(S_WBC,NA) #White Blood Cell
		S_TBI  <- c(S_TBI,NA) #Bilirubin total (TBI)
		S_PC   <- c(S_PC,NA) #Platelet Count
		S_ALB  <- c(S_ALB,NA) #Albumin
		S_PH   <- c(S_PH,NA) #Arterial pH
		S_Ca   <- c(S_Ca,NA) #Calcium
		S_AC   <- c(S_AC,NA) #Glucose
		S_Hb   <- c(S_Hb,NA) #Hemoglobin
		S_Mg   <- c(S_Mg,NA) #Magnesium
		S_PTT  <- c(S_PTT,NA) #Partial Thromboplastin Time
		S_K    <- c(S_K,NA) #Potassium
		S_BUN  <- c(S_BUN,NA) #Blood Urea Nitrogen
		S_Cl   <- c(S_Cl,NA) #Chloride
		S_INR  <- c(S_INR,NA) #International Normalized Ratio
		S_Na   <- c(S_Na,NA) #Sodium
		S_TCO2 <- c(S_TCO2,NA) #TCO2
		S_Cr   <- c(S_Cr,NA) #Creatinine
		S_PT   <- c(S_PT,NA) #Prothrombin time

		A_IV  <- c(A_IV,NA)
		A_VP  <- c(A_VP,NA)
		R     <- c(R,   NA)
		H     <- c(H,   NA)
		
		S_age <- c(S_age, NA)
		
		print(l)

     }#end for       # icu
    }#end if        # if icu
}#end for            # myPatients


summary(S_HR)
summary(S_RR)
summary(S_BPs)
summary(S_BPd)
summary(S_BPm)
summary(S_T)
summary(S_W)
summary(S_PaCO2)
summary(S_PaO2)
summary(S_FiO2)
summary(S_SpO2)
summary(S_GCS)

summary(S_WBC)
summary(S_TBI)
summary(S_PC)
summary(S_ALB)
summary(S_PH)
summary(S_Ca)
summary(S_AC)
summary(S_Hb)
summary(S_Mg)
summary(S_PTT)
summary(S_K)
summary(S_BUN)
summary(S_Cl)
summary(S_INR)
summary(S_Na)
summary(S_TCO2)
summary(S_Cr)
summary(S_PT)


windows()
plot(patient_vt,rep(1,length(patient_vt)))
points(patient_dt,rep(1.1,length(patient_dt)),col=2)
points(patient_pt,rep(1.2,length(patient_pt)),col=3)


# get 25 action classes (0-24)
actionsN <- 25

A <- NULL
for (i in 1:length(A_IV)) {
    if (!is.na(A_IV[i])) {
       if (A_IV[i] == 0) {
          if (is.na(A_VP[i])) A <- c(A, 0)
          else if (A_VP[i] == 0) A <- c(A, 0)
          else if (A_VP[i] > 0 & A_VP[i] < 100/4) A <- c(A, 1)
          else if (A_VP[i] >= 100/4 & A_VP[i] < (100/4)*2) A <- c(A, 2)
          else if (A_VP[i] >= (100/4)*2 & A_VP[i] < (100/4)*3) A <- c(A, 3)
		  else if (A_VP[i] >= (100/4)*3 & A_VP[i] <= 100) A <- c(A, 4)
          else print(A_VP[i])
       } else if (A_IV[i] > 0 & A_IV[i] < 100/4){
		  if (is.na(A_VP[i])) A <- c(A, 5)
          else if (A_VP[i] == 0) A <- c(A, 5)
          else if (A_VP[i] > 0 & A_VP[i] < 100/4) A <- c(A, 6)
          else if (A_VP[i] >= 100/4 & A_VP[i] < (100/4)*2) A <- c(A, 7)
          else if (A_VP[i] >= (100/4)*2 & A_VP[i] < (100/4)*3) A <- c(A, 8)
		  else if (A_VP[i] >= (100/4)*3 & A_VP[i] <= 100) A <- c(A, 9)
          else print(A_VP[i])
       }else if (A_IV[i] >= 100/4 & A_IV[i] < (100/4)*2){
		  if (is.na(A_VP[i])) A <- c(A, 10)
          else if (A_VP[i] == 0) A <- c(A, 10)
          else if (A_VP[i] > 0 & A_VP[i] < 100/4) A <- c(A, 11)
          else if (A_VP[i] >= 100/4 & A_VP[i] < (100/4)*2) A <- c(A, 12)
          else if (A_VP[i] >= (100/4)*2 & A_VP[i] < (100/4)*3) A <- c(A, 13)
		  else if (A_VP[i] >= (100/4)*3 & A_VP[i] <= 100) A <- c(A, 14)
          else print(A_VP[i])
	   }else if (A_IV[i] >= (100/4)*2 & A_IV[i] < (100/4)*3){
		  if (is.na(A_VP[i])) A <- c(A, 15)
          else if (A_VP[i] == 0) A <- c(A, 15)
          else if (A_VP[i] > 0 & A_VP[i] < 100/4) A <- c(A, 16)
          else if (A_VP[i] >= 100/4 & A_VP[i] < (100/4)*2) A <- c(A, 17)
          else if (A_VP[i] >= (100/4)*2 & A_VP[i] < (100/4)*3) A <- c(A, 18)
		  else if (A_VP[i] >= (100/4)*3 & A_VP[i] <= 100) A <- c(A, 19)
          else print(A_VP[i])
	   }else if (A_IV[i] >= (100/4)*3 & A_IV[i] <= 100){
		  if (is.na(A_VP[i])) A <- c(A, 20)
          else if (A_VP[i] == 0) A <- c(A, 20)
          else if (A_VP[i] > 0 & A_VP[i] < 100/4) A <- c(A, 21)
          else if (A_VP[i] >= 100/4 & A_VP[i] < (100/4)*2) A <- c(A, 22)
          else if (A_VP[i] >= (100/4)*2 & A_VP[i] < (100/4)*3) A <- c(A, 23)
		  else if (A_VP[i] >= (100/4)*3 & A_VP[i] <= 100) A <- c(A, 24)
          else print(A_VP[i])
	   
	   }
    } else A <- c(A, NA)
}