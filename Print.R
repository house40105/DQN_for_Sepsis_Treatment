setwd("figure")


width_size<-1200
height_size<-600

ps_size=15
ps_angle=30

#windows()
i <- vitals$itemid %in% id_heart_rate
pl <- vitals[i,]#
ps1<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Heart Rate')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))




#windows()
i <- vitals$itemid %in% id_respiratory_rate
pl <- vitals[i,]#
ps2<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Respiratory Rate')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_blood_pressure_systolic
pl <- vitals[i,]#
ps3<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Systolic Blood Pressure')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_blood_pressure_diastolic
pl <- vitals[i,]#
ps4<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Diastolic Blood Pressure')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_blood_pressure_mean
pl <- vitals[i,]#
ps5<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Mean Blood Pressure')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_body_temperature
pl <- vitals[i,]#
ps6<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Body Temperature')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_weight
pl <- vitals[i,]#
ps7<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Weight')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_PaCO2
pl <- vitals[i,]#
ps8<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'PaCO2')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_PaO2
pl <- vitals[i,]#
ps9<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'PaO2')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_FiO2
pl <- vitals[i,]#
ps10<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'FiO2')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_spo2
pl <- vitals[i,]#
ps11<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'SpO2')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_WBC
pl <- vitals[i,]#
ps12<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'White Blood Cell')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_Bilirubin
pl <- vitals[i,]#
ps13<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Bilirubin total (TBI)')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_Platelet
pl <- vitals[i,]#
ps14<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Platelet Count')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_Albumin
pl <- vitals[i,]#
ps15<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Albumin')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_ArterialPH
pl <- vitals[i,]#
ps16<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Arterial pH')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_Calcium
pl <- vitals[i,]#
ps17<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Calcium')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_Glucose
pl <- vitals[i,]#
ps18<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Glucose')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_Hemoglobin
pl <- vitals[i,]#
ps19<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Hemoglobin')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_Magnesium
pl <- vitals[i,]#
ps20<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Magnesium')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_PTT
pl <- vitals[i,]#
ps21<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Partial Thromboplastin Time (PTT)')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_Potassium
pl <- vitals[i,]#
ps22<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Potassium')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_BUN
pl <- vitals[i,]#
ps23<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Blood Urea Nitrogen (BUN)')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_Chloride
pl <- vitals[i,]#
ps24<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Chloride')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_INR
pl <- vitals[i,]#
ps25<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'International Normalized Ratio (INR)')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))

#windows()
i <- vitals$itemid %in% id_Sodium
pl <- vitals[i,]#
ps26<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Sodium')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_TCO2
pl <- vitals[i,]#
ps27<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'TCO2')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_Creatinine
pl <- vitals[i,]#
ps28<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Creatinine')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_PT
pl <- vitals[i,]#
ps29<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Prothrombin time (PT)')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=ps_angle, hjust=1, vjust=.5))


#windows()
i <- vitals$itemid %in% id_GCS
pl <- vitals[i,]#
ps30<-qplot(x=as.character(subject_id),y=valuenum,data=pl,geom="boxplot",color=as.character(subject_id))+
labs(x = 'Patients',y = 'Valuenum',title = 'Glasgow Coma Scale (GCS)')+
theme(legend.position="none",plot.title = element_text(hjust = 0.5,size=20))+
theme(axis.title.x=element_text(size=ps_size),axis.title.y=element_text(size=ps_size),axis.text.x = element_text(angle=45, hjust=1, vjust=.5))



ggarrange(ps1,ps2,ps3,ps4,ncol=1,nrow=4)

ggarrange(ps5,ps6,ps7,ps8,ps9,ncol=1,nrow=5)
ggarrange(ps10,ps11,ps12,ps13,ps14,ncol=1,nrow=5)
ggarrange(ps15,ps16,ps17,ps18,ps19,ncol=1,nrow=5)
ggarrange(ps20,ps21,ps22,ps23,ps24,ncol=1,nrow=5)
ggarrange(ps25,ps26,ps27,ps28,ps29,ncol=1,nrow=5)
ggarrange(ps30,ncol=1,nrow=1)




tiff("4 vasopressor dose.tiff", width=1000, height=1000)
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
#par(mfrow=c(2,2))
#hist(tmp,breaks=50,xlim = c(0,800),border = "black",main="Dopamine",xlab = "Dose",cex.main=3,cex.lab=1.8,cex.sub = 8)
tmp<-data.frame(tmp)
for(i in 1:length(tmp)) tmp[i]<-tmp[i]+10
p1<-ggplot(data = tmp,mapping = aes(x = tmp))+ geom_histogram()+
scale_x_continuous(name = "Dose",limits = c(0,850),breaks = seq(from = 0, to = 850, by = 150))+
scale_y_continuous(limits = c(0,35))+
labs(title="Dopamine")+ theme(plot.title=element_text(size=25,hjust = 0.5))+
theme(axis.text=element_text(size=25),axis.title.x=element_text(size=25),axis.title.y=element_text(size=25),legend.text = element_text(size = 20),legend.title = element_text(size = 20))

tmp <- NULL
for (i in 1:length(myPatients)) {
    j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]),grep("dobutamine",drugs$drug,ignore.case=TRUE))
    if (length(j) > 0) tmp <- c(tmp,as.numeric(as.character(drugs$dose_val_rx[j])))
    #print(i)
}
#hist(tmp,breaks=50,xlim = c(0,800),border = "black",main="Dobutamine",xlab = "Dose",cex.main=3,cex.lab=1.8,cex.sub = 3)
tmp<-data.frame(tmp)
for(i in 1:length(tmp)) tmp[i]<-tmp[i]+10
p2<-ggplot(data = tmp,mapping = aes(x = tmp))+ geom_histogram()+
scale_x_continuous(name = "Dose",limits = c(0,850),breaks = seq(from = 0, to = 850, by = 150))+
scale_y_continuous(limits = c(0,35))+
labs(title="Dobutamine")+ theme(plot.title=element_text(size=25,hjust = 0.5))+
theme(axis.text=element_text(size=25),axis.title.x=element_text(size=25),axis.title.y=element_text(size=25),legend.text = element_text(size = 20),legend.title = element_text(size = 20))


tmp <- NULL
for (i in 1:length(myPatients)) {
    j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]),grep("epinephrine",drugs$drug,ignore.case=TRUE))
    if (length(j) > 0) tmp <- c(tmp,as.numeric(as.character(drugs$dose_val_rx[j])))
    #print(i)
}
#hist(tmp,breaks=50,xlim = c(0,800),border = "black",main="Epinephrine",xlab = "Dose",cex.main=3,cex.lab=1.8,cex.sub = 3)
tmp<-data.frame(tmp)
for(i in 1:length(tmp)) tmp[i]<-tmp[i]+10
p3<-ggplot(data = tmp,mapping = aes(x = tmp))+ geom_histogram()+
scale_x_continuous(name = "Dose",limits = c(0,850),breaks = seq(from = 0, to = 850, by = 150))+
scale_y_continuous(limits = c(0,35))+
labs(title="Epinephrine")+ theme(plot.title=element_text(size=25,hjust = 0.5))+
theme(axis.text=element_text(size=25),axis.title.x=element_text(size=25),axis.title.y=element_text(size=25),legend.text = element_text(size = 20),legend.title = element_text(size = 20))


tmp <- NULL
for (i in 1:length(myPatients)) {
    j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]),grep("norepinephrine",drugs$drug,ignore.case=TRUE))
    if (length(j) > 0) tmp <- c(tmp,as.numeric(as.character(drugs$dose_val_rx[j])))
    #print(i)
}
#hist(tmp,breaks=50,xlim = c(0,800),border = "black",main="Norepinephrine",xlab = "Dose",cex.main=3,cex.lab=1.8,cex.sub = 3)
tmp<-data.frame(tmp)
for(i in 1:length(tmp)) tmp[i]<-tmp[i]+10
p4<-ggplot(data = tmp,mapping = aes(x = tmp))+ geom_histogram()+
scale_x_continuous(name = "Dose",limits = c(0,850),breaks = seq(from = 0, to = 850, by = 150))+
scale_y_continuous(limits = c(0,35))+
labs(title="Norepinephrine")+ theme(plot.title=element_text(size=25,hjust = 0.5))+
theme(axis.text=element_text(size=25),axis.title.x=element_text(size=25),axis.title.y=element_text(size=25),legend.text = element_text(size = 20),legend.title = element_text(size = 20))

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,labels=c("A","B","C","D"),font.label = list(size = 25))


dev.off()




#IV
tmp_iv <- NULL
for (i in 1:length(myPatients)) {
	j <- intersect(which(paste(drugs[,2],drugs[,3]) == myPatients[i]), grep("IV", drugs$route))#ALL Intravenous therapy (IV, IV DRIP, IV BOLUS, IV PCA )
	if (length(j) > 0) tmp_iv <- c(tmp_iv,as.numeric(as.character(drugs$dose_val_rx[j])))#
}
hist(tmp_iv,breaks=250,xlim = c(0,6000),border = "black",main="IV",xlab = "Dose")

tiff("All IV dose.tiff", width=1000, height=1000)
df_iv<- data.frame(Dose=tmp_iv)
p5<-ggplot(data=df_iv)+geom_histogram(mapping=aes(x=Dose),bins = 250)+
#scale_x_continuous(name = "Dose",limits = c(0:7000,24000:25000))+
labs(title="Intravenous fluids (IV) ")+ theme(plot.title=element_text(size=25,hjust = 0.5))+
theme(axis.text=element_text(size=25),axis.title.x=element_text(size=25),axis.title.y=element_text(size=25),legend.text = element_text(size = 20),legend.title = element_text(size = 20))
dev.off()

#T.tiff IV and VP plot
ggarrange(p5,ggarrange(p1,p2,p3,p4,ncol=2,nrow=2,labels=c("B","C","D","E"),font.label = list(size = 25), align = "v"),nrow=2,labels="A",font.label = list(size = 25))






#R #A
dim(tmp)
sum(is.na(R))
windows()
plot(A)
windows()
plot(R)


#count Patients' gender
p_sid <-unique(diagnoses_icd[diagnoses_icd[,5] %in% myICDs,2])
p_sid <- p_sid[p_sid[]!="10120"]
p_gender <- static$gender[static$subject_id %in% p_sid]
df_gender <- data.frame(Gender=c("Male", "Female"),count=c(sum(p_gender=="M"),sum(p_gender=="F")))
df_gender$value2 <- df_gender$count * 100 / sum(df_gender$count) 

tiff("gender.tiff")
ggplot(data=df_gender, aes(x = "", y = count, fill = Gender)) +
geom_bar(aes(x=factor(1),y=count,fill=Gender),stat = "identity") +
coord_polar("y", start=0)+
theme_void()+
theme(legend.text=element_text(,size=15))+
geom_text(size=7,aes(y = count/2 + c(0, cumsum(count)[-length(count)]), x = sum(count)/30, label = paste0(value2,"%","(",count,")")))+
scale_fill_manual(values = c("#F89F99", "#9AB1FF"))  
dev.off()



#count survivor
p_survivor<-data.frame(Subject=NA,Survivor=NA)
p_age<-data.frame(Subject=NA,Hadm=NA,Icustay=NA,Age=NA)
p_hour<-data.frame(Subject=NA,Hadm=NA,Icustay=NA,Hours=NA)
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
		
		p_hour<-rbind(p_hour,c(patient_a[i,2],patient_a[i,3],patient_a[i,4],(endSec-startSec)/60/60))

        myT <- startSec
        while (myT < endSec) {
			# every hour
			k <- patient_vt >= myT & patient_vt < (myT + 3600)%/% (365*24*60*60)
			p_age<-rbind(p_age,c(patient_a[i,2],patient_a[i,3],patient_a[i,4],((myT + 1800 - dob) %/% (365*24*60*60))))
			   
			#tmp_r <- 0
			#survive
			if (is.na(deathtime) | myT < deathtime){
				if(myT + 3600 >= endSec){ 
					#print(paste("+",'i = ', myPatients[i],'myT = ', myT,'deathtime = ',deathtime))
					p_survivor<-rbind(p_survivor,c(patient_a[i,2],deathtime))
				}
				
				
			}
			else if(myT >= deathtime){ #die
				#print(paste("-",'i = ', myPatients[i],'myT = ', myT,'deathtime = ',deathtime))
				p_survivor<-rbind(p_survivor,c(patient_a[i,2],deathtime))
			}  
			myT <- myT + 3600
        }#end while #hour
     }#end for       # icu
    }#end if        # if icu
}#end for            # myPatients
p_survivor<-unique(p_survivor)
p_survivor<- p_survivor[!is.na(p_survivor[,1]),]
df_survivor <- data.frame(Survivor=c("Survivors", "Non-survivors"),count=c(sum(is.na(p_survivor[,2])),sum(!is.na(p_survivor[,2]))))
df_survivor$value2 <- df_survivor$count * 100 / sum(df_survivor$count) 

tiff("Survivors.tiff")
ggplot(data=df_survivor, aes(x = "", y = count, fill = Survivor)) +
geom_bar(aes(x=factor(1),y=count,fill=Survivor),stat = "identity") +
coord_polar("y", start=0)+
theme_void()+
theme(legend.text=element_text(,size=15))+
geom_text(size=7,aes(y = count/2 + c(0, cumsum(count)[-length(count)]), x = sum(count)/30, label = paste0(value2,"%","(",count,")")))+
scale_fill_manual(values = c("#D588FF", "#9FD269"))  #9FD269 green
dev.off()

#hours in icu
p_hour<- p_hour[!is.na(p_hour[,1]),]
demographic<-merge(p_hour,p_survivor,by="Subject",all=T)
for(i in 1:nrow(demographic)) if(is.na(demographic[i,5]))demographic[i,5]<-"Survivor" else demographic[i,5]<-"Non"

#age
p_age<-unique(p_age)
p_age<- p_age[!is.na(p_age[,1]),]

static$gender[static$subject_id %in% demographic[grep("Survivor",demographic$Survivor),]$Subject]
static$gender[static$subject_id %in% demographic$Subject]

mean(demographic[-grep("Survivor",demographic$Survivor),]$Hours)
grep(demographic[grep("Survivor",demographic$Survivor),]$Subject,p_age$Subject)
demographic[grep("Survivor",demographic$Survivor),]$Subject