data <- read.csv("425_wc_patient_miRNA(no_normalizing).csv",h=T)

setwd("D:/DL/New data/Processed data/425 patient data/With clinical data/Combined/Normalized")
data2 <- read.csv("425_wc_train_combined(normalized).csv",h=T)
data3 <- read.csv("425_wc_validation_combined(normalized).csv",h=T)
data4 <- read.csv("425_wc_test_combined(normalized).csv",h=T)

grep("hsa.mir.99a",colnames(data4))
colnames(data)[938]
grep("hsa.let.7a.1",colnames(data4))
937-9+1
ncol(data)

#extract
mir_train_normalized<-data2[,1:937]
mir_validation_normalized<-data3[,1:937]
mir_test_normalized<-data4[,1:937]

setwd("D:/DL/Variational autoencoder/Tryout_12_22_2020")
write.csv(mir_train_normalized, "mir_train_normalized.csv", row.names = F)
write.csv(mir_validation_normalized, "mir_validation_normalized.csv", row.names = F)
write.csv(mir_test_normalized, "mir_test_normalized.csv", row.names = F)

#in case program closed mid-way
mir_train_normalized<-read.csv("mir_train_normalized.csv", h = T)
mir_validation_normalized<-read.csv("mir_validation_normalized.csv", h = T)
mir_test_normalized<-read.csv("mir_test_normalized.csv", h = T)

setwd("D:/DL/New data/Processed data/425 patient data/With clinical data/Clinical data")
data_clinical<-read.csv("425_wc_patient_clinic(no_normalizing).csv",h=T)

tr_match<-mir_train_normalized$Patient_ID
tr_id<-which(data_clinical$Patient_ID %in% tr_match, arr.ind = T)
length(tr_id)
clinical_tr<-data_clinical[tr_id,]
all.equal(clinical_tr$Patient_ID, tr_match)
clinical_tr<-clinical_tr[order(-clinical_tr$OS.time),]
rownames(clinical_tr)<-seq(1:nrow(clinical_tr))

val_match<-mir_validation_normalized$Patient_ID
val_id<-which(data_clinical$Patient_ID %in% val_match, arr.ind = T)
length(val_id)
clinical_val<-data_clinical[val_id,]
all.equal(clinical_val$Patient_ID, val_match)
clinical_val<-clinical_val[order(-clinical_val$OS.time),]
rownames(clinical_val)<-seq(1:nrow(clinical_val))

tes_match<-mir_test_normalized$Patient_ID
tes_id<-which(data_clinical$Patient_ID %in% tes_match, arr.ind = T)
length(tes_id)
clinical_tes<-data_clinical[tes_id,]
all.equal(clinical_tes$Patient_ID, tes_match)
clinical_tes<-clinical_tes[order(-clinical_tes$OS.time),]
rownames(clinical_tes)<-seq(1:nrow(clinical_tes))

setwd("D:/DL/Variational autoencoder/Tryout_12_22_2020")
write.csv(clinical_tr, "clinical_tr.csv", row.names = F)
write.csv(clinical_val, "clinical_val.csv", row.names = F)
write.csv(clinical_tes, "clinical_tes.csv", row.names = F)

#survival analysis
setwd("D:/DL/Variational autoencoder/Tryout_12_22_2020/latent_variables")
tr_z<-read.csv("tr_z.csv", h = F)
tes_z<-read.csv("tes_z.csv", h = F)
colnames(tr_z)<-paste("Z", seq(1:10), sep = "_")
colnames(tes_z)<-paste("Z", seq(1:10), sep = "_")
comb_tr<-cbind(clinical_tr, tr_z)
comb_tes<-cbind(clinical_tes, tes_z)

library(survival)
formula_dep<-paste(colnames(clinical_tr)[2:3], collapse = " + ")
formula1<-paste(colnames(clinical_tr)[-(1:3)], collapse = " + ")
all.equal(colnames(comb_tes),colnames(comb_tr))
formula2<-paste(colnames(comb_tr)[-(1:3)], collapse = " + ")
formula_dep
formula1
formula2

cox1<-coxph(Surv(OS.time, OS) ~ race_black + race_white + age + stageh + gradeh,
            data = clinical_tr)
summary(cox1)

cox1_2<-coxph(Surv(OS.time, OS) ~ race_black + race_white + age + stageh + gradeh,
            data = clinical_tes)
summary(cox1_2)

cox2<-coxph(Surv(OS.time, OS) ~ race_black + race_white + age + stageh + gradeh + Z_1 + Z_2 + Z_3 + Z_4 + Z_5 + Z_6 + Z_7 + Z_8 + Z_9 + Z_10,
            data = comb_tr)
summary(cox2)

cox2_2<-coxph(Surv(OS.time, OS) ~ race_black + race_white + age + stageh + gradeh + Z_1 + Z_2 + Z_3 + Z_4 + Z_5 + Z_6 + Z_7 + Z_8 + Z_9 + Z_10,
            data = comb_tes)
summary(cox2_2)

all.equal(colnames(clinical_tr),colnames(clinical_tes))
clinical_comb<-rbind(clinical_tr, clinical_tes)
comb_data<-rbind(comb_tr, comb_tes)

cox3<-coxph(Surv(OS.time, OS) ~ race_black + race_white + age + stageh + gradeh,
            data = clinical_comb)
summary(cox3)

cox4<-coxph(Surv(OS.time, OS) ~ race_black + race_white + age + stageh + gradeh + Z_1 + Z_2 + Z_3 + Z_4 + Z_5 + Z_6 + Z_7 + Z_8 + Z_9 + Z_10,
            data = comb_data)
summary(cox4)

#test functions
x<-c(2,2)
y<-c(1,2,3,4)
which(y %in% x,arr.ind = T)

##divide
setwd("D:/DL/New data/Processed data/425 patient data/With clinical data/Combined/Not normalized")
data_raw<-read.csv("425_wc_patient_combined(no_normalizing).csv", h = T)
grep("hsa.mir.99a",colnames(data_raw))
colnames(data_raw)[938]
grep("hsa.let.7a.1",colnames(data_raw))
937-9+1

data_raw<-data_raw[,1:937]
ncol(data_raw)

#getting_raw_data
setwd("D:/DL/Variational autoencoder/Tryout_12_30_2020")
write.csv(data_raw, "425_patients_data_raw.csv", row.names = F)

data_raw<-read.csv("425_patients_data_raw.csv", h = T)

#dividing modules
div<-rmultinom(nrow(data_raw),1,c(0.64,0.16,0.2))
classify<-c(1,2,3)
tvt<-t(div)%*%classify
tvt<-as.numeric(tvt)
tvt
ind_tr<-which(tvt==1,arr.ind = T)
ind_va<-which(tvt==2,arr.ind = T)
ind_te<-which(tvt==3,arr.ind = T)

data_tr<-data_raw[ind_tr, ]
data_val<-data_raw[ind_va, ]
data_tes<-data_raw[ind_te, ]

mean_count<-c()
sd_count<-c()
epsilon<-0.000001
for(i in 9:ncol(data_tr)){
  temp_mean<-mean(data_tr[,i])
  temp_sd<-sd(data_tr[,i])
  data_tr[,i]<-(data_tr[,i]-temp_mean)/(temp_sd+epsilon)
  mean_count[i-8]<-temp_mean
  sd_count[i-8]<-temp_sd
}

Normalizing_tv<-function(vmean,vsd,index,epsilon,data){
  for (i in index:ncol(data)) {
    data[,i]<-(data[,i]-vmean[i-index+1])/(vsd[i-index+1]+epsilon)
  }
  return(data)
}

data_val<-Normalizing_tv(mean_count, sd_count, 9, 0.000001, data_val)
data_tes<-Normalizing_tv(mean_count, sd_count, 9, 0.000001, data_tes)

#dividing data

dividing_parts<-function(data, p){
  
  div<-rmultinom(nrow(data), 1, p)
  classify<-c(1,2,3)
  tvt<-t(div)%*%classify
  tvt<-as.numeric(tvt)
  ind_tr<-which(tvt==1,arr.ind = T)
  ind_val<-which(tvt==2,arr.ind = T)
  ind_tes<-which(tvt==3,arr.ind = T)
  data_tr<-data[ind_tr, ]
  data_val<-data[ind_val, ]
  data_tes<-data[ind_tes, ]
  divided_data<-list(data_tr, data_val, data_tes)
  names(divided_data)<-c("training", "validation", "testing")
  
  return(divided_data)
}

#divided_data<-dividing_parts(data_raw, c(0.64, 0.16, 0.2))
#data_val_2<-divided_data[["validation"]]

Normalizing_train<-function(data_tr, index, epsilon){
  
  mean_count<-c()
  sd_count<-c()
  for(i in index:ncol(data_tr)){
    temp_mean<-mean(data_tr[,i])
    temp_sd<-sd(data_tr[,i])
    data_tr[,i]<-(data_tr[,i]-temp_mean)/(temp_sd+epsilon)
    mean_count[i-index+1]<-temp_mean
    sd_count[i-index+1]<-temp_sd
  }
  processed_train<-list(data_tr, mean_count, sd_count)
  names(processed_train)<-c("dtraining", "vmean", "vsd")
  
  return(processed_train)
}

#data_tr_2<-divided_data[["training"]]
#processed_train<-Normalizing_train(data_tr_2, 9, 0.000001)
#data_tr_2<-processed_train[["dtraining"]]
#mean(data_tr_2[,100])
#sd(data_tr_2[,103])

Normalizing_tv<-function(vmean,vsd,index,epsilon,data){
  
  for (i in index:ncol(data)) {
    data[,i]<-(data[,i]-vmean[i-index+1])/(vsd[i-index+1]+epsilon)
  }
  
  return(data)
}

data_dividing<-function(data, p, index, epsilon){
  
  divided_data<-dividing_parts(data, p)
  data_tr<-divided_data[["training"]]
  data_val<-divided_data[["validation"]]
  data_tes<-divided_data[["testing"]]
  
  processed_train<-Normalizing_train(data_tr, index, epsilon)
  data_tr<-processed_train[["dtraining"]]
  mean_count<-processed_train[["vmean"]]
  sd_count<-processed_train[["vsd"]]
  
  data_val<-Normalizing_tv(mean_count, sd_count, index, epsilon, data_val)
  data_tes<-Normalizing_tv(mean_count, sd_count, index, epsilon, data_tes)
  
  processed_data<-list(data_tr, data_val, data_tes)
  names(processed_data)<-c("dtraining", "dvalidation", "dtesting")
  
  return(processed_data)
}

processed_data<-data_dividing(data_raw, c(0.64, 0.16, 0.20), 9, 0.000001)
data_tr<-processed_data[["dtraining"]]
data_val<-processed_data[["dvalidation"]]
data_tes<-processed_data[["dtesting"]]

