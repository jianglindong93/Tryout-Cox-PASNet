#getting_raw_data
setwd("D:/DL/Variational autoencoder/Tryout_12_30_2020")
write.csv(data_raw, "425_patients_data_raw.csv", row.names = F)

data_raw<-read.csv("425_patients_data_raw.csv", h = T)

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

setwd("D:/DL/Variational autoencoder/Tryout_12_30_2020/divided_data")
file_names<-paste("exp", seq(1:20), sep = "_")
for(folder in file.names){
  dir.create(folder)
}
#file_names<-list.files()
for(folder in file_names){
  file_direction<-paste("D:/DL/Variational autoencoder/Tryout_12_30_2020/divided_data", folder, sep = "/")
  setwd(file_direction)
  count_num<-unlist(strsplit(folder, split = "_"))[2]
  processed_data<-data_dividing(data_raw, c(0.64, 0.16, 0.20), 9, 0.000001)
  data_tr<-processed_data[["dtraining"]]
  data_val<-processed_data[["dvalidation"]]
  data_tes<-processed_data[["dtesting"]]
  tr_name<-paste(paste(c("data_tr", count_num), collapse = "_"), ".csv", sep = "")
  val_name<-paste(paste(c("data_val", count_num), collapse = "_"), ".csv", sep = "")
  tes_name<-paste(paste(c("data_tes", count_num), collapse = "_"), ".csv", sep = "")
  write.csv(data_tr, tr_name, row.names = F)
  write.csv(data_val, val_name, row.names = F)
  write.csv(data_tes, tes_name, row.names = F)
}
for(folder in file_names){
  file_direction<-paste("D:/DL/Variational autoencoder/Tryout_12_30_2020/divided_data", folder, sep = "/")
  setwd(file_direction)
  count_num<-unlist(strsplit(folder, split = "_"))[2]
  latent_folder<-paste("latent_features", count_num, sep = "_")
  dir.create(latent_folder)
}

paste(paste(paste("'Z", seq(1:10), sep = "_"), collapse = "', "), "'", sep = "")

beta_folder<-paste("beta", c(5, 25, 125, 500, 1000), sep = "_")
for(folder in file_names){
  file_direction<-paste("D:/DL/Variational autoencoder/Tryout_12_30_2020/divided_data", folder, sep = "/")
  count_num<-unlist(strsplit(folder, split = "_"))[2]
  file_direction_beta<-paste(file_direction, paste("latent_features", count_num, sep = "_"), sep = "/")
  setwd(file_direction_beta)
  for(beta_count in beta_folder){
    dir.create(beta_count)
  }
}

#summary statistics
m_1<-c(0.65, 0.65, 0.66, 0.65, 0.64, 0.63, 0.66, 0.63, 0.62, 0.61, 0.61, 0.66, 0.64, 0.63, 0.63, 0.62, 0.66, 0.63, 0.64, 0.68)
length(m_1)
mean(m_1)
sd(m_1)
m_1<-c(0.64, 0.68, 0.71, 0.72, 0.69, 0.69, 0.72, 0.72, 0.71, 0.67)
length(m_1)
mean(m_1)
sd(m_1)

m_5<-c(0.67, 0.66, 0.67, 0.61, 0.62, 0.61, 0.65, 0.63, 0.64, 0.64, 0.62, 0.64, 0.65, 0.62, 0.61, 0.62, 0.67, 0.64, 0.65, 0.67)
mean(m_5)
sd(m_5)
m_5<-c(0.69, 0.67, 0.74, 0.72, 0.65, 0.71, 0.70, 0.69, 0.73, 0.69)
mean(m_5)
sd(m_5)

m_25<-c(0.65, 0.65, 0.66, 0.63, 0.62, 0.63, 0.67, 0.64, 0.64, 0.64, 0.61, 0.66, 0.66, 0.64, 0.62, 0.63, 0.65, 0.63, 0.65, 0.66)
mean(m_25)
sd(m_25)
m_25<-c(0.66, 0.74, 0.69, 0.66, 0.63, 0.67, 0.74, 0.77, 0.76, 0.64)
mean(m_25)

m_125<-c(0.66, 0.65, 0.67, 0.64, 0.63, 0.63, 0.66, 0.64, 0.65, 0.63, 0.61, 0.67, 0.64, 0.62, 0.62, 0.63, 0.67, 0.64, 0.65, 0.66)
mean(m_125)
sd(m_125)
m_125<-c(0.65, 0.71, 0.71, 0.68, 0.72, 0.67, 0.71, 0.73, 0.75)
mean(m_125)
sd(m_125)

m_500<-c(0.65, 0.65, 0.65, 0.63, 0.63, 0.64, 0.64, 0.63, 0.63, 0.62, 0.61, 0.67, 0.63, 0.63, 0.61, 0.62, 0.64, 0.62, 0.63, 0.67)
mean(m_500)
sd(m_500)
m_500<-c(0.60, 0.67, 0.71, 0.68, 0.65, 0.64, 0.70, 0.72, 0.73, 0.67)
mean(m_500)
sd(m_500)

m_1000<-c(0.66, 0.65, 0.67, 0.63, 0.63, 0.65, 0.66, 0.63, 0.62, 0.62, 0.63, 0.66, 0.64, 0.63, 0.61, 0.62, 0.67, 0.63, 0.67, 0.66)
mean(m_1000)
sd(m_1000)
m_1000<-c(0.65, 0.62, 0.73, 0.75, 0.71, 0.65, 0.64, 0.70, 0.68, 0.74, 0.76)
mean(m_1000)
sd(m_1000)


