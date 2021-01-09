setwd("D:/DL/New data/Processed data/425 patient data/With clinical data/Combined/Not normalized")
data_raw<-read.csv("425_wc_patient_combined(no_normalizing).csv", h = T)
grep("hsa.mir.99a",colnames(data_raw))
colnames(data_raw)[937]
grep("hsa.let.7a.1",colnames(data_raw))
colnames(data_raw)[7]

data_raw<-data_raw[, -4]

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

#test_divid<-dividing_parts(data_raw, c(0.64, 0.16, 0.2))
#sum(test_divid[["training"]][,"OS"])

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

Normalizing_tv<-function(vmean,vsd,index,epsilon,data){
  
  for (i in index:ncol(data)) {
    data[,i]<-(data[,i]-vmean[i-index+1])/(vsd[i-index+1]+epsilon)
  }
  
  return(data)
}

data_dividing<-function(data, p, index, epsilon){
  
  carry_on<-TRUE
  survey<-c("race_white", "stageh", "gradeh")
  try_count<-0
  while (carry_on) {
    probes<-0
    bad_split<-FALSE
    divided_data<-dividing_parts(data, p)
    for(section in names(divided_data)[c(1, 3)]){
      if(bad_split){
        break
      }else{
        for(blobs in survey){
          if(sum(divided_data[[section]][, blobs]) > (nrow(divided_data[[section]]) - 5)){
            bad_split<-TRUE
            break
          }else{
            probes<-probes + 1
          }
        }
      }
    }
    if(probes >= (length(names(divided_data)) - 1) * length(survey)){
      carry_on<-FALSE
    }
    try_count<-try_count + 1
    if(try_count >= 200 & carry_on == TRUE){
      print("Warning: reached 200 times maximum tryout limit, division still not desirable.")
      break
    }
  }
  
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

#test_tryout<-data_dividing(data_raw, c(0.64, 0.16, 0.2), 8, 0.000001)
#nrow(test_tryout[["dtesting"]])
#sum(test_tryout[["dtesting"]][, "race_white"])
#sum(test_tryout[["dtesting"]][, "stageh"])
#sum(test_tryout[["dtesting"]][, "gradeh"])

setwd("D:/DL/Variational autoencoder/Tryout_01_07_2021/divided_data")
file_names<-paste("exp", seq(1:10), sep = "_")
for(folder in file_names){
  dir.create(folder)
}
#file_names<-list.files()
for(folder in file_names){
  file_direction<-paste("D:/DL/Variational autoencoder/Tryout_01_07_2021/divided_data", folder, sep = "/")
  setwd(file_direction)
  count_num<-unlist(strsplit(folder, split = "_"))[2]
  processed_data<-data_dividing(data_raw, c(0.64, 0.16, 0.2), 8, 0.000001)
  
  data_tr<-processed_data[["dtraining"]]
  data_tr_mirna<-processed_data[["dtraining"]][, 1:936]
  data_tr_gene<-processed_data[["dtraining"]][, c(1:7, 937:ncol(processed_data[["dtraining"]]))]
  
  data_val<-processed_data[["dvalidation"]]
  data_val_mirna<-processed_data[["dvalidation"]][, 1:936]
  data_val_gene<-processed_data[["dvalidation"]][, c(1:7, 937:ncol(processed_data[["dvalidation"]]))]
  
  data_tes<-processed_data[["dtesting"]]
  data_tes_mirna<-processed_data[["dtesting"]][, 1:936]
  data_tes_gene<-processed_data[["dtesting"]][, c(1:7, 937:ncol(processed_data[["dtesting"]]))]
  
  tr_name<-paste(paste(c("data_tr", count_num), collapse = "_"), ".csv", sep = "")
  tr_mirna_name<-paste(paste(c("data_tr_mirna", count_num), collapse = "_"), ".csv", sep = "")
  tr_gene_name<-paste(paste(c("data_tr_gene", count_num), collapse = "_"), ".csv", sep = "")
  
  val_name<-paste(paste(c("data_val", count_num), collapse = "_"), ".csv", sep = "")
  val_mirna_name<-paste(paste(c("data_val_mirna", count_num), collapse = "_"), ".csv", sep = "")
  val_gene_name<-paste(paste(c("data_val_gene", count_num), collapse = "_"), ".csv", sep = "")
  
  tes_name<-paste(paste(c("data_tes", count_num), collapse = "_"), ".csv", sep = "")
  tes_mirna_name<-paste(paste(c("data_tes_mirna", count_num), collapse = "_"), ".csv", sep = "")
  tes_gene_name<-paste(paste(c("data_tes_gene", count_num), collapse = "_"), ".csv", sep = "")
  
  write.csv(data_tr, tr_name, row.names = F)
  write.csv(data_tr_mirna, tr_mirna_name, row.names = F)
  write.csv(data_tr_gene, tr_gene_name, row.names = F)
  
  write.csv(data_val, val_name, row.names = F)
  write.csv(data_val_mirna, val_mirna_name, row.names = F)
  write.csv(data_val_gene, val_gene_name, row.names = F)
  
  write.csv(data_tes, tes_name, row.names = F)
  write.csv(data_tes_mirna, tes_mirna_name, row.names = F)
  write.csv(data_tes_gene, tes_gene_name, row.names = F)
}
for(folder in file_names){
  file_direction<-paste("D:/DL/Variational autoencoder/Tryout_01_07_2021/divided_data", folder, sep = "/")
  setwd(file_direction)
  count_num<-unlist(strsplit(folder, split = "_"))[2]
  latent_folder<-paste("latent_features", count_num, sep = "_")
  dir.create(latent_folder)
}

paste(paste(paste("'Z", seq(1:25), sep = "_"), collapse = "', "), "'", sep = "")

beta_folder<-paste("beta", c(5, 25, 125, 500, 1000), sep = "_")
for(folder in file_names){
  file_direction<-paste("D:/DL/Variational autoencoder/Tryout_01_07_2021/divided_data", folder, sep = "/")
  count_num<-unlist(strsplit(folder, split = "_"))[2]
  file_direction_beta<-paste(file_direction, paste("latent_features", count_num, sep = "_"), sep = "/")
  setwd(file_direction_beta)
  for(beta_count in beta_folder){
    dir.create(beta_count)
  }
}

11410-7
grep("A1BG", colnames(data_tes_gene))
grep("ZZZ3", colnames(data_tes_gene))

#code testing
x<-1
y<-3
if(x==1&y==2){
  print("good job")
}else{
  print("nice try")
}






