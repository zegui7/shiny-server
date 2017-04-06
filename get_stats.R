library(reshape2)
library(stringi)
library(data.table)
library(stats)

res_names <- c("Ala","Arg","Asn","Asp","Cys","Gln","Glu","Gly","His","Ile",
               "Leu","Lys","Met","Phe","Pro","Ser","Thr","Trp","Tyr","Val")

full_data <- read.csv("/home/jalmeida/SPOT_ON/ppi4dock/ppi4dock_final_full.csv",
                      header = TRUE,row.names = NULL)
full_data_back <- full_data
full_data <- full_data_back
full_data$PDBResName <- factor(stri_trans_general(full_data$PDBResName,
                                                  id = "Title"))
full_data <- full_data[,-c(1,2,3,5,6,7,8,9,10,12,13,14,15,16,29:42)]
full_data$Interface <- rep(1,nrow(full_data))
full_data[,3:14] <- abs(full_data[,3:14])
  
hs_data <- full_data[full_data$REG == 1,]
ns_data <- full_data[full_data$REG == 0,]

reg_data <- aggregate(REG ~ PDBResName, data = full_data,sum)
reg_data$Interface <- aggregate(Interface ~ PDBResName, data = full_data, sum)$Interface

full_data_mean <- colMeans(full_data[,-1])
full_data_sd <- apply(full_data[,-1],2,sd)
hs_data_mean <- colMeans(hs_data[,-1])
hs_data_sd <- apply(hs_data[,-1],2,sd)
ns_data_mean <- colMeans(ns_data[,-1])
ns_data_sd <- apply(ns_data[,-1],2,sd)

all_global_means <- rbind(full_data_mean,hs_data_mean,ns_data_mean)
all_global_sd <- rbind(full_data_sd,hs_data_sd,ns_data_sd)

res_data_full <- aggregate(. ~ PDBResName,full_data,FUN = mean)
extra <- res_names[!(res_names %in% res_data_full$PDBResName)]
extra_df <- cbind(extra,data.frame(matrix(data = 0,nrow = length(extra),
                                          ncol = ncol(res_data_full) - 1)))
colnames(extra_df) <- colnames(res_data_full)
res_data_full <- rbind(res_data_full,extra_df)

res_data_full_sd <- aggregate(. ~ PDBResName,full_data,FUN = sd)
extra <- res_names[!(res_names %in% res_data_full_sd$PDBResName)]
extra_df <- cbind(extra,data.frame(matrix(data = 0,nrow = length(extra),
                                          ncol = ncol(res_data_full_sd) - 1)))
colnames(extra_df) <- colnames(res_data_full_sd)
res_data_full_sd <- rbind(res_data_full_sd,extra_df)

res_data_hs <- aggregate(. ~ PDBResName,hs_data,FUN = mean)
extra <- res_names[!(res_names %in% res_data_hs$PDBResName)]
extra_df <- cbind(extra,data.frame(matrix(data = 0,nrow = length(extra),
                                          ncol = ncol(res_data_hs) - 1)))
colnames(extra_df) <- colnames(res_data_hs)
res_data_hs <- rbind(res_data_hs,extra_df)

res_data_hs_sd <- aggregate(. ~ PDBResName,hs_data,FUN = sd)
extra <- res_names[!(res_names %in% res_data_hs_sd$PDBResName)]
extra_df <- cbind(extra,data.frame(matrix(data = 0,nrow = length(extra),
                                          ncol = ncol(res_data_hs_sd) - 1)))
colnames(extra_df) <- colnames(res_data_hs_sd)
res_data_hs_sd <- rbind(res_data_hs_sd,extra_df)

res_data_ns <- aggregate(. ~ PDBResName,ns_data,FUN = mean)
extra <- res_names[!(res_names %in% res_data_ns$PDBResName)]
extra_df <- cbind(extra,data.frame(matrix(data = 0,nrow = length(extra),
                                          ncol = ncol(res_data_ns) - 1)))
colnames(extra_df) <- colnames(res_data_ns)
res_data_ns <- rbind(res_data_ns,extra_df)

res_data_ns_sd <- aggregate(. ~ PDBResName,ns_data,FUN = sd)
extra <- res_names[!(res_names %in% res_data_ns_sd$PDBResName)]
extra_df <- cbind(extra,data.frame(matrix(data = 0,nrow = length(extra),
                                          ncol = ncol(res_data_ns_sd) - 1)))
colnames(extra_df) <- colnames(res_data_ns_sd)
res_data_ns_sd <- rbind(res_data_ns_sd,extra_df)

write.csv(res_data_full,"res_data_full.csv")
write.csv(res_data_full_sd,"res_data_full_sd.csv")
write.csv(res_data_hs,"res_data_hs.csv")
write.csv(res_data_hs_sd,"res_data_hs_sd.csv")
write.csv(res_data_ns,"res_data_ns.csv")
write.csv(res_data_ns_sd,"res_data_ns_sd.csv")

write.csv(all_global_means,"all_global_means.csv")
write.csv(all_global_sd,"all_global_sd.csv")
write.csv(reg_data,"reg_data.csv")

