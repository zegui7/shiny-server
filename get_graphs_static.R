library(plotly)
library(reshape2)
library(stringi)
library(data.table)
library(reshape2)

go_long <- function(data_frame1,data_frame2,data_frame3,
                    data_frame1_sd,data_frame2_sd,data_frame3_sd,
                    x_name,y_name,labels){
 wide_frame <- cbind(data_frame1[c(x_name,y_name)],
                     data_frame2[y_name],data_frame3[y_name])
 wide_frame_sd <- cbind(data_frame1_sd[c(x_name,y_name)],data_frame2_sd[y_name],
                        data_frame3_sd[y_name])
 labels_sd <- paste(labels,"_sd",sep = '')
 colnames(wide_frame) <- c(x_name,labels)
 colnames(wide_frame_sd) <- c(x_name,labels_sd)
 
 long_frame <- melt(wide_frame,id.vars = x_name)
 long_frame_sd <- melt(wide_frame_sd,id.vars = x_name)
 colnames(long_frame) <- c(x_name,"variable","mean")
 colnames(long_frame_sd) <- c(x_name,"variable","sd")
 
 long_frame <- cbind(long_frame,sd = long_frame_sd$sd)
 return(long_frame)
}

long_data <- go_long(res_data_hs,res_data_ns,res_data_full,
                     res_data_hs_sd,res_data_ns_sd,res_data_full_sd,
                     "PDBResName","SASA.comp",c("comp_HS","comp_NS","comp_full"))

res_names <- c("Ala","Arg","Asn","Asp","Cys","Gln","Glu","Gly","His","Ile",
               "Leu","Lys","Met","Phe","Pro","Ser","Thr","Trp","Tyr","Val")

ggplot(data = long_data,mapping = aes(x = PDBResName,y = mean,fill = variable)) + 
 geom_errorbar(aes(ymax = mean + sd,ymin = mean - sd), 
                 position='dodge', width=0.25) +
 geom_bar(stat="identity",position = "dodge") + 
 scale_fill_manual(values = c("#A52929","#264E86","#228B22"),name="",
                   labels=c("HS","NS","Global")) +
 ggtitle("title") +
 theme_classic() + 
 theme(plot.title = element_text(hjust = 0.5)) +
 geom_abline(slope = 0, intercept = all_global_means[2,"SASA.comp"],linetype = 2,
             color = "#681919",size = 1) +
 geom_abline(slope = 0, intercept = all_global_means[3,"SASA.comp"],linetype = 2,
             color = "#0b4391",size = 1) +
 geom_abline(slope = 0, intercept = all_global_means[1,"SASA.comp"],linetype = 2,
             color = "#20560d", size = 1) +
 ylab("Number of neighbour residues mean") + 
 xlab("Residue")

res_data_hs <- read.csv("res_data_hs.csv")
res_data_hs_sd <- read.csv("res_data_hs_sd.csv")
res_data_ns <- read.csv("res_data_ns.csv")
res_data_ns_sd <- read.csv("res_data_ns_sd.csv")
res_data_full <- read.csv("res_data_full.csv")
res_data_full_sd <- read.csv("res_data_full_sd.csv")

reg_data <- read.csv("reg_data.csv")
all_global_means <- read.csv("all_global_means.csv")
all_global_sd <- read.csv("all_global_sd.csv")

