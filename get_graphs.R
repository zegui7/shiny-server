library(plotly)
library(reshape2)
library(stringi)
library(data.table)

res_names <- c("Ala","Arg","Asn","Asp","Cys","Gln","Glu","Gly","His","Ile",
               "Leu","Lys","Met","Phe","Pro","Ser","Thr","Trp","Tyr","Val")


make_trace <- function(data_frame1,data_frame2,data_frame3,x_name,y_name,labels) {
 data_frame1 <- data_frame1[order(as.character(data_frame1[,x_name])),]
 data_frame2 <- data_frame2[order(as.character(data_frame2[,x_name])),]
 data_frame3 <- data_frame3[order(as.character(data_frame3[,x_name])),] 
 final_data_frame <- cbind(data_frame1[x_name],data_frame1[y_name],
                           data_frame2[y_name],data_frame3[y_name])
 colnames(final_data_frame) <- c("x",labels)
 final_data_frame$x <- factor(final_data_frame$x)
 return(final_data_frame)
}

res_data_hs <- read.csv("res_data_hs.csv")
res_data_hs_sd <- read.csv("res_data_hs_sd.csv")
res_data_hs_sd[,c(3:ncol(res_data_hs_sd))] <- round(res_data_hs_sd[,c(3:ncol(res_data_hs_sd))],digits = 2)

res_data_ns <- read.csv("res_data_ns.csv")
res_data_ns_sd <- read.csv("res_data_ns_sd.csv")
res_data_ns_sd[,c(3:ncol(res_data_ns_sd))] <- round(res_data_ns_sd[,c(3:ncol(res_data_ns_sd))],digits = 2)

res_data_full <- read.csv("res_data_full.csv")
res_data_full_sd <- read.csv("res_data_full_sd.csv")
res_data_full_sd[,c(3:ncol(res_data_full_sd))] <- round(res_data_full_sd[,c(3:ncol(res_data_full_sd))],digits = 2)

reg_data <- read.csv("reg_data.csv")
all_global_means <- read.csv("all_global_means.csv")
all_global_sd <- read.csv("all_global_sd.csv")
all_global_sd[,c(2:ncol(all_global_sd))] <- round(all_global_sd[,c(2:ncol(all_global_sd))],digits = 2)

sasamon_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                           data_frame3 = res_data_full,
                           x_name = "PDBResName",y_name = "SASA.mon",
                           labels = c("SASA.mon_HS","SASA.mon_NS","SASA.mon_full"))
sasacomp_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                            data_frame3 = res_data_full,
                            x_name = "PDBResName",y_name = "SASA.comp",
                            labels = c("SASA.comp_HS","SASA.comp_NS","SASA.comp_full"))
sasarel_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                           data_frame3 = res_data_full,
                           x_name = "PDBResName",y_name = "SASA.rel",
                           labels = c("SASA.rel_HS","SASA.rel_NS","SASA.rel_full"))
sasadel_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                           data_frame3 = res_data_full,
                           x_name = "PDBResName",y_name = "SASA.del",
                           labels = c("SASA.del_HS","SASA.del_NS","SASA.del_full"))

traced_data_sasa <- cbind(sasamon_data,sasacomp_data[,2:4],
                     sasarel_data[2:4],sasadel_data[2:4])
traced_data_sasa$x <- factor(traced_data_sasa$x,levels = res_names)
traced_data_sasa[,c(2:ncol(traced_data_sasa))] <- round(traced_data_sasa[,c(2:ncol(traced_data_sasa))],digits = 2)

graph_SASA <- plot_ly(data = traced_data_sasa,x = ~x) %>%
 add_bars(y = ~ SASA.mon_HS,name = "HS",marker = list(color = "#A52929"),
          text = paste("SD =",res_data_hs_sd$SASA.mon)) %>%
 add_bars(y = ~ SASA.mon_NS,name = "NS",marker = list(color = "#264E86"),
          text = paste("SD =",res_data_ns_sd$SASA.mon)) %>%
 add_bars(y = ~ SASA.mon_full,name = "All",marker = list(color = "#228B22"),
          text = paste("SD =",res_data_full_sd$SASA.mon)) %>%
 add_trace(y = round(mean(all_global_means$SASA.mon[2]),2),mode = "lines",
           type = "scatter",name = "HS mean",
           line = list(dash = "dot",color = "#681919",width = 2),
           text = paste("SD =",all_global_sd$SASA.mon[2])) %>%
 add_trace(y = round(mean(all_global_means$SASA.mon[3]),2),mode = "lines",
           type = "scatter",name = "NS mean",
           line = list(dash = "dot",color = "#152d4f",width = 2),
           text = paste("SD =",all_global_sd$SASA.mon[3])) %>%
 add_trace(y = round(mean(all_global_means$SASA.mon[1]),2),mode = "lines",
           type = "scatter",name = "All mean",
           line = list(dash = "dot",color = "#20560d",width = 2),
           text = paste("SD =",all_global_sd$SASA.mon[1])) %>%
 
 add_bars(y = ~ SASA.comp_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$SASA.comp,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ SASA.comp_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$SASA.comp,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ SASA.comp_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$SASA.mon,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(mean(all_global_means$SASA.comp[2]),2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(mean(all_global_means$SASA.comp[3]),2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(mean(all_global_means$SASA.comp[1]),2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 add_bars(y = ~ SASA.del_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$SASA.del,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ SASA.del_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$SASA.del,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ SASA.del_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$SASA.del,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(mean(all_global_means$SASA.del[2]),2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(mean(all_global_means$SASA.del[3]),2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(mean(all_global_means$SASA.del[1]),2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 add_bars(y = ~ SASA.rel_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$SASA.rel,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ SASA.rel_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$SASA.rel,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ SASA.rel_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$SASA.rel,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(mean(all_global_means$SASA.rel[2]),2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(mean(all_global_means$SASA.rel[3]),2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(mean(all_global_means$SASA.rel[1]),2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 layout(xaxis = list(title = "Residue Name",showgrid = FALSE),
        yaxis = list(title = "SASA Values",showgrid = TRUE),
        updatemenus = list(
         list(
          x = 0.5,
          buttons = list(
           list(method = "update",
                args = list(
                 list("visible" = c(T,T,T,T,T,T,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F))),
                label = "Monomer"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    T,T,T,T,T,T,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F))),
                label = "Complex"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    T,T,T,T,T,T,
                                    F,F,F,F,F,F))),
                label = "Relative"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    T,T,T,T,T,T))),
                label = "Deletion")
          )
         )
        )
        )

dist2.5_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                           data_frame3 = res_data_full,
                           x_name = "PDBResName",y_name = "X.Dist.2.5",
                           labels = c("Dist2.5_HS","Dist2.5_NS","Dist2.5_full"))
dist4.0_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                            data_frame3 = res_data_full,
                            x_name = "PDBResName",y_name = "X.Dist.4.0",
                            labels = c("Dist4.0_HS","Dist4.0_NS","Dist4.0_full"))
hbonds_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                           data_frame3 = res_data_full,
                           x_name = "PDBResName",y_name = "X.H.Bonds",
                           labels = c("hbonds_HS","hbonds_NS","hbonds_full"))
sbridges_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                          data_frame3 = res_data_full,
                          x_name = "PDBResName",y_name = "X.Sbridges",
                          labels = c("sbridges_HS","sbridges_NS","sbridges_full"))
hydrophobic_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                          data_frame3 = res_data_full,
                          x_name = "PDBResName",y_name = "X.Hydrophobic",
                          labels = c("hydro_HS","hydro_NS","hydro_full"))

traced_data_struc <- cbind(dist2.5_data,dist4.0_data[,2:4],
                           hbonds_data[2:4],sbridges_data[,2:4],
                           hydrophobic_data[,2:4])
traced_data_struc$x <- factor(traced_data_struc$x,levels = res_names)
traced_data_struc[,c(2:ncol(traced_data_struc))] <- round(traced_data_struc[,c(2:ncol(traced_data_struc))],digits = 2)

graph_struc <- plot_ly(data = traced_data_struc,x = ~x) %>%
 add_bars(y = ~ Dist2.5_HS,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$X.Dist.2.5,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ Dist2.5_NS,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$X.Dist.2.5,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ Dist2.5_full,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$X.Dist.2.5,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(all_global_means$X.Dist.2.5[2],2),mode = "lines",
           type = "scatter",name = "HS mean",
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(all_global_means$X.Dist.2.5[3],2),mode = "lines",
           type = "scatter",name = "NS mean",
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(all_global_means$X.Dist.2.5[1],2),mode = "lines",
           type = "scatter",name = "All mean",
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 add_bars(y = ~ Dist4.0_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$X.Dist.4.0,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ Dist4.0_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$X.Dist.4.0,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ Dist4.0_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$X.Dist.4.0,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(all_global_means$X.Dist.4.0[2],2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(all_global_means$X.Dist.4.0[3],2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(all_global_means$X.Dist.4.0[1],2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 add_bars(y = ~ hbonds_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$X.H.Bonds,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ hbonds_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$X.H.Bonds,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ hbonds_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$X.H.Bonds,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(all_global_means$X.H.Bonds[2],2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(all_global_means$X.H.Bonds[3],2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(all_global_means$X.H.Bonds[1],2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 add_bars(y = ~ sbridges_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$X.Sbridges,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ sbridges_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$X.Sbridges,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ sbridges_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$X.Sbridges,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(all_global_means$X.Sbridges[2],2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(all_global_means$X.Sbridges[3],2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(all_global_means$X.Sbridges[1],2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 add_bars(y = ~ hydro_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$X.Hydrophobic,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ hydro_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$X.Hydrophobic,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ hydro_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$X.Hydrophobic,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(all_global_means$X.Hydrophobic[2],2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(all_global_means$X.Hydrophobic[3],2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(all_global_means$X.Hydrophobic[1],2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 layout(xaxis = list(title = "Residue Name",showgrid = FALSE),
        yaxis = list(title = "SASA Values",showgrid = TRUE),
        updatemenus = list(
         list(
          x = 0.5,
          buttons = list(
           list(method = "update",
                args = list(
                 list("visible" = c(T,T,T,T,T,T,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F))),
                label = "# of atoms at 2.5A"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    T,T,T,T,T,T,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F))),
                label = "# of atoms at 4.0A"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    T,T,T,T,T,T,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F))),
                label = "H-bonds"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    T,T,T,T,T,T,
                                    F,F,F,F,F,F))),
                label = "Salt bridges"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    T,T,T,T,T,T))),
                label = "Hydrophobic")
          )
         )
         )
 )

hsns_data <- data.frame(PDBResName = reg_data$PDBResName,
                        hsns_prop = reg_data$REG/sum(reg_data$REG),
                        interface_prop = reg_data$Interface/sum(reg_data$Interface),
                        hsns_inter_full = reg_data$REG/sum(reg_data$Interface),
                        hsns_inter_res = reg_data$REG/reg_data$Interface)

hsns_data$enrichment_factor <- hsns_data$hsns_prop/(sum(reg_data$REG)/sum(reg_data$Interface))
hsns_data[,c(2:ncol(hsns_data))] <- round(hsns_data[,c(2:ncol(hsns_data))],digits = 2)


graph_hsns <- plot_ly(data = hsns_data,x = ~ PDBResName) %>%
 add_bars(y = ~ hsns_prop,name = "HS",marker = list(color = "#A52929")) %>%
 add_bars(y = ~ interface_prop,name = "Interface",marker = list(color = "#264E86")) %>%
 add_bars(y = ~ enrichment_factor,name = "E. factor",marker = list(color = "#228B22"),
          visible = FALSE) %>%
 
 layout(xaxis = list(title = "Residue Name",showgrid = FALSE),
        yaxis = list(title = "Proportion",showgrid = TRUE),
        updatemenus = list(
         list(
          x = 0.5,
          buttons = list(
           list(method = "update",
                args = list(
                 list("visible" = c(T,T,F))),
                 label = "HS and interface"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,T))),
                 label = "Enrichment factor")
           )
          )
         )
        )

bf_full_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                           data_frame3 = res_data_full,
                           x_name = "PDBResName",y_name = "b_factor_full",
                           labels = c("b_factor_full_HS","b_factor_full_NS","b_factor_full_full"))
bf_side_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                           data_frame3 = res_data_full,
                           x_name = "PDBResName",y_name = "b_factor_side",
                           labels = c("b_factor_side_HS","b_factor_side_NS","b_factor_side_full"))
bf_back_data <- make_trace(data_frame1 = res_data_hs,data_frame2 = res_data_ns,
                           data_frame3 = res_data_full,
                           x_name = "PDBResName",y_name = "b_factor_back",
                           labels = c("b_factor_back_HS","b_factor_back_NS","b_factor_back_full"))

traced_data_bfact <- cbind(bf_full_data,bf_side_data[,2:4],
                           bf_back_data[2:4])
traced_data_bfact$x <- factor(traced_data_bfact$x,levels = res_names)
traced_data_bfact[,c(2:ncol(traced_data_struc))] <- round(traced_data_bfact[,c(2:ncol(traced_data_bfact))],digits = 2)

graph_bfact <- plot_ly(data = traced_data_bfact,x = ~x) %>%
 add_bars(y = ~ b_factor_full_HS,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$b_factor_full,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ b_factor_full_NS,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$b_factor_full,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ b_factor_full_full,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$b_factor_full,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(all_global_means$b_factor_full[2],2),mode = "lines",
           type = "scatter",name = "HS mean",
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(all_global_means$b_factor_full[3],2),mode = "lines",
           type = "scatter",name = "NS mean",
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(all_global_means$b_factor_full[1],2),mode = "lines",
           type = "scatter",name = "All mean",
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 add_bars(y = ~ b_factor_back_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$b_factor_back,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ b_factor_back_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$b_factor_back,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ b_factor_back_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$b_factor_back,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(all_global_means$b_factor_back[2],2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(all_global_means$b_factor_back[3],2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(all_global_means$b_factor_back[1],2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 add_bars(y = ~ b_factor_side_HS,visible = FALSE,name = "HS",marker = list(color = "#A52929"),
          error_y = ~ list(type = "array",array = res_data_hs_sd$b_factor_side,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ b_factor_side_NS,visible = FALSE,name = "NS",marker = list(color = "#264E86"),
          error_y = ~ list(type = "array",array = res_data_ns_sd$b_factor_side,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_bars(y = ~ b_factor_side_full,visible = FALSE,name = "All",marker = list(color = "#228B22"),
          error_y = ~ list(type = "array",array = res_data_full_sd$b_factor_side,color = "#000000",
                           width = 2, thickness = 1)) %>%
 add_trace(y = round(all_global_means$b_factor_side[2],2),mode = "lines",
           type = "scatter",name = "HS mean",visible = FALSE,
           line = list(dash = "dot",color = "#681919",width = 2)) %>%
 add_trace(y = round(all_global_means$b_factor_side[3],2),mode = "lines",
           type = "scatter",name = "NS mean",visible = FALSE,
           line = list(dash = "dot",color = "#152d4f",width = 2)) %>%
 add_trace(y = round(all_global_means$b_factor_side[1],2),mode = "lines",
           type = "scatter",name = "All mean",visible = FALSE,
           line = list(dash = "dot",color = "#20560d",width = 2)) %>%
 
 layout(xaxis = list(title = "Residue Name",showgrid = FALSE),
        yaxis = list(title = "B-factor values",showgrid = TRUE),
        updatemenus = list(
         list(
          x = 0.5,
          buttons = list(
           list(method = "update",
                args = list(
                 list("visible" = c(T,T,T,T,T,T,
                                    F,F,F,F,F,F,
                                    F,F,F,F,F,F))),
                label = "Full residue"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    T,T,T,T,T,T,
                                    F,F,F,F,F,F))),
                label = "Residue backbone"),
           list(method = "update",
                args = list(
                 list("visible" = c(F,F,F,F,F,F,
                                    F,F,F,F,F,F,
                                    T,T,T,T,T,T))),
                label = "Residue sidechain")
          )
         )
        )
 )

#htmlwidgets::saveWidget(graph_SASA,file = "graph_SASA.html")
#htmlwidgets::saveWidget(graph_struc,file = "graph_struc.html")
