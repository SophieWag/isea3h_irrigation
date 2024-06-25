#Figure 5: plots the partial dependencies

#read the partial dependencies from the output folder
pdp_dgg7_class <- read.table(file=paste0(oFol,"pdp_dgg_class.txt"),header = TRUE,sep =",")
pdp_ll_class <- read.table(file=paste0(oFol,"pdp_ll_class.txt"),header = TRUE,sep =",")
pdp_ll_reg <- read.table(file=paste0(oFol,"pdp_ll_reg.txt"),header = TRUE,sep =",")
pdp_dgg7_reg <- read.table(file=paste0(oFol,"pdp_dgg_reg.txt"),header = TRUE,sep =",")

pdp_dgg7_reg_gdp <- read.table(file=paste0(oFol,"pdp_dgg_reg_gdp.txt"),header = TRUE,sep =",")
pdp_dgg7_class_gdp <- read.table(file=paste0(oFol,"pdp_dgg_class_gdp.txt"),header = TRUE,sep =",")
pdp_ll_reg_gdp <- read.table(file=paste0(oFol,"pdp_ll_reg_gdp.txt"),header = TRUE,sep =",")
pdp_ll_class_gdp <- read.table(file=paste0(oFol,"pdp_ll_class_gdp.txt"),header = TRUE,sep =",")

pdp_dgg8_class <- read.table(file=paste0(oFol,"class_pdp_rf68.txt"),header = TRUE,sep =",")
pdp_dgg9_class <- read.table(file=paste0(oFol,"class_pdp_rf69.txt"),header = TRUE,sep =",")
pdp_ll_area_class <- read.table(file=paste0(oFol,"class_pdp_rf4_ll_area.txt"),header = TRUE,sep =",")

pdp_dgg8_reg <- read.table(file=paste0(oFol,"reg_pdp_rf78.txt"),header = TRUE,sep =",")
pdp_dgg9_reg <- read.table(file=paste0(oFol,"reg_pdp_rf79.txt"),header = TRUE,sep =",")
pdp_ll_area_reg <- read.table(file=paste0(oFol,"reg_pdp_rf5_area.txt"),header = TRUE,sep =",")

names <- c("Med_dgg7","Med_yhat_dgg7",
           "Dis_dgg7","Dis_yhat_dgg7",
           "Evap_dgg7","Evap_yhat_dgg7",
           "Dis2_dgg7","Dis2_yhat_dgg7",
           "Pop_dgg7","Pop_yhat_dgg7",
           "Prec_dgg7","Prec_yhat_dgg7",
           
           "Med_dgg8","Med_yhat_dgg8",
           "Dis_dgg8","Dis_yhat_dgg8",
           "Evap_dgg8","Evap_yhat_dgg8",
           "Dis2_dgg8","Dis2_yhat_dgg8",
           "Pop_dgg8","Pop_yhat_dgg8",
           "Prec_dgg8","Prec_yhat_dgg8",
           
           "Med_dgg9","Med_yhat_dgg9",
           "Dis_dgg9","Dis_yhat_dgg9",
           "Evap_dgg9","Evap_yhat_dgg9",
           "Dis2_dgg9","Dis2_yhat_dgg9",
           "Pop_dgg9","Pop_yhat_dgg9",
           "Prec-_dgg9","Prec_yhat_dgg9",
           
           "Med_ll","Med_yhat_ll",
           "Dis_ll","Dis_yhat_ll",
           "Evap_ll","Evap_yhat_ll",
           "Dis2_ll","Dis2_yhat_ll",
           "Pop_ll","Pop_yhat_ll",
           "Prec_ll","Prec_yhat_ll",
           
           "Med_ll_area","Med_yhat_ll_area",
           "Dis_ll_area","Dis_yhat_ll_area",
           "Evap_ll_area","Evap_yhat_ll_area",
           "Dis2_ll_area","Dis2_yhat_ll_area",
           "Pop_ll_area","Pop_yhat_ll_area",
           "Prec_ll_area","Prec_yhat_ll_area")


pdp_reg <- cbind(pdp_dgg7_reg,
                 pdp_dgg8_reg,
                 pdp_dgg9_reg,
                 pdp_ll_reg,
                 pdp_ll_area_reg)
names <- c("Med_dgg7","Med_yhat_dgg7",
           "Dis_dgg7","Dis_yhat_dgg7",
           "Evap_dgg7","Evap_yhat_dgg7",
           "Dis2_dgg7","Dis2_yhat_dgg7",
           "Pop_dgg7","Pop_yhat_dgg7",
           "Prec_dgg7","Prec_yhat_dgg7",
           
           "Pop_dgg8","Pop_yhat_dgg8",
           "Dis_dgg8","Dis_yhat_dgg8",
           "Evap_dgg8","Evap_yhat_dgg8",
           "Prec_dgg8","Prec_yhat_dgg8",
           "Med_dgg8","Med_yhat_dgg8",
           
           "Pop_dgg9","Pop_yhat_dgg9",
           "Dis_dgg9","Dis_yhat_dgg9",
           "Evap_dgg9","Evap_yhat_dgg9",
           "Prec_dgg9","Prec_yhat_dgg9",
           "Med_dgg9","Med_yhat_dgg9",
           
           "Med_ll","Med_yhat_ll",
           "Dis_ll","Dis_yhat_ll",
           "Evap_ll","Evap_yhat_ll",
           "Dis2_ll","Dis2_yhat_ll",
           "Pop_ll","Pop_yhat_ll",
           "Prec_ll","Prec_yhat_ll",
           
           "Pop_ll_area","Pop_yhat_ll_area",
           "Dis_ll_area","Dis_yhat_ll_area",
           "Evap_ll_area","Evap_yhat_ll_area",
           "Prec_ll_area","Prec_yhat_ll_area",
           "Med_ll_area","Med_yhat_ll_area")
names(pdp_reg) <- names

pdp_dgg8_class <- pdp_dgg8_class[,-(11:12)]
pdp_dgg9_class <- pdp_dgg9_class[,-(11:12)]
pdp_ll_area_class <- pdp_ll_area_class[,-(11:12)]
pdp_class <- cbind(pdp_dgg7_class,
                   pdp_dgg8_class,
                   pdp_dgg9_class,
                   pdp_ll_class,
                   pdp_ll_area_class)
names(pdp_class) <- names


### Classification
# Figure 5a 
  #Population density
  p1 <- ggplot(data = pdp_class, aes(x = Pop_ll))+
    geom_line(aes(y = Pop_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Pop_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
    geom_line(aes(y = Pop_yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_line(aes(y = Pop_yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_line(aes(y = Pop_yhat_dgg9,color = "ISEA3H resolution 9"))+
    theme_minimal()+
    scale_x_continuous(name = "Population density")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p1
  #Discharge
  p2 <- ggplot(data = pdp_class, aes(x = Dis_ll))+
    geom_line(aes(y = Dis_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Dis_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
    geom_line(aes(y = Dis_yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_line(aes(y = Dis_yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_line(aes(y = Dis_yhat_dgg9,color = "ISEA3H resolution 9"))+
    theme_minimal()+
    scale_x_continuous(name = "Discharge")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p2
  #Precipitation
  p3 <- ggplot(data = pdp_class, aes(x = Prec_ll))+
    geom_line(aes(y = Prec_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Prec_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
    geom_line(aes(y = Prec_yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_line(aes(y = Prec_yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_line(aes(y = Prec_yhat_dgg9,color = "ISEA3H resolution 9"))+
    theme_minimal()+
    scale_x_continuous(name = "Precipitation")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
p3
  #Evaporation
p4 <- ggplot(data = pdp_class, aes(x = Evap_ll))+
  geom_line(aes(y = Evap_yhat_ll,color = "Longitude-Latitude"))+
  geom_line(aes(y = Evap_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
  geom_line(aes(y = Evap_yhat_dgg7,color = "ISEA3H resolution 7"))+
  geom_line(aes(y = Evap_yhat_dgg8,color = "ISEA3H resolution 8"))+
  geom_line(aes(y = Evap_yhat_dgg9,color = "ISEA3H resolution 9"))+
  theme_minimal()+
  scale_x_continuous(name = "Evaporation")+ 
  scale_y_continuous(name = "Partial dependence")+
  scale_color_manual(name = "",
                     values = c("Longitude-Latitude" = "#D53E4F",
                                "Longitude-Latitude with area weights" = "#F06341",
                                "ISEA3H resolution 7" = "#3288BD",
                                "ISEA3H resolution 8" = "#3D9CB1" ,
                                "ISEA3H resolution 9" = "#4BADA9"),
                     breaks = c("Longitude-Latitude",
                                "Longitude-Latitude with area weights",
                                "ISEA3H resolution 7",
                                "ISEA3H resolution 8", 
                                "ISEA3H resolution 9"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=14),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
p4
  #Median increase in productivity
p5 <- ggplot(data = pdp_class, aes(x = Med_ll))+
  geom_line(aes(y = Med_yhat_ll,color = "Longitude-Latitude"))+
  geom_line(aes(y = Med_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
  geom_line(aes(y = Med_yhat_dgg7,color = "ISEA3H resolution 7"))+
  geom_line(aes(y = Med_yhat_dgg8,color = "ISEA3H resolution 8"))+
  geom_line(aes(y = Med_yhat_dgg9,color = "ISEA3H resolution 9"))+
  theme_minimal()+
  scale_x_continuous(name = "Median increase in productivity")+ 
  scale_y_continuous(name = "Partial dependence")+
  scale_color_manual(name = "",
                     values = c("Longitude-Latitude" = "#D53E4F",
                                "Longitude-Latitude with area weights" = "#F06341",
                                "ISEA3H resolution 7" = "#3288BD",
                                "ISEA3H resolution 8" = "#3D9CB1" ,
                                "ISEA3H resolution 9" = "#4BADA9"),
                     breaks = c("Longitude-Latitude",
                                "Longitude-Latitude with area weights",
                                "ISEA3H resolution 7",
                                "ISEA3H resolution 8", 
                                "ISEA3H resolution 9"))+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title=element_text(size=14),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))
p5
  #GDP
  pdp_gdp_dgg <- rbind(pdp_dgg_class_gdp[2,],pdp_dgg_class_gdp[3,],
                       pdp_dgg_class_gdp[5,],pdp_dgg_class_gdp[1,],
                       pdp_dgg_class_gdp[4,])
  
  pdp_gdp_ll <- rbind(pdp_ll_class_gdp[2,],pdp_ll_class_gdp[3,],
                      pdp_ll_class_gdp[5,],pdp_ll_class_gdp[1,],
                      pdp_ll_class_gdp[4,])
  pdp_gdp_class <- cbind(pdp_gdp_dgg, pdp_gdp_ll, yhat_dgg8,yhat_dgg9, yhat_ll_area,c(1,2,3,4,5))
  pdp_gdp_class <- pdp_gdp_class[,-3]
  names(pdp_gdp_class) <- c("GDP","yhat_dgg7", 
                            "yhat_ll",
                            "yhat_dgg8",
                            "yhat_dgg9",
                            "yhat_ll_area",
                            "order")
  row.names(pdp_gdp_class) <- NULL
  pdp_gdp_class$GDP <- as.factor(pdp_gdp_class$GDP)
  pdp_gdp_class$order <- as.factor(pdp_gdp_class$order)
  
  p6 <- ggplot(data = pdp_gdp_class, aes(x = order, group = 1))+
    geom_line(data = pdp_gdp_class[pdp_gdp_class$GDP != "missing",],aes(y = yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_point(stat = "identity",aes(y = yhat_dgg7,color = "ISEA3H resolution 7"))+
    
    geom_line(data = pdp_gdp_class[pdp_gdp_class$GDP != "missing",],aes(y = yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_point(stat = "identity",aes(y = yhat_dgg8,color = "ISEA3H resolution 8"))+
    
    geom_line(data = pdp_gdp_class[pdp_gdp_class$GDP != "missing",],aes(y = yhat_dgg9,color = "ISEA3H resolution 9"))+
    geom_point(stat = "identity",aes(y = yhat_dgg9,color = "ISEA3H resolution 9"))+
    
    geom_line(data = pdp_gdp_class[pdp_gdp_class$GDP != "missing",],aes(y = yhat_ll_area, color = "Longitude-Latitude with area weights"))+
    geom_point(stat = "identity",aes(y = yhat_ll_area, color = "Longitude-Latitude with area weights"))+
    
    geom_line(data = pdp_gdp_class[pdp_gdp_class$GDP != "missing",],aes(y = yhat_ll, color = "Longitude-Latitude"))+
    geom_point(stat = "identity",aes(y = yhat_ll, color = "Longitude-Latitude"))+
    
    theme_minimal()+
    scale_x_discrete(name = "GDP per capita",
                     labels = c("1" = "Low", "2"= "Lower Middle","3"= "Upper Middle",
                                "4"= "High","5"= "Missing"))+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p6
  #Create plot
  png(paste0(resultFol,"fig05a.jpeg"),width = 801, height = 500)
  p <- ggarrange(p1,p2,p3,p4,p5,p6,nrow = 3, ncol = 2, common.legend = TRUE, legend="bottom")
  p
  dev.off()
  
### Regression
  #Figure 5b
  #Population density
  p1 <- ggplot(data = pdp_reg, aes(x = Pop_ll))+
    geom_line(aes(y = Pop_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Pop_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
    geom_line(aes(y = Pop_yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_line(aes(y = Pop_yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_line(aes(y = Pop_yhat_dgg9,color = "ISEA3H resolution 9"))+
    theme_minimal()+
    scale_x_continuous(name = "Population density")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p1
  #Discharge
  p2 <- ggplot(data = pdp_reg, aes(x = Dis_ll))+
    geom_line(aes(y = Dis_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Dis_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
    geom_line(aes(y = Dis_yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_line(aes(y = Dis_yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_line(aes(y = Dis_yhat_dgg9,color = "ISEA3H resolution 9"))+
    theme_minimal()+
    scale_x_continuous(name = "Discharge")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p2
  
  #Precipitation
  p3 <- ggplot(data = pdp_reg, aes(x = Prec_ll))+
    geom_line(aes(y = Prec_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Prec_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
    geom_line(aes(y = Prec_yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_line(aes(y = Prec_yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_line(aes(y = Prec_yhat_dgg9,color = "ISEA3H resolution 9"))+
    theme_minimal()+
    scale_x_continuous(name = "Precipitation")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p3
  
  #Evaporation
  p4 <- ggplot(data = pdp_reg, aes(x = Evap_ll))+
    geom_line(aes(y = Evap_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Evap_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
    geom_line(aes(y = Evap_yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_line(aes(y = Evap_yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_line(aes(y = Evap_yhat_dgg9,color = "ISEA3H resolution 9"))+
    theme_minimal()+
    scale_x_continuous(name = "Evaporation")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p4
  
  #Median increase in productivity
  p5 <- ggplot(data = pdp_reg, aes(x = Med_ll))+
    geom_line(aes(y = Med_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Med_yhat_ll_area,color = "Longitude-Latitude with area weights"))+
    geom_line(aes(y = Med_yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_line(aes(y = Med_yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_line(aes(y = Med_yhat_dgg9,color = "ISEA3H resolution 9"))+
    theme_minimal()+
    scale_x_continuous(name = "Median increase in productivity")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p5
  #GDP
  pdp_gdp_dgg <- rbind(pdp_dgg_reg_gdp[2,],pdp_dgg_reg_gdp[3,],
                       pdp_dgg_reg_gdp[5,],pdp_dgg_reg_gdp[1,],
                       pdp_dgg_reg_gdp[4,])
  pdp_gdp_ll <- rbind(pdp_ll_reg_gdp[2,],pdp_ll_reg_gdp[3,],
                      pdp_ll_reg_gdp[5,],pdp_ll_reg_gdp[1,],
                      pdp_ll_reg_gdp[4,])
  pdp_gdp <- cbind(pdp_gdp_dgg,pdp_gdp_ll,yhat_dgg8,yhat_dgg9,yhat_ll_area,c(1,2,3,4,5))
  pdp_gdp <- pdp_gdp[,-3]
  names(pdp_gdp) <- c("GDP",
                      "yhat_dgg7",
                      "yhat_ll",
                      "yhat_dgg8",
                      "yhat_dgg9",
                      "yhat_ll_area",
                      "order")
  row.names(pdp_gdp) <- NULL
  pdp_gdp$GDP <- as.factor(pdp_gdp$GDP)
  pdp_gdp$order <- as.factor(pdp_gdp$order)
  
  p6 <- ggplot(data = pdp_gdp, aes(x = order, group = 1))+
    geom_line(data = pdp_gdp[pdp_gdp$GDP != "missing",],aes(y = yhat_dgg7,color = "ISEA3H resolution 7"))+
    geom_point(stat = "identity",aes(y = yhat_dgg7,color = "ISEA3H resolution 7"))+
    
    geom_line(data = pdp_gdp[pdp_gdp$GDP != "missing",],aes(y = yhat_dgg8,color = "ISEA3H resolution 8"))+
    geom_point(stat = "identity",aes(y = yhat_dgg8,color = "ISEA3H resolution 8"))+
    
    geom_line(data = pdp_gdp[pdp_gdp$GDP != "missing",],aes(y = yhat_dgg9,color = "ISEA3H resolution 9"))+
    geom_point(stat = "identity",aes(y = yhat_dgg9,color = "ISEA3H resolution 9"))+
    
    geom_line(data = pdp_gdp[pdp_gdp$GDP != "missing",],aes(y = yhat_ll, color = "Longitude-Latitude"))+
    geom_point(stat = "identity",aes(y = yhat_ll, color = "Longitude-Latitude"))+
    
    geom_line(data = pdp_gdp[pdp_gdp$GDP != "missing",],aes(y = yhat_ll_area, color = "Longitude-Latitude with area weights"))+
    geom_point(stat = "identity",aes(y = yhat_ll_area, color = "Longitude-Latitude with area weights"))+
    
    theme_minimal()+
    scale_x_discrete(name = "GDP per capita",
                     labels = c("1" = "Low", "2"= "Lower Middle","3"= "Upper Middle",
                                "4"= "High","5"= "Missing"))+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "",
                       values = c("Longitude-Latitude" = "#D53E4F",
                                  "Longitude-Latitude with area weights" = "#F06341",
                                  "ISEA3H resolution 7" = "#3288BD",
                                  "ISEA3H resolution 8" = "#3D9CB1" ,
                                  "ISEA3H resolution 9" = "#4BADA9"),
                       breaks = c("Longitude-Latitude",
                                  "Longitude-Latitude with area weights",
                                  "ISEA3H resolution 7",
                                  "ISEA3H resolution 8", 
                                  "ISEA3H resolution 9"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p6
  #Create plot
  png(paste0(resultFol,"fig05b.jpeg"),width = 801, height = 500)
  p <- ggarrange(p1,p2,p3,p4,p5,p6,nrow = 3, ncol = 2, common.legend = TRUE, legend="bottom")
  p
  dev.off()


