#Figure 6: plots the partial dependencies

#read the partial dependencies from the output folder
pdp_dgg_class <- read.table(file=paste0(oFol,"pdp_dgg_class.txt"),header = TRUE,sep =",")
pdp_ll_class <- read.table(file=paste0(oFol,"pdp_ll_class.txt"),header = TRUE,sep =",")
pdp_ll_reg <- read.table(file=paste0(oFol,"pdp_ll_reg.txt"),header = TRUE,sep =",")
pdp_dgg_reg <- read.table(file=paste0(oFol,"pdp_dgg_reg.txt"),header = TRUE,sep =",")


pdp_dgg_reg_gdp <- read.table(file=paste0(oFol,"pdp_dgg_reg_gdp.txt"),header = TRUE,sep =",")
pdp_dgg_class_gdp <- read.table(file=paste0(oFol,"pdp_dgg_class_gdp.txt"),header = TRUE,sep =",")

pdp_ll_reg_gdp <- read.table(file=paste0(oFol,"pdp_ll_reg_gdp.txt"),header = TRUE,sep =",")
pdp_ll_class_gdp <- read.table(file=paste0(oFol,"pdp_ll_class_gdp.txt"),header = TRUE,sep =",")
pdp_ll_class_pop <- read.table(file=paste0(oFol,"pdp_ll_class_.txt"),header = TRUE,sep =",")

names <- c("Med_dgg","Med_yhat_dgg","Dis_dgg","Dis_yhat_dgg",
           "Evap_dgg","Evap_yhat_dgg","Dis2_dgg","Dis2_yhat_dgg",
           "Pop_dgg","Pop_yhat_dgg","Prec-_dgg","Prec_yhat_dgg",
           "Med_ll","Med_yhat_ll","Dis_ll","Dis_yhat_ll",
           "Evap_ll","Evap_yhat_ll","Dis2_ll","Dis2_yhat_ll",
           "Pop_ll","Pop_yhat_ll","Prec_ll","Prec_yhat_ll")


pdp_reg <- cbind(pdp_dgg_reg,pdp_ll_reg)
names(pdp_reg) <- names

pdp_class <- cbind(pdp_dgg_class,pdp_ll_class)
names(pdp_class) <- names

# Figure 6a 
  #Population density
  p1 <- ggplot(data = pdp_class, aes(x = Pop_ll))+
    geom_line(aes(y = Pop_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Pop_yhat_dgg,color = "ISEA3H"))+
    theme_minimal()+
    scale_x_continuous(name = "Population density")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Classification",values = c("#000004FF","#BB3754FF"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  
  #Discharge
  p2 <- ggplot(data = pdp_class, aes(x = Dis_dgg))+
    geom_line(aes(y = Dis_yhat_dgg,color = "ISEA3H"))+
    geom_line(aes(y = Dis_yhat_ll,color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_continuous(name = "Discharge")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Classification",values = c("#000004FF","#BB3754FF"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  
  #Precipitation
  p3 <- ggplot(data = pdp_class, aes(x = Prec_ll))+
    geom_line(aes(y = Prec_yhat_dgg,color = "ISEA3H"))+
    geom_line(aes(y = Prec_yhat_ll,color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_continuous(name = "Precipitation")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Classification",values = c("#000004FF","#BB3754FF"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))

  #Evaporation
  p4 <- ggplot(data = pdp_class, aes(x = Evap_ll))+
    geom_line(aes(y = Evap_yhat_dgg,color = "ISEA3H"))+
    geom_line(aes(y = Evap_yhat_ll,color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_continuous(name = "Evaporation")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Classification",values = c("#BB3754FF","#000004FF"),
                       labels = c("Longitude-Latitude","ISEA3H"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))

  #Median increase in productivity
  p5 <- ggplot(data = pdp_class, aes(x = Med_ll))+
    geom_line(aes(y = Med_yhat_dgg,color = "ISEA3H"))+
    geom_line(aes(y = Med_yhat_ll,color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_continuous(name = "Median increase in productivity")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Classification",values = c("#BB3754FF","#000004FF"),
                       labels = c("Longitude-Latitude","ISEA3H"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  #GDP
  pdp_gdp_dgg <- rbind(pdp_dgg_class_gdp[2,],pdp_dgg_class_gdp[3,],
                       pdp_dgg_class_gdp[5,],pdp_dgg_class_gdp[1,],
                       pdp_dgg_class_gdp[4,])
  
  pdp_gdp_ll <- rbind(pdp_ll_class_gdp[2,],pdp_ll_class_gdp[3,],
                      pdp_ll_class_gdp[5,],pdp_ll_class_gdp[1,],
                      pdp_ll_class_gdp[4,])
  pdp_gdp_class <- cbind(pdp_gdp_dgg, pdp_gdp_ll,c(1,2,3,4,5))
  pdp_gdp_class <- pdp_gdp_class[,-3]
  names(pdp_gdp_class) <- c("GDP","yhat_dgg", "yhat_ll","order")
  row.names(pdp_gdp_class) <- NULL
  pdp_gdp_class$GDP <- as.factor(pdp_gdp_class$GDP)
  pdp_gdp_class$order <- as.factor(pdp_gdp_class$order)
  
  p6 <- ggplot(data = pdp_gdp_class, aes(x = order, group = 1))+
    geom_line(data = pdp_gdp_class[pdp_gdp_class$GDP != "missing",],aes(y = yhat_dgg,color = "ISEA3H"))+
    geom_point(stat = "identity",aes(y = yhat_dgg,color = "ISEA3H"))+
    geom_line(data = pdp_gdp_class[pdp_gdp_class$GDP != "missing",],aes(y = yhat_ll, color = "Longitude-Latitude"))+
    geom_point(stat = "identity",aes(y = yhat_ll, color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_discrete(name = "GDP per capita",
                     labels = c("1" = "Low", "2"= "Lower Middle","3"= "Upper Middle",
                                "4"= "High","5"= "Missing"))+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Classification",values = c("#BB3754FF","#000004FF"),
                       labels = c("Longitude-Latitude","ISEA3H"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p6
  #Create plot
  png(paste0(resultFol,"fig6a.jpeg"),width = 801, height = 464)
  p <- ggarrange(p1,p2,p3,p4,p5,p6,nrow = 3, ncol = 2, common.legend = TRUE, legend="bottom")
  p
  dev.off()
  
  
  #Figure 6b
  #Population density
  p1 <- ggplot(data = pdp_reg, aes(x = Pop_ll))+
    geom_line(aes(y = Pop_yhat_ll,color = "Longitude-Latitude"))+
    geom_line(aes(y = Pop_yhat_dgg,color = "ISEA3H"))+
    theme_minimal()+
    scale_x_continuous(name = "Population density")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Regression",values = c("#000004FF","#BB3754FF"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  
  #Discharge
  p2 <- ggplot(data = pdp_reg, aes(x = Dis_dgg))+
    geom_line(aes(y = Dis_yhat_dgg,color = "ISEA3H"))+
    geom_line(aes(y = Dis_yhat_ll,color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_continuous(name = "Discharge")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Regression",values = c("#000004FF","#BB3754FF"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  
  #Precipitation
  p3 <- ggplot(data = pdp_reg, aes(x = Prec_ll))+
    geom_line(aes(y = Prec_yhat_dgg,color = "ISEA3H"))+
    geom_line(aes(y = Prec_yhat_ll,color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_continuous(name = "Precipitation")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Regression",values = c("#000004FF","#BB3754FF"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  
  #Evaporation
  p4 <- ggplot(data = pdp_reg, aes(x = Evap_ll))+
    geom_line(aes(y = Evap_yhat_dgg,color = "ISEA3H"))+
    geom_line(aes(y = Evap_yhat_ll,color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_continuous(name = "Evaporation")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Regression",values = c("#BB3754FF","#000004FF"),
                       labels = c("Longitude-Latitude","ISEA3H"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  
  #Median increase in productivity
  p5 <- ggplot(data = pdp_reg, aes(x = Med_ll))+
    geom_line(aes(y = Med_yhat_dgg,color = "ISEA3H"))+
    geom_line(aes(y = Med_yhat_ll,color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_continuous(name = "Median increase in productivity")+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Regression",values = c("#BB3754FF","#000004FF"),
                       labels = c("Longitude-Latitude","ISEA3H"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  #GDP
  pdp_gdp_dgg <- rbind(pdp_dgg_reg_gdp[2,],pdp_dgg_reg_gdp[3,],
                       pdp_dgg_reg_gdp[5,],pdp_dgg_reg_gdp[1,],
                       pdp_dgg_reg_gdp[4,])
  pdp_gdp_ll <- rbind(pdp_ll_reg_gdp[2,],pdp_ll_reg_gdp[3,],
                      pdp_ll_reg_gdp[5,],pdp_ll_reg_gdp[1,],
                      pdp_ll_reg_gdp[4,])
  
  pdp_gdp <- cbind(pdp_gdp_dgg,pdp_gdp_ll,c(1,2,3,4,5))
  pdp_gdp <- pdp_gdp[,-3]
  names(pdp_gdp) <- c("GDP","yhat_dgg","yhat_ll","order")
  row.names(pdp_gdp) <- NULL
  pdp_gdp$GDP <- as.factor(pdp_gdp$GDP)
  pdp_gdp$order <- as.factor(pdp_gdp$order)
  
  p6 <- ggplot(data = pdp_gdp, aes(x = order, group = 1))+
    geom_line(data = pdp_gdp[pdp_gdp$GDP != "missing",],aes(y = yhat_dgg,color = "ISEA3H"))+
    geom_point(stat = "identity",aes(y = yhat_dgg,color = "ISEA3H"))+
    geom_line(data = pdp_gdp[pdp_gdp$GDP != "missing",],aes(y = yhat_ll, color = "Longitude-Latitude"))+
    geom_point(stat = "identity",aes(y = yhat_ll, color = "Longitude-Latitude"))+
    theme_minimal()+
    scale_x_discrete(name = "GDP per capita",
                     labels = c("1" = "Low", "2"= "Lower Middle","3"= "Upper Middle",
                                "4"= "High","5"= "Missing"))+ 
    scale_y_continuous(name = "Partial dependence")+
    scale_color_manual(name = "Regression",values = c("#BB3754FF","#000004FF"),
                       labels = c("Longitude-Latitude","ISEA3H"))+
    theme(axis.text.x = element_text(size=12),
          axis.text.y = element_text(size=12),
          axis.title=element_text(size=14),
          legend.title=element_text(size=12),
          legend.text=element_text(size=12))
  p6
  #Create plot
  png(paste0(resultFol,"fig6b.jpeg"),width = 801, height = 464)
  p <- ggarrange(p1,p2,p3,p4,p5,p6,nrow = 3, ncol = 2, common.legend = TRUE, legend="bottom")
  p
  dev.off()
  
  
  

