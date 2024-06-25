#plots the variable importance of the random forest models

#Figure 4a
## Regression
varimp_7 <- read.table(file=paste0(resultFol,"rf7_dgg7_varimp.txt"),header = TRUE,sep =",")
varimp_8 <- read.table(file=paste0(resultFol,"rf7_dgg8_varimp.txt"),header = TRUE,sep =",")
varimp_9 <- read.table(file=paste0(resultFol,"rf7_dgg9_varimp.txt"),header = TRUE,sep =",")
varimp <- rbind(varimp_7,varimp_8,varimp_9)
varimp$res <- c(rep("7",6),rep("8",6),rep("9",6))
varimp$res <- as.factor(varimp$res)

png(paste0(resultFol,"fig04d_dgg.png"),width = 801, height = 464)
q <- ggplot(varimp, aes(x = reorder(Variable, Importance), y = Importance, fill = res)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_x_discrete(labels=c("PopDens"="Population density","Evaporation"="Evaporation",
                            "MedInc"="Median increase in productivity",
                            "Discharge"="Discharge","Precipitation"="Precipitation",
                            "GDP"="GDP per capita"))+
  scale_fill_manual(values = c("#3288BD","#3D9CB1","#4BADA9"))+
  labs(title = "",
       x = " ",
       y = "Importance",
       fill = "Resolution") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        legend.text=element_text(size=12),
        legend.title = element_text(size = 12))
print(q)
dev.off()



### Classification
varimp_7 <- read.table(file=paste0(resultFol,"rf6_dgg7_varimp.txt"),header = TRUE,sep =",")
varimp_8 <- read.table(file=paste0(resultFol,"rf6_dgg8_varimp.txt"),header = TRUE,sep =",")
varimp_9 <- read.table(file=paste0(resultFol,"rf6_dgg9_varimp.txt"),header = TRUE,sep =",")

varimp <- rbind(varimp_7,varimp_8,varimp_9)
varimp$res <- c(rep("7",6),rep("8",6),rep("9",6))
varimp$res <- as.factor(varimp$res)

png(paste0(resultFol,"fig04c_dgg.png"),width = 801, height = 464)
q <- ggplot(varimp, aes(x = reorder(Variable, Importance), y = Importance, fill = res)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_x_discrete(labels=c("PopDens"="Population density","Evaporation"="Evaporation",
                            "MedInc"="Median increase in productivity",
                            "Discharge"="Discharge","Preccipitation"="Precipitation",
                            "GDP"="GDP per capita"))+
  scale_fill_manual(values = c("#3288BD","#3D9CB1","#4BADA9"))+
  labs(title = "",
       x = " ",
       y = "Importance",
       fill = "Resolution") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        legend.text=element_text(size=12),
        legend.title = element_text(size = 12))
print(q)
dev.off()


## Classification - Lon-Lat
varimp_4 <- read.table(file=paste0(resultFol,"rf4_ll_varimp.txt"),header = TRUE,sep =",")
varimp_area <- read.table(file=paste0(resultFol,"rf4_ll_area_varimp.txt"),header = TRUE,sep =",")

varimp <- rbind(varimp_4,varimp_area)
varimp$res <- c(rep("Basic",6),rep("With area weights",6))
varimp$res <- as.factor(varimp$res)

png(paste0(resultFol,"fig04a_ll.png"),width = 801, height = 464)
q <- ggplot(varimp, aes(x = reorder(Variable, Importance), y = Importance, fill = res)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_x_discrete(labels=c("PopDens"="Population density","Evaporation"="Evaporation",
                            "MedInc"="Median increase in productivity",
                            "Discharge"="Discharge","Precipitation"="Precipitation","GDP"="GDP per capita"))+
  scale_fill_manual(values = c("#D53E4F","#F06341"))+
  labs(title = "",
       x = " ",
       y = "Importance",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        legend.text=element_text(size=12))
print(q)
dev.off()


## Regression - Lon-Lat
varimp_5 <- read.table(file=paste0(resultFol,"rf5_ll_varimp.txt"),header = TRUE,sep =",")
varimp_area <- read.table(file=paste0(resultFol,"rf5_ll_area_varimp.txt"),header = TRUE,sep =",")

varimp <- rbind(varimp_5,varimp_area)
varimp$res <- c(rep("Basic",6),rep("With area weights",6))
varimp$res <- as.factor(varimp$res)

png(paste0(resultFol,"fig04b_ll.png"),width = 801, height = 464)
q <- ggplot(varimp, aes(x = reorder(Variable, Importance), y = Importance, fill = res)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_x_discrete(labels=c("PopDens"="Population density","Evaporation"="Evaporation",
                            "MedInc"="Median increase in productivity",
                            "Discharge"="Discharge","Precipitation"="Precipitation","GDP"="GDP per capita"))+
  scale_fill_manual(values = c("#D53E4F","#F06341"))+
  labs(title = "",
       x = " ",
       y = "Importance",
       fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        legend.text=element_text(size=12))
print(q)
dev.off()


