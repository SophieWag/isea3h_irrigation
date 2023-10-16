#plots the variable importance of the random forest models

#Figure 5a
png(paste0(resultFol,"fig05a.png"), width = 801, height = 464)
    q <-  vip::vip(rf4,scale = TRUE,aesthetics = list(color = "#9E2961", fill = "#9E2961"))+
          ggtitle(paste0("Longitude-Latitude grid: Classification random forest"))+theme_bw()+
          theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
          scale_x_discrete(labels=c("PopDens"="Population density","Evaporation"="Evaporation",
                                    "MedInc"="Median increase in productivity","Discharge"="Discharge",
                                    "Precipiation"="Precipitation","GDP"="GDP per capita"))
print(q)
dev.off()

#Figure 5b
png(paste0(resultFol,"fig05b.png"),width = 801, height = 464)
    q <-  vip::vip(rf5,scale = TRUE,aesthetics = list(color = "#9E2961", fill = "#9E2961", alpha = 0.7))+
          ggtitle(paste0("Longitude-Latitude grid: Regression random forest"))+theme_bw()+
          theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
          scale_x_discrete(labels=c("PopDens"="Population density","Evaporation"="Evaporation",
                                    "MedInc"="Median increase in productivity",
                                    "Discharge"="Discharge","Prec"="Precipitation","GDP"="GDP per capita"))
print(q)
dev.off()

#Figure 5c
png(paste0(resultFol,"fig05c.png"), width = 801, height = 464)
q <-  vip::vip(rf6,scale = TRUE,aesthetics = list(color = "#280C4D", fill = "#280C4D"))+
  ggtitle(paste0("ISEA3H grid: Classification random forest"))+theme_bw()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(labels=c("PopDens"="Population density","Evaporation"="Evaporation",
                            "MedInc"="Median increase in productivity","Discharge"="Discharge",
                            "Precipiation"="Precipitation","GDP"="GDP per capita"))
print(q)
dev.off()

#Figure 5d
png(paste0(resultFol,"fig05d.png"),width = 801, height = 464)
q <-  vip::vip(rf5,scale = TRUE,aesthetics = list(color = "#280C4D", fill = "#280C4D", alpha = 0.7))+
  ggtitle(paste0("ISEA3H grid: Regression random forest"))+theme_bw()+
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16))+
  scale_x_discrete(labels=c("PopDens"="Population density","Evaporation"="Evaporation",
                            "MedInc"="Median increase in productivity",
                            "Discharge"="Discharge","Prec"="Precipitation","GDP"="GDP per capita"))
print(q)
dev.off()


