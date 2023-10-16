#creates descriptive plots
#final.RData has to be loaded
#Figure A2b: ISEA3H histogram
png(paste0(resultFol,"figA2b.png"), width = 2000, height = 1000, units = "px", res = 400, pointsize = 4)
g2 <- ggplot(dgg_test, aes(x=Irrigation)) + geom_histogram(color = "#280C4D", fill = "#280C4D", bins = 50)
g2 <- g2 + theme_minimal() + labs(x="Irrigation Fraction", y = "Count")
g2
dev.off()

#Figure A2a: Lon-Lat histogram
png(paste0(resultFol,"figA2a.png"), width = 2000, height = 1000, units = "px", res = 400, pointsize = 4)
g2 <- ggplot(ll_test, aes(x=Irrigation)) + geom_histogram(color = "#280C4D", fill = "#280C4D", bins = 50)
g2 <- g2 + theme_minimal() + labs(x="Irrigation Fraction", y = "Count")
g2
dev.off()



## Table A1: Desc table 
train_data <- ll_train
train_data <- dgg_train

means <- c(mean(train_data$Irrigation),
           mean(train_data$PopDens),
           mean(train_data$MedInc),
           mean(train_data$Discharge),
           mean(train_data$Precipitation),
           mean(train_data$Evaporation))

sds <- c(sd(train_data$Irrigation),
         sd(train_data$PopDens),
         sd(train_data$MedInc),
         sd(train_data$Discharge),
         sd(train_data$Precipitation),
         sd(train_data$Evaporation))

mins <- c(min(train_data$Irrigation),
          min(train_data$PopDens),
          min(train_data$MedInc),
          min(train_data$Discharge),
          min(train_data$Precipitation),
          min(train_data$Evaporation))

maxs <- c(max(train_data$Irrigation),
          max(train_data$PopDens),
          max(train_data$MedInc),
          max(train_data$Discharge),
          max(train_data$Precipitation),
          max(train_data$Evaporation))

meds <- c(median(train_data$Irrigation),
          median(train_data$PopDens),
          median(train_data$MedInc),
          median(train_data$Discharge),
          median(train_data$Precipitation),
          median(train_data$Evaporation))

desc <- cbind(round(means,4), round(sds,4), round(mins,4), round(maxs,4), round(meds,4))
colnames(desc) <- c("Mean", "SD","Min","Max","Median")
rownames(desc) <- c("Irrigation fraction","Population denisty",
                    "Median increase in productiviy",
                    "Discharge","Precipitation","Evaporation")

write.table(desc,paste0(resultFol,"tabA1.txt"),col.names = TRUE,sep = ",",row.names = FALSE)

#Figure A1: Global mean plot
t <- seq(1902, 2005)
mean_irr <- apply(irrFrac[,2:105],c(2),mean)
mean_popdens <- apply(popdens[,2:105],c(2),mean)
mean_prec <- apply(prec[,2:105],c(2),mean)
mean_median <- apply(median_increase[,2:105],c(2),mean)
mean_eva <- apply(evaporation[,2:105],c(2),mean)
mean_dis <- apply(dis[,2:105],c(2),mean)
mean_gdp <- apply(lpjGDPpc[,2:105],c(2),mean, na.rm = TRUE)
cols <- openColours("inferno", 9)
cols
### Create data frame
ts <- cbind(t, mean_irr, mean_gdp, mean_popdens,
            mean_prec, mean_dis, mean_median,
            mean_eva)
ts <- as.data.frame(ts)

#png(paste0(resultFol,"Mean_Irr.png"))
#p <- ggplot(ts)
#p <- p +geom_line(aes(x=t, y=mean_irr), colour = "#000004")+
#  xlab("Year")+ylab("Global Mean Irrigation Fraction")+theme_minimal()+
#  theme(axis.title.x = element_text(colour="black", size=14, face="bold"),
#        axis.title.y = element_text(colour="black", size=14, face="bold"),
#        axis.text.x = element_text(colour="black", size=14),
#        axis.text.y = element_text(colour="black", size=14))
#print(p)
#dev.off()
png(paste0(resultFol,"figA1a.png"))
p <- ggplot(ts)
p <- p +geom_line(aes(x=t, y=mean_popdens), colour = "#280C4D")+
  xlab("Year")+ylab("Global Mean Population Density")+theme_minimal()+
  theme(axis.title.x = element_text(colour="black", size=14, face="bold"),
        axis.title.y = element_text(colour="black", size=14, face="bold"),
        axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  scale_x_continuous(breaks=c(0,1920,1950,1980,2000))
print(p)
dev.off()
png(paste0(resultFol,"figA1b.png"))
p <- ggplot(ts)
p <- p +geom_line(aes(x=t, y=mean_dis), colour = "#D34941")+
  xlab("Year")+ylab("Global Mean Discharge")+theme_minimal()+ 
  theme(axis.title.x = element_text(colour="black", size=14, face="bold"),
        axis.title.y = element_text(colour="black", size=14, face="bold"),
        axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  scale_x_continuous(breaks=c(0,1920,1950,1980,2000))
print(p)
dev.off()
png(paste0(resultFol,"figA1c.png"))
p <- ggplot(ts[-(1:2),])
p <- p +geom_line(aes(x=t, y=mean_eva), colour = "#F37D17")+
  xlab("Year")+ylab("Global Mean Evaporation")+theme_minimal()+
  theme(axis.title.x = element_text(colour="black", size=14, face="bold"),
        axis.title.y = element_text(colour="black", size=14, face="bold"),
        axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  scale_x_continuous(breaks=c(0,1920,1950,1980,2000))
print(p)
dev.off()
png(paste0(resultFol,"figA1d.png"))
p <- ggplot(ts[-(1:2),])
p <- p +geom_line(aes(x=t, y=mean_median), colour = "#64156C")+
  xlab("Year")+ylab("Global Mean Median Increase in Productivity")+theme_minimal()+
  theme(axis.title.x = element_text(colour="black", size=14, face="bold"),
        axis.title.y = element_text(colour="black", size=14, face="bold"),
        axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  scale_x_continuous(breaks=c(0,1920,1950,1980,2000))
print(p)
dev.off()
png(paste0(resultFol,"figA1e.png"))
p <- ggplot(ts)
p <- p +geom_line(aes(x=t, y=mean_prec), colour = "#9E2961")+
  xlab("Year")+ylab("Global Mean Precipitation")+theme_minimal()+
  theme(axis.title.x = element_text(colour="black", size=14, face="bold"),
        axis.title.y = element_text(colour="black", size=14, face="bold"),
        axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  scale_x_continuous(breaks=c(0,1920,1950,1980,2000))
print(p)
dev.off()

png(paste0(resultFol,"figA1f.png"))
p <- ggplot(ts)
p <- p +geom_line(aes(x=t, y=mean_gdp), colour = "#F8C02C")+
  xlab("Year")+ylab("Global Mean GDP per capita")+theme_minimal()+
  theme(axis.title.x = element_text(colour="black", size=14, face="bold"),
        axis.title.y = element_text(colour="black", size=14, face="bold"),
        axis.text.x = element_text(colour="black", size=14),
        axis.text.y = element_text(colour="black", size=14))+
  scale_x_continuous(breaks=c(0,1920,1950,1980,2000))
print(p)
dev.off()




# Figure A3
gdp <<- lpjGDPpc
gdp <- as.data.frame(gdp)
for(i in 1:ncol(gdp)){
  for(j in 1:nrow(gdp)){
    if(is.na(lpjGDPpc[j,i])==TRUE){
      gdp[j,i] <- "missing"
    }else if(lpjGDPpc[j,i] <= 1005){
      gdp[j,i] <- "low"
    }else if(lpjGDPpc[j,i] > 1005 && lpjGDPpc[j,i] <= 3975){
      gdp[j,i] <- "lower middle"
    }else if(lpjGDPpc[j,i] > 3975 && lpjGDPpc[j,i] <= 12275){
      gdp[j,i] <- "upper middle"
    }else if(lpjGDPpc[j,i] > 12275){
      gdp[j,i] <- "high"
    }
  }
}
# Calculate the amount of each class occuring per year
gdp_low <- vector()
gdp_lowmiddle <- vector()
gdp_highmiddle <- vector()
gdp_high <- vector()
gdp_mis <- vector()
for(i in 1:105){
  
  gdp_fac<-as.factor(gdp[,i])
  tab<-table(gdp_fac)
  tab<-as.data.frame(tab)
  
  if(length(which(tab$gdp_fac=="low"))>0){
    gdp_low<-c(gdp_low, tab[which(tab$gdp_fac=="low"),2])
  }else {gdp_low <-c(gdp_low,0)}
  
  if(length(which(tab$gdp_fac=="lower middle"))>0){
    gdp_lowmiddle<-c(gdp_lowmiddle,tab[which(tab$gdp_fac=="lower middle"),2])
  }else{gdp_lowmiddl<-c(gdp_lowmiddle,0)}
  
  if(length(which(tab$gdp_fac=="upper middle"))>0){
    gdp_highmiddle<-c(gdp_highmiddle, tab[which(tab$gdp_fac=="upper middle"),2])
  }else{gdp_highmiddle<-c(gdp_highmiddle,0)}
  
  if(length(which(tab$gdp_fac=="high"))>0){
    gdp_high<-c(gdp_high, tab[which(tab$gdp_fac=="high"),2])
  }else{gdp_high<-c(gdp_high,0)}
  
  if(length(which(tab$gdp_fac=="missing"))>0){
    gdp_mis<-c(gdp_mis,tab[which(tab$gdp_fac=="missing"),2])
  }else{gdp_mis<-c(gdp_mis,0)}
  
}

t <- (1901:2005)
ts <- cbind(t,gdp_mis,gdp_high,gdp_highmiddle,gdp_low,gdp_lowmiddle)
ts <- as.data.frame(ts)
time <- rep(t,5)
d <- c(ts$gdp_mis,ts$gdp_high,ts$gdp_highmiddle,ts$gdp_low,ts$gdp_lowmiddle)
df <- data.frame(time = time, gdp = d)
df <- df[order(df$time),]
df$class <- rep(c("gdp_mis","gdp_high","gdp_highmiddle","gdp_lowmiddle","gdp_low"),105)


png(paste0(resultFol,"figA3.png"))
p<-ggplot(df, aes(time, gdp, fill = class)) + geom_col(position = "dodge")
p <- p +geom_bar(stat = "identity", aes(fill = class))+theme_bw()+xlab("Year")+
  ylab("Count")+theme_bw()+ theme(
    axis.title.x = element_text(colour="black", size=14, face="bold"),
    axis.title.y = element_text(colour="black", size=14, face="bold"),
    axis.text.x = element_text(colour="black", size=14),
    axis.text.y = element_text(colour="black", size=14),
    legend.key.size = unit(1, 'cm'), 
    legend.key.height = unit(1, 'cm'), 
    legend.key.width = unit(1, 'cm'), 
    legend.title = element_text(size=14), 
    legend.text = element_text(size=14))+
  scale_fill_discrete(name = "Class",
                      labels = c("High", "Upper Middle", "Low", "Lower Middle", "Missing"))+
  scale_x_continuous(breaks = c(1900,1920,1950,1980,2005))+
  scale_y_continuous(breaks = c(0,20000,40000,60000,67420))

print(p)
dev.off()


# Figure A4
data = ll_test$Irrigation
range <- seq(0,1,0.01)
brks=range
mycolors <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(length(range)-1)
palette=mycolors
legendtitle = ""
png(paste0(resultFol,"figA4.png"), width = 2000, height = 1000, units = "px", res = 400, pointsize = 4)
if (!length(palette) == (length(brks) - 1)) {palette <- colorRampPalette(RColorBrewer::brewer.pal(9,palette))(length(brks) - 1)}
ires <- 2 # inverse resolution (set to 2 for 0.5*0.5 degrees)
legendticks <- seq(from = 0, to = 100, length.out = length(brks))
data[data < brks[1]] <- brks[1]
data[data > brks[length(brks)]] <- brks[length(brks)]
ra <- raster::raster(ncols = 720, nrows = 360)
range <- range(data)
ra[raster::cellFromXY(ra,cbind(lon,lat))] <-  data
extent <- raster::extent(c(-180, 180, -60, 90))
par(bty = "n", oma = c(0,0,0,0), mar = c(0,0,0,3), xpd = T)
raster::plot(ra, ext = extent, breaks = brks, col = palette, main = "",
             legend = FALSE, axes = FALSE, maxpixels = 720*360)
fields::image.plot(legend.only = TRUE, zlim = range(brks), col = palette, 
                   useRaster = FALSE, breaks = legendticks,
                   lab.breaks = round(brks,2), legend.shrink = 0.8, 
                   legend.args = list("", side = 3, font = 2, line = 1))
dev.off()

