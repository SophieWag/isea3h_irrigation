#creates Tables A3 and A4
ll_train <- read.table(file=paste0(oFol,"ll_train.txt"),header = TRUE,sep =",")
ll_train<- as.data.frame(ll_train)

#Table 3
# Pearson Correlation Coefficient 
corramat <- cbind(ll_train$PopDens,ll_train$MedInc, 
                  ll_train$Discharge,ll_train$Evaporation,ll_train$Precipitation)
correlations <- matrix(, nrow = 5, ncol= 5)
  
for(i in 1:5){
  for(j in 1:5){
    correlations[i,j] = cor(corramat[,i], corramat[,j], method = "pearson")
  }
}
colnames(correlations) <- c("Population Density",
                            "Median Increase","Discharge",
                            "Evaporation","Precipitation")
rownames(correlations) <- c("Population Density",
                            "Median Increase","Discharge",
                            "Evaporation","Precipitation")
write.table(correlations,paste0(resultFol,"tabA3.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
#Table A4
#Variance Inflation Factor

tab <- VIF(lm(Irrigation ~  PopDens  + MedInc + Discharge  + Precipitation + Evaporation, data = ll_train))
tab <- as.data.frame(tab)
rownames(tab) <- c("Population Density","Median Increase",
                    "Discharge","Precipitation","Evaporation")
colnames(tab) <- "VIF"
write.table(tab,paste0(resultFol,"tabA4.txt"),col.names = TRUE,sep = ",",row.names = FALSE)

  
  

