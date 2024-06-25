# prepares the working dataframes
# input: iFol = input folder path and oFol = output folder path
preparation <- function(iFol,oFol){
  
  Sys.setlocale('LC_ALL','C')
  options(scipen = 999)
  
  # ===== Settings ====== #
  ncells=67420
  plotting=T
  
  # ===== Loading the data into the environment ===== #
  load(file=paste0(iFol,"data.RData"))
  
  load(file=paste0(iFol,"countryData_67420.RData"))
  
  
  # ===== Prepare Data Set ========= #
  ### Calculate the cell area for each grid cell
  cellarea <<- (111194.9/2)*(111194.9/2)*cos(lat/180*pi) # cellarea in m2
  
  ### Sum discharge to yearly values
  dis <<- apply(discharge_landuse,c(1,3),sum)
  
  ### Sum precipitation to yearly values 
  prec <<- apply(prec_landuse,c(1,3),sum)
  
  ### Sum evaporation and transpiration to yearly values
  evap <<- apply(evap_landuse,c(1,3),sum)
  interec <<- apply(interc_landuse,c(1,3),sum)

  ### Sum evaporation
  evaporation <<- evap + interec 
  
  remove(evap,interec,green,blue,evap_landuse,prec_landuse,discharge_landuse, 
         transp_green_landuse,transp_blue_landuse,interc_landuse, runoff_landuse)

  ### Recode GDP per capita into a class variable
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
  
  
  
  ### Create matrix with dummies for irrigation (yes = 1)
  Irr_Obs <<- matrix(0,67420,105)
    for(j in 1:ncol(irrFrac)){
      Irr_Obs[which(irrFrac[,j] != 0)] <- 1
    }

  
  # ==== Save the obtained data set =======#
  save.image(paste0(oFol,"final_data.RData"))
}



  preparation(iFol, oFol)
  load(paste0(oFol,"final_data.RData"))
  
  ### Set up training, validation and test dataframes for the lon-lat grid
  ll_train <- sampling_lonlat(2, 99)
  ll_train$Irrigation <- as.numeric(ll_train$Irrigation)
  ll_train$PopDens <- as.numeric(ll_train$PopDens)
  ll_train$Discharge <- as.numeric(ll_train$Discharge)
  ll_train$Evaporation <- as.numeric(ll_train$Evaporation)
  ll_train$Precipitation <- as.numeric(ll_train$Precipitation)
  ll_train$MedInc <- as.numeric(ll_train$MedInc)
  
  ll_val <- sampling_lonlat(101, 105)
  ll_val$Irrigation <- as.numeric(ll_val$Irrigation)
  ll_val$PopDens <- as.numeric(ll_val$PopDens)
  ll_val$Discharge <- as.numeric(ll_val$Discharge)
  ll_val$Evaporation <- as.numeric(ll_val$Evaporation)
  ll_val$Precipitation <- as.numeric(ll_val$Precipitation)
  ll_val$MedInc <- as.numeric(ll_val$MedInc)
  
  ll_test <- sampling_lonlat(100, 100)
  ll_test$Irrigation <- as.numeric(ll_test$Irrigation)
  ll_test$PopDens <- as.numeric(ll_test$PopDens)
  ll_test$Discharge <- as.numeric(ll_test$Discharge)
  ll_test$Evaporation <- as.numeric(ll_test$Evaporation)
  ll_test$Precipitation <- as.numeric(ll_test$Precipitation)
  ll_test$MedInc <- as.numeric(ll_test$MedInc)
  
  write.table(ll_train,paste0(oFol,"ll_train.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  write.table(ll_val,paste0(oFol,"ll_val.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  write.table(ll_test,paste0(oFol,"ll_test.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
  ### Set up training, validation and test dataframes for the isea3h grid system
  data <- dggridR_sampling(7)
  #data <- dggridR_sampling(8)
  #data <- dggridR_sampling(9)
  
  dgg_train <- data[-(which(data$Year == 1901 |data$Year == 2000 | data$Year == 2001 | data$Year == 2002 | data$Year == 2003 | data$Year == 2004)),]
  dgg_train <- as.data.frame(dgg_train)
  dgg_train$Irrigation <- as.numeric(dgg_train$Irrigation)
  dgg_train$PopDens <- as.numeric(dgg_train$PopDens)
  dgg_train$Discharge <- as.numeric(dgg_train$Discharge)
  dgg_train$Evaporation <- as.numeric(dgg_train$Evaporation)
  dgg_train$Precipitation <- as.numeric(dgg_train$Precipitation)
  dgg_train$MedInc <- as.numeric(dgg_train$MedInc)
  
  dgg_test <- data[which(data$Year == 2000),]
  dgg_test$Irrigation <- as.numeric(dgg_test$Irrigation)
  dgg_test$PopDens <- as.numeric(dgg_test$PopDens)
  dgg_test$Discharge <- as.numeric(dgg_test$Discharge)
  dgg_test$Evaporation <- as.numeric(dgg_test$Evaporation)
  dgg_test$Precipitation <- as.numeric(dgg_test$Precipitation)
  dgg_test$MedInc <- as.numeric(dgg_test$MedInc)
  
  dgg_val <- data[which(data$Year == 2001 | data$Year == 2002 | data$Year == 2003 | data$Year == 2004),]
  dgg_val$Irrigation <- as.numeric(dgg_val$Irrigation)
  dgg_val$PopDens <- as.numeric(dgg_val$PopDens)
  dgg_val$Discharge <- as.numeric(dgg_val$Discharge)
  dgg_val$Evaporation <- as.numeric(dgg_val$Evaporation)
  dgg_val$Precipitation <- as.numeric(dgg_val$Precipitation)
  dgg_val$MedInc <- as.numeric(dgg_val$MedInc)
  
  write.table(dgg_train,paste0(oFol,"dgg_train7.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  write.table(dgg_val,paste0(oFol,"dgg_val7.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  write.table(dgg_test,paste0(oFol,"dgg_test7.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
  write.table(dgg_train,paste0(oFol,"dgg_train8.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  write.table(dgg_val,paste0(oFol,"dgg_val8.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  write.table(dgg_test,paste0(oFol,"dgg_test8.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
  write.table(dgg_train,paste0(oFol,"dgg_train9.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  write.table(dgg_val,paste0(oFol,"dgg_val9.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  write.table(dgg_test,paste0(oFol,"dgg_test9.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  
  
  
