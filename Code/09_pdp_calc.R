#Function to predict irrigation status 0 = no, 1 = yes
rf_predict <- function(object,newdata){
  pred_rf4 <- predict(object, newdata)
  mean_pred <- mean(pred_rf4$predictions[,2])
  pred <- rep(0, nrow(pred_rf4$predictions))
  pred[which(pred_rf4$predictions[,2]>mean_pred)] <- 1
  return(mean(pred))
}


train_data <- read.table(file=paste0(resultFol,"sample_class_isea3h.txt"),header = TRUE,sep =",")


#Calculate partial dependence
#input: model (ranger object)
pdp_calc <- function(model){
  if(model$treetype == "Regression"){
    prf_popdens <<- partial(model,pred.var = "PopDens", 
                            prob = F,
                            grid.resolution = 100,
                            smooth = LOESS)
    prf_discharge <<- partial(model,pred.var = "Discharge", 
                              prob = F,
                              grid.resolution = 100,
                              smooth = LOESS)
    prf_evap <<- partial(model,pred.var = "Evaporation", 
                            prob = F,
                            grid.resolution = 100,
                            smooth = LOESS)
    prf_prec <<- partial(model,pred.var = "Precipitation", 
                            prob = F,
                            grid.resolution = 100,
                            smooth = LOESS)
    prf_medinc <<- partial(model,pred.var = "MedInc", 
                            prob = F,
                            grid.resolution = 100,
                            smooth = LOESS)
    prf_gdp <<- partial(model,pred.var = "GDP", 
                            prob = T,
                            grid.resolution = 100,
                            smooth = LOESS)
    K <- cbind(prf_popdens,prf_discharge,prf_evap,prf_prec,prf_medinc,prf_gdp)
    write.table(K,paste0(resultFol,"reg_pdp.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
    
  }
  else if(model$treetype == "Probability estimation"){
    prf_popdens <<- partial(model,pred.var = "PopDens", 
                            prob = T,
                            grid.resolution = 100,
                            smooth = LOESS,
                            pred.fun = rf_predict)
    prf_discharge <<- partial(model,pred.var = "Discharge", 
                            prob = T,
                            grid.resolution = 100,
                            smooth = LOESS,
                            pred.fun = rf_predict)
    prf_evap <<- partial(model,pred.var = "Evaporation", 
                         prob = T,
                         grid.resolution = 100,
                         smooth = LOESS,
                         pred.fun = rf_predict)
    prf_prec <<- partial(model,pred.var = "Precipitation", 
                         prob = T,
                         grid.resolution = 100,
                         smooth = LOESS,
                         pred.fun = rf_predict)
    prf_medinc <<- partial(model,pred.var = "MedInc", 
                           prob = T,
                           grid.resolution = 100,
                           smooth = LOESS,
                           pred.fun = rf_predict)
    prf_gdp <<- partial(model,pred.var = "GDP", 
                        prob = T,
                        grid.resolution = 100,
                        smooth = LOESS,
                        pred.fun = rf_predict)
    K <- cbind(prf_popdens,prf_discharge,prf_evap,prf_prec,prf_medinc,prf_gdp)
    write.table(K,paste0(resultFol,"class_pdp_ll_area.txt"),col.names = TRUE,sep = ",",row.names = FALSE)
  }
pdp_rf <- cbind(prf_medinc,prf_discharge,prf_evap,prf_gdp,prf_popdens,prf_prec)
return(pdp_rf)
}



