# Create working dataframe for the lon-lat grid
# input parameters: begin and end (range: 1,...,105)
sampling_lonlat <- function(begin, end){
  
  # ==== Packages === #
  library(rccdates)
  library(data.table)
  
  dimension = vector()
  posish <- cbind(lon,lat)
  
  for(i in begin:end){
    nam <-paste("Data", i, sep="")
    dat <- irrFrac[,i]
    
    df <- cbind(rep(1900+i, length(irrFrac[,i])),
                 posish[,1], posish[,2],
                irrFrac[,i],popdens[, i],
                median_increase[, i], dis[, i],
                prec[,i],evaporation[,i], gdp[,i], Irr_Obs[,i])
    colnames(df) <- c("Time", "Longitude" , "Latitude", 
                      "Irrigation","PopDens","MedInc",
                      "Discharge","Precipitation","Evaporation","GDP", "Irr_Obs")
    
    df <- as.data.frame(df)
    assign(nam, df)
  }

  Data <-  rbindlist(mget(paste0('Data', begin:end)), use.names = TRUE)
  
  remove(dat,df)
  do.call(remove,as.list(paste0('Data', begin:end)))

  return(Data)
}


