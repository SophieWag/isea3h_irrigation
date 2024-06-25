# Create working dataframe for the isea3h grid
# input parameters: res (works with resolution 7)
dggridR_sampling <- function(res){
  
  # ==== Packages === #
  library(dggridR)
  
  for(t in 1:105){
    Dgrid <- paste("DGrid", t, sep = "")
    
    Test_Data <- cbind(lon, lat, irrFrac[,t])
    Test_Data <- as.data.frame(Test_Data)
    
    #Project data to hexagonal cells
    dggs  <- dggridR::dgconstruct(res = res, metric=FALSE, resround='nearest')
    
    #Get the cell centers for the data that the observations point would fall into
    Test_Data$cell <- dggridR::dgGEO_to_SEQNUM(dggs,Test_Data$lon,Test_Data$lat)$seqnum
    cellcenters   <- dggridR::dgSEQNUM_to_GEO(dggs,Test_Data$cell)
    
    zellen <- as.numeric(as.character(Test_Data$cell))
    zell <- zellen[!duplicated(zellen)]
    
    #Get the grid cell boundaries
    grid    <- dggridR::dgcellstogrid(dggs,zell,frame=TRUE,wrapcells=TRUE)
    
    ### calculate the position of the grid cells for each sampled hexagon-cell
    cid <- vector()
    zell_pos <- vector()
    for(k in 1:length(zell)){
      #Take the first entry of the unique list of grid cells (zell) and see where
      #it appears in the list of all grid cells (zellen)
      len <- length(which(zellen == zell[k]))
      
      #Create a vector with the grid cell ids and its number of appearance
      cid <- c(cid, rep(zell[k], len))
      
      #Create a vector with the grid cell positions of the grid cell ids
      zell_pos <- c(zell_pos, which(zellen == zell[k]))
    }
    
    #Create dataframe with the grid cell ids and their position in the original dataset
    M <- cbind(cid, zell_pos)
    time <- rep(t+1900,length(zell))
    
    ### create dataframe for year t
    Dat <- cbind(Test_Data$cell,lon,lat,irrFrac[,t],popdens[,t],median_increase[,t],prec[,t],dis[,t],evaporation[,t],gdp[,t])
    Dat <- as.data.frame(Dat)
    colnames(Dat) <- c("Cell","Longitude","Latitude","Irrigation","PopDens","MedInc","Precipitation","Discharge","Evaporation","GDP")
    Dat$Irrigation <- as.numeric(Dat$Irrigation)
    Dat$PopDens<- as.numeric(Dat$PopDens)
    Dat$MedInc <- as.numeric(Dat$MedInc)
    Dat$Precipitation <- as.numeric(Dat$Precipitation)
    Dat$Discharge<- as.numeric(Dat$Discharge)
    Dat$Evaporation <- as.numeric(Dat$Evaporation)
    
    meanval <- vector()
    cid2 <- vector()
    popcell <- vector()
    distcell <- vector()
    evacell <- vector()
    medcell <- vector()
    preccell <- vector()
    dischargecell <- vector()
    gdp_cell <- vector()
    for(i in 1:length(zell)){
      cid2 <- c(cid2, zell[i])
      meanval <- c(meanval, mean(Dat$Irrigation[which(zellen == zell[i])]))
      popcell <- c(popcell, mean(Dat$PopDens[which(zellen == zell[i])]))
      medcell <- c(medcell, mean(Dat$MedInc[which(zellen == zell[i])]))
      preccell <- c(preccell, mean(Dat$Prec[which(zellen == zell[i])]))
      dischargecell <- c(dischargecell, mean(Dat$Discharge[which(zellen == zell[i])]))
      evacell <- c(evacell, mean(Dat$Evaporation[which(zellen == zell[i])]))
      gdp_cell <- c(gdp_cell, names(which.max(table(Dat$GDP[which(zellen == zell[i])]))))
    }
    
    
    K <- cbind(cid2,meanval,popcell,distcell,medcell,preccell,dischargecell,evacell,time,gdp_cell)
    K <- as.data.frame(K)
    assign(Dgrid, K)
  }
  
 
  
  Hexgrid <- do.call(rbind, mget(paste0('DGrid', 1:105)))
  colnames(Hexgrid) <- c("CellID","Irrigation","PopDens","MedInc","Precipitation","Discharge","Evaporation","Year","GDP")

  Hexgrid$Irr_Obs <- 0
  Hexgrid$Irr_obs[which(Hexgrid$Irrigation != 0)] <- 1
  
  rm(list = paste0('DGrid',1:105))
  return(Hexgrid)
}


