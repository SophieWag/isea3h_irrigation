#plots the centercount in the isea3h grid

  #Resolution 7 
  spacing = 100
 
  dggs <- dggridR::dgconstruct(spacing=spacing, metric=FALSE, resround='nearest')
  Test_Data <- cbind(lon, lat, irrFrac[,100])
  Test_Data <- as.data.frame(Test_Data)
  Test_Data$cell <- dgGEO_to_SEQNUM(dggs,Test_Data$lon,Test_Data$lat)$seqnum
  cellcenters <- dgSEQNUM_to_GEO(dggs,Test_Data$cell)
  cellcounts <- Test_Data %>% group_by(cell) %>% summarise(count=n())
  grid <- dgcellstogrid(dggs,Test_Data$cell,frame=TRUE,wrapcells=TRUE)
  grid <- merge(grid,cellcounts,by.x="cell",by.y="cell")
  countries <- map_data("world")
  

  #png(paste0(resultFol,"centercount",res,".png"),width = 4000, height = 3000, units="px",res=400,pointsize = 4)
  p<- ggplot() + 
    geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color = "black")   +
    geom_polygon(data=grid,      aes(x=long, y=lat, group=group, fill=count), alpha=5)    +
    geom_path   (data=grid,      aes(x=long, y=lat, group=group), alpha=0.2, color="white") +
    scale_fill_gradient(low="blue", high="red")+labs(fill="Centercount")
# print(p)    
# dev.off()
  
 png(paste0(resultFol,"fig01",res,".png"),width = 4000, height = 2000, units="px",res=400,pointsize = 4)
  p <- p+coord_map("ortho", orientation = c(24, 10, 0))+
    xlab('')+ylab('')+
    theme(axis.ticks.x=element_blank())+
    theme(axis.ticks.y=element_blank())+
    theme(axis.text.x=element_blank())+
    theme(axis.text.y=element_blank())+
    ggtitle('')
 print(p)  
 dev.off()
