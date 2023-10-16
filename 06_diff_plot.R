####
library(ggplot2)
library(RColorBrewer)
library(ggforce)
library(pmr)
#final data has to be loaded 

# Figure 4a
data = diff_pred
range <- seq(-0.5,0.5,0.05)
brks=range
mycolors <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(length(range)-1)
palette=mycolors
legendtitle = ""
png(paste0(resultFol,"fig04a.png"), width = 2000, height = 1000, units = "px", res = 400, pointsize = 4)
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
#maps::map('world', add = TRUE, res = 0, lwd = 0.1, ylim = c(-60,90))
dev.off()


# Figure 4b
dgg_test$diff <- as.numeric(diff_dgg)
range <- seq(-0.5,0.5,0.05)
grid <- dgcellstogrid(dggs,dgg_test$CellID,frame=TRUE,wrapcells=TRUE)
grid <- merge(grid,dgg_test,by.x="cell",by.y="CellID")

mycolors <- colorRampPalette(rev(brewer.pal(11, "Spectral")))(length(range)-1)
png(paste0(resultFol,"fig04b.png"),width = 2000, height = 1000, units="px",res=400,pointsize = 4)
ggplot(data = grid) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=diff)) +
  scale_size_binned(breaks = range, guide = guide_bins(show.limits = T)) +
  theme_void() +
  binned_scale(aesthetics = "fill", scale_name = "custom", 
               palette = ggplot2:::binned_pal(scales::manual_pal(values = mycolors)),
               guide = "bins",
               breaks = range, limits = c(-0.5, 0.5), show.limits = T)+
  theme(legend.position = "none")
dev.off()




  