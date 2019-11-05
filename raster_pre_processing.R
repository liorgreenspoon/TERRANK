
libraries = c("raster","scales","maps","sp","plyr","snow","rgdal","zoom","ggvoronoi","gdalUtils","data.table")
for (lib in libraries){
  if (lib %in% rownames(installed.packages())==FALSE){install.packages(lib)}
  library(lib,character.only = TRUE)}

setwd("/home/dan/Desktop/Lior's/dev/shinyApp")
pop = raster("ackert4_pop.tif")
#year = "2010"
#data_layer  = raster(paste("UnitedStatesStates-",year,"-UNEMPLOY.tiff",sep = ""))
data_layer = raster("Max Temperature of Warmest Month.tif")
projected_layer = projectRaster(from = data_layer, to = pop)
#writeRaster(projected_layer, filename = paste("projected_UNEMPLOY_",year,".grd",sep =""), format = "raster",overwrite = TRUE)
writeRaster(projected_layer, filename = "max_temp.grd", format = "raster",overwrite = TRUE)

data_layer<-projected_layer
#data_layer = crop(data_layer,pop)
#pop = crop(pop,data_layer)
two_band_raster = stack(pop, data_layer)
names(two_band_raster) = c("pop", "data_layer")
df = as.data.frame(two_band_raster)
df = df[complete.cases(df), ]
save(df,file="max_temp.Rda")

### VALIDATION ###
breaks = seq(-50,50,1)
df$binned_data = cut(df$data_layer, breaks)
aggregated_data = aggregate(df[, "pop", drop = FALSE], df[, "binned_data", drop = FALSE], sum ,na.rm = FALSE)
barplot(aggregated_data$pop,names.arg = aggregated_data$binned_data, axes = FALSE)
point = SpatialPoints(cbind(3066644.09286927,4095231.37024089))

value_at_locations = extract(data_layer,point)
