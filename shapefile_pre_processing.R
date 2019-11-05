setwd("/home/dan/Desktop/Lior's/dev/shinyApp")
libraries = c("raster","scales","maps","sp","plotKML","plyr","snow","rgdal","zoom","rgeos","pracma","rgeos", "maptools")
for (lib in libraries){
  if (lib %in% rownames(installed.packages())==FALSE){install.packages(lib)}
  library(lib,character.only = TRUE)
}
crs_dest<-raster("ackert4_pop.tif")
raster_dest = raster("Average_precipitation_30_sec.tif")
data_layer<-readOGR(dsn = "/home/dan/Desktop/Lior's/dev/shinyApp/World_Bank__Access_to_Clean_Cooking_Fuel_%25_of_Population",
                 layer = "World_Bank__Access_to_Clean_Cooking_Fuel_%_of_Population")


# data_layer<-spTransform(data_layer, crs_dest)
data_layer_raster<-rasterize(data_layer,raster_dest,field = data_layer@data$CleanFue14)
data_layer_raster_projected<-projectRaster(data_layer_raster,crs_dest)

writeRaster(data_layer_raster_projected,"Access_to_Clean_Cooking_Fuel.grd")

df <- data.frame(matrix(ncol = 3, nrow = 248)) 
colnames(df)<-c("data_layer","pop","binned_data")
df$data_layer<-data_layer@data$CleanFue14
df$pop<-(as.numeric(as.character(data_layer@data$Countrie_4)))
save(df,file="Access_to_Clean_Cooking_Fuel.Rda")



# #### TEST OUTPUT ###########
# breaks = seq(0,100,1)
# df$binned_data = cut(df$data_layer, breaks, labels = FALSE, include.lowest = TRUE)
# aggregated_data = aggregate(df[, "pop", drop = FALSE], df[, "binned_data", drop = FALSE], sum)
# 
# weighted_ecdf<-wtd.Ecdf(as.vector(aggregated_data$binned_data),weights = as.vector(aggregated_data$pop))
# aggregated_data$ecdf = weighted_ecdf$ecdf[2:length(weighted_ecdf$ecdf)]
# 
# diff<-as.data.frame(setdiff(breaks,as.vector(aggregated_data$binned_data)))
# colnames(diff)<-c("binned_data")
# aggregated_data<-merge(aggregated_data,diff, by = "binned_data",all = TRUE)
# barplot(aggregated_data$pop,names.arg = aggregated_data$binned_data)
# # 9958951.08937805
point = SpatialPoints(cbind(9958951.08937805,1415727.60862710)) ##Ho Chi Minh
value_at_locations = extract(data_layer_raster_projected,point)
location_index = ceiling(value_at_locations/bin_size)-ceiling(minVal/bin_size)
