

setwd("/home/dan/Desktop/Lior's/dev/shinyApp")
data_layer_name = "Yearly average of daily totals of potential photovoltaic electricity production.tif"
pop_layer_name = "ackert4_pop.tif"

pop <- raster(pop_layer_name)
data_layer<-raster(data_layer_name)

data_layer = projectRaster(from = data_layer, to = pop)

data_layer = crop(data_layer,pop)
pop = crop(pop,data_layer)
two_band_raster = stack(pop, data_layer)
names(two_band_raster) = c("pop", "data_layer")
df = as.data.frame(two_band_raster)
df = df[complete.cases(df), ]
save(df,file="photovoltaic.Rda")
writeRaster(data_layer, "photovoltaic.grd")

point = SpatialPoints(cbind(-6255583.39889796,5087820.73981720))
value_at_locations = extract(data_layer,point)
