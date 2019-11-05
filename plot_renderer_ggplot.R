##############  LOAD PARAMS FOR DEBUG MODE ####################
# load("life_expectancy_years.Rda")
# data_layer = raster("life_expectancy_years.grd")
# minVal = 0
# maxVal = 200
# bin_size = 1
# x_title = "foo"
# x_units = "goo"
# location_name = "TLV"
# xcoord = 3066644.09286927
# ycoord = 4095231.37024089
##############  LOAD PARAMS FOR DEBUG MODE ####################

plot_renderer<-function(data_layer_name,df_name,
                        minVal,maxVal,bin_size,x_title,xcoord,ycoord, plot_type, x_units, location_name){
  data_layer<-raster(data_layer_name)
  load(df_name)
  breaks = seq(minVal,maxVal,bin_size)
  df$binned_data = as.numeric(as.character(cut(df$data_layer, breaks, labels = breaks[1:length(breaks)-1])))
  aggregated_data = aggregate(df$pop, by = list(df$binned_data), sum, na.rm = TRUE)
  colnames(aggregated_data) = c("binned_data","pop")
  aggregated_data <- aggregated_data %>% mutate(frequency = pop/sum(pop)) 
  aggregated_data$ecdf <-cumsum(aggregated_data$frequency)
  
  
  point = SpatialPoints(cbind(xcoord,ycoord))
  value_at_locations = raster::extract(data_layer,point)
  if(!is.na(value_at_locations) && value_at_locations%%bin_size==0){
    value_at_locations = value_at_locations - 1*10^(-3) 
  }
  location_index_min = floor(min(value_at_locations)/bin_size)*bin_size
  location_index_max = ceiling(max(value_at_locations)/bin_size)*bin_size
  location_index = (location_index_min+location_index_max)/2
  location_ecdf = aggregated_data$ecdf[aggregated_data$binned_data ==location_index_min]
  arrow_length = max(aggregated_data$pop)/30
  arrow_pos <- 
    data.frame(x1 = location_index, x2 = location_index, 
               y1 = aggregated_data$pop[aggregated_data$binned_data ==location_index_min]+arrow_length +(arrow_length/10), 
               y2 =aggregated_data$pop[aggregated_data$binned_data ==location_index_min]+(arrow_length/10))
  
  
  if (plot_type=="Distribution"){
    
    distribution<-ggplot(df, aes(x = data_layer, weight =pop)) + 
      geom_histogram(breaks = breaks, fill = "grey45", color = "white")+
      scale_y_continuous(labels = comma) +
      xlim(aggregated_data$binned_data[1], aggregated_data$binned_data[nrow(aggregated_data)]+bin_size)+
      ggtitle(paste("# of People vs.", x_title)) +
      xlab(paste(x_title, x_units)) + ylab("# of People") +
      theme(plot.title = element_text(hjust = 0.5, size=20, family="Comic Sans MS"),text = element_text(size=18, family="Comic Sans MS")) 

    if(length(location_index) !=0 && !is.na(location_ecdf)){
      distribution<- distribution + geom_shadowtext(data = arrow_pos,aes(label="Here \n you are!", x=x1, y=y1+2*arrow_length), fontface = "bold",
                                                    colour = "dodgerblue3", size=4 ,show.legend = FALSE,inherit.aes = FALSE,bg.colour = 'white') +
        geom_segment(aes(x = x1, xend = x2, y = y1, yend = y2), colour = "dodgerblue3",data = arrow_pos, 
                     arrow=arrow(length=unit(2, "mm")),size = 2, show.legend = FALSE,inherit.aes = FALSE) +         
        labs(subtitle = paste("The people of ", location_name," are in the ",round(location_ecdf*100),"th percentile", sep = "")) +
        theme(plot.subtitle= element_text(hjust = 0.5, size=14, family="Comic Sans MS", colour = "dodgerblue3"))
    }else{
      distribution<- distribution + 
        labs(subtitle = "Sorry, for the chosen location we could not find data :(") +
        theme(plot.subtitle= element_text(hjust = 0.5, size=14, family="Comic Sans MS", colour = "dodgerblue3"))
    }
    distribution
  }
  
  
  else{
    if(!is.na(value_at_locations)){
      value_at_locations = floor(value_at_locations)
      if (value_at_locations != maxVal && value_at_locations != minVal){
        raster_breaks = c(minValue(data_layer),location_index_min,location_index_max,maxValue(data_layer))
        colors = c("grey","blue","grey")
      }
      else if (value_at_locations<=maxVal && value_at_locations>=maxVal-bin_size){
        raster_breaks = c(minVal,location_index_min,maxVal)
        colors = c("grey","blue")
      }
      else if (value_at_locations>=minVal && value_at_locations<=minVal+bin_size){
        breaks = c(minVal,location_index_max,maxVal)
        colors = c("blue","grey")
      }
      plot(data_layer,breaks = raster_breaks,col = colors, legend = FALSE, box = FALSE,xaxt='n', yaxt='n')
      title(main=paste("Where can you find the same \n ", x_title))
    }
    else{
      plot(data_layer, col = c("grey"),legend = FALSE, box = FALSE,xaxt='n', yaxt='n')
      title(main="Sorry, for the chosen location we could not find data :(")
    }
  }
}
##############  ALTERNATIVE LOCATIONS FOR DEBUG MODE ####################

# location_name = "NYC"
# xcoord = -6250939.011313787
# ycoord = 5098006.14796643

# location_name = "Moscow"
# xcoord = 2868867.56913903
# ycoord = 6621961.30320268

# location_name = "Katzrin"
# xcoord = 3133972.72265642
# ycoord = 4202852.03655902

##############  ALTERNATIVE LOCATIONS FOR DEBUG MODE ####################

