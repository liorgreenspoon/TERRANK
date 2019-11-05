plot_renderer<-function(data_layer_name,df_name,
                                minVal,maxVal,bin_size,x_title,xcoord,ycoord, plot_type, x_units, location_name){
  data_layer<-raster(data_layer_name)
  load(df_name)
  breaks = seq(minVal,maxVal,bin_size)
  #breaks_df = as.data.frame(breaks)
  df$binned_data = as.numeric(as.character(cut(df$data_layer, breaks, labels = breaks[1:length(breaks)-1])))
  aggregated_data = aggregate.data.frame(df[, "pop", drop = FALSE], df[, "binned_data", drop = FALSE], sum, na.rm = TRUE)
  #aggregated_data = aggregate(df$pop, by = list(df$binned_data), sum, na.rm = TRUE)
  colnames(aggregated_data) = c("binned_data","pop")
  aggregated_data <- aggregated_data %>% mutate(frequency = pop/sum(pop)) 
  aggregated_data$ecdf <-cumsum(aggregated_data$frequency)
  
  #weighted_ecdf<-wtd.Ecdf(as.vector(aggregated_data$binned_data),weights = as.vector(aggregated_data$pop))
  #aggregated_data$ecdf = weighted_ecdf$ecdf[2:length(weighted_ecdf$ecdf)]
  
  #aggregated_data = merge(aggregated_data, breaks_df, by.x = "binned_data",by.y = "breaks",all = TRUE)
  point = SpatialPoints(cbind(xcoord,ycoord))
  value_at_locations = extract(data_layer,point)
  location_index = NA
  location_index = which(aggregated_data$binned_data==(value_at_locations-value_at_locations%%bin_size))
  if (length(location_index)==0){
    location_index = which(aggregated_data$binned_data==(value_at_locations-value_at_locations%%bin_size-bin_size))
  }
  aggregated_data$color = "grey"
  if(!is.na(location_index) && length(location_index) !=0){
    aggregated_data$color[location_index] = "blue"
  }
  if (plot_type=="Distribution"){
    weighted.hist(aggregated_data$binned_data,aggregated_data$pop,breaks = breaks,
                  col = aggregated_data$color,axes = FALSE, ylab ="")
    percentile = NA
    if (length(location_index) !=0 && !is.na(aggregated_data$ecdf[location_index])){
      percentile = paste(location_name," is in the ",round(aggregated_data$ecdf[location_index]*100),"th percentile", sep = "")
      sub = percentile
    }
    else{
      msg = "Sorry, for the chosen location we could not find data."
      sub = msg
    }
    eaxis(side = 2)
    title(main=paste("# of people vs.", x_title) ,xlab=paste(x_title, x_units), ylab="# of people",
          col = aggregated_data$color,
          mtext(sub, col = "blue"))
  }else{
    if(!is.na(value_at_locations)){
      value_at_locations = floor(value_at_locations)
      if (value_at_locations != maxVal && value_at_locations != minVal){
        breaks = c(minValue(data_layer),(value_at_locations-value_at_locations%%bin_size),(value_at_locations-value_at_locations%%bin_size+bin_size),maxValue(data_layer))
        plot(data_layer,breaks = breaks,col = c("grey","blue","grey"), legend = FALSE, box = FALSE,xaxt='n', yaxt='n')
      }
      else if (value_at_locations<=maxVal && value_at_locations>=maxVal-bin_size){
        breaks = c(minVal,maxVal-bin_size,maxVal)
        plot(data_layer,breaks = breaks,col = c("grey","blue"), legend = FALSE, box = FALSE,xaxt='n', yaxt='n')
      }
      else if (value_at_locations>=minVal && value_at_locations<=minVal+bin_size){
        breaks = c(minVal,minVal+bin_size,maxVal)
        plot(data_layer,breaks = breaks,col = c("blue","grey"), legend = FALSE, box = FALSE,xaxt='n', yaxt='n')
      }
      title(main=paste("Where can you find the same", x_title))
    }
    else{
      plot(data_layer, col = c("grey"),legend = FALSE, box = FALSE,xaxt='n', yaxt='n')
      title(main="Sorry, for the chosen location we could not find data.")
    }
  }
}



