auto_histogram <- function(data){
  numeric_data <- Filter(is.numeric, data)
  histograms <- list()

  for(i in 1:ncol(numeric_data)){
    histograms <- append(histograms, histo_chart(numeric_data[,i]))
  }
  names(histograms) <- colnames(numeric_data)
  join_charts(list_of_plots = histograms)
}


histo_chart <- function(vector){
  hist_data <- graphics::hist(vector, plot = F)
  breaks <- get_hist_breaks(hist_data)
  counts <- hist_data$counts
  data <- data.frame(cbind(breaks, counts))
  data[,'counts'] <- as.numeric(data[,'counts'])
  column_chart(data, x = 'breaks', series = 'counts')
}


get_hist_breaks <- function(hist_data){
  paste(hist_data$breaks[1:(length(hist_data$breaks) - 1)], hist_data$breaks[2:length(hist_data$breaks)], sep='-')
}
