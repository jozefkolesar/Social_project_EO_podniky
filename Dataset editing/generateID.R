# This function is for generating unique id for every row of dataframe
# ID column will be placed at first position of data frame

generateID <- function(data){
  
  data$ID <- 1:nrow(data)
  if(colnames(data[1])!="ID")
    data <- data[,c(ncol(data),1:ncol(data)-1)]
  return(data)
}
