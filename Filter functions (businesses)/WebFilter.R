library(readxl)
data = read_excel("data_eo.xlsx")

filterByWeb <- function(data, value) {
  if(value){
    return(data[!is.na(data$WEBSITE),])
  }
  return(data[is.na(data$WEBSITE),]) 
}

data_ma_web = filterByWeb(data, TRUE)
data_nema_web = filterByWeb(data, FALSE)
