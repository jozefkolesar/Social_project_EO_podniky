library(stringr)
library(tidyverse)

data = data
head(data)


district_filter = function(data,parameter) {
  
  for (row in data["DISTRICT_ACTIVITY"]) {
    split = strsplit(row,";")
  }
  
  df=data.frame(matrix(ncol = ncol(data), nrow = 0))
  colnames(df) = colnames(data)
  
  for(premenna in parameter) {
    for (x in 1:nrow(data)) {
      if (length(split[[x]]) > 1 && !is.na(data["DISTRICT_ACTIVITY"][x,])) {
        for (y in 1:length(split[[x]])) {
          if (str_trim(split[[x]][y]) == premenna) {
            df = rbind(df,data[x,])
          }
        }
      }
      if (str_trim(split[[x]][1]) == premenna && !is.na(data["DISTRICT_ACTIVITY"][x,])) {
        df = rbind(df,data[x,])
      }
    }
  }
  
  df = df %>% distinct()
  return(df)
}

takticka_sluska = district_filter(data, c("Spišská Nová Ves","Gelnica","Levice"))
