
install.packages("tidyverse",type = "source")
library(tidyverse)


regionFilter <- function(data, region){
  
  df=data.frame(matrix(ncol=ncol(data),nrow = 0))
  colnames(df) = colnames(data)
  
  for (row in data['REGION_ACTIVITY']){
    split = strsplit(row,";")
  }
  
  pocet = 0
  
  
  for(y in region){
    
    for (x in 1:nrow(data)){
      if( split[[x]][1] == y){
        #print(split[[x]])
        df =rbind(df,data[x,])
        pocet = pocet + 1
      }
      if (length(split[[x]]) == 2 ){
        if( gsub(" ", "",split[[x]][2]) == gsub(" ","",y)){
          # print(split[[x]])
          df =rbind(df,data[x,])
          pocet = pocet + 1
        }
        
      }
    }
    
  }
  
  print(pocet)
  
  df = df %>% distinct()
  df
  
}

test = regionFilter(data,c("Prešovský kraj","Trenčiansky kraj"))