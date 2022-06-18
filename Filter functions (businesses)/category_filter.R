
data = read_excel("Datasets_data.xlsx")

#Catering a stravovanie		[catering] 
#Tlačiarenské a grafické služby	[tlac] 
#Reklama, marketing, web, IT		[reklama] 
#Výroba, montáž, stavebné práce	[stavebne] 
#Vzdelávanie a školenie		[vzdelavanie] 
#Upratovacie práce a služby, čistiareň	[upratovanie] 
#Údržba zelene, technické služby	[zelen] 
#Obchod				[obchod] 
#Odpady a recyklácia			[odpad] 
#Ostatné služby (prekladateľstvo, účtovníctvo)	[ostatne] 
#Potravinárska výroba			[potraviny] 
#Propagačné a darčekové predmety, remeselná výroba	[suveniry] 
#Textilná výroba, šitie, dekorácie	[textil] 
#Iné					[ine] 
#
install.packages("tidyverse",type = "source")
library(tidyverse)


categoryFilter <- function(data, category){
  
  df=data.frame(matrix(ncol=ncol(data),nrow = 0))
  colnames(df) = colnames(data)
  
  for (row in data['BUSINESS_CAT']){
    split = strsplit(row,";")
  }
  
  pocet = 0
  
  
  for(y in category){
    
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

#priklady
stavebné_práce = categoryFilter(data,c("stavebné práce","potravinárska výroba"))
obchod = categoryFilter(data,"obchod")
