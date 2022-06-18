library(readxl)
library(tidyr)
library(stringr)
data1 <- read_excel("data_eo.xlsx")

SplitColumn <- function(x){
  #data2 <- separate(data1,col = BUSINESS_ACTIVITY,into = c("A", "B"),sep = "[+:•]")
  data2 <- separate(x,col = BUSINESS_ACTIVITY,into = c("A", "B"),sep = "ponuka:")
  oblast2 <- str_remove_all(data2$A, "Ponuka")
  oblast2 <- str_remove_all(oblast2, "ponuka")
  oblast2 <- str_remove_all(oblast2, "[.+-]")
  oblast2 <- str_remove_all(oblast2, "[•]")
  oblast2 <- gsub("[/,\n]",";", oblast2)
  oblast2
  
  #ponuka2 <- str_remove_all(data2$B, "[-()]")
  
  data2$Oblasť <- oblast2
  data2$Ponuka <- data2$B
  data2$Oblasť[is.na(data2$Oblasť)] <- "Oblasť nie je bližšie špecifikovaná"
  data2$Ponuka[is.na(data2$Ponuka)] <- "Ponuka nie je bližšie špecifikovaná"
  
  data2$A <- NULL
  data2$B <- NULL
  
  return(data2)
}

test <- SplitColumn(data1)
