install.packages("readxl")
library("readxl")
data <- read_excel("data_prva_uprava.xlsx")
dara1 = data.frame(data)
library(tidyr)

#for(i in dara1$EMAIL) {
  
#  i=i+1
#}


###toto funguje
#skuska="stefania.neuvirthova@demax.sk;. frantisek.kovacik@demax.sk"
#skuska
#toto = unlist(regmatches(dara1$EMAIL, gregexpr("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", dara1$EMAIL)))
#toto

install.packages("stringr")
library(stringr)
#hotovo = sapply(str_extract_all(dara1$EMAIL,"[a-z_+0-9]+\\@\\w+\\.[a-z]{2,4}"), function(x){ paste(x,collapse = " ")})
#hotovo

#for(row in dara1$EMAIL){
#  sapply(str_extract_all(dara1$EMAIL[i],"[a-z_+0-9]+\\@\\w+\\.[a-z]{2,4}"), function(x){ paste(x,collapse = " ")})
#  i=1+1
#}

#data$EMAIL =  sapply(str_extract_all(dara1$EMAIL,"[a-z_+0-9]+\\@\\w+\\.[a-z]{2,4}"), function(x){ paste(x,collapse = " ")})

EmailSplit <- function(x){
  adresa <-x$EMAILS
  
  x$EMAIL =  sapply(str_extract_all(dara1$EMAIL,"[a-z_+0-9]+\\@\\w+\\.[a-z]{2,4}"), function(x){ paste(x,collapse = " ")})
  return(x)
}

