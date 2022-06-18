install.packages("readxl")
install.packages("stringr")

library("readxl")
data <- read_excel("data_prva_uprava.xlsx")

library(tidyr)
library(stringr)

data$PHONE


phone_reparation = function(x) {
  
  x$PHONE = gsub("\\/", "",x$PHONE)
  x$PHONE = gsub(" , ", "; ", x$PHONE) 
  x$PHONE = gsub("^0", "" , x$PHONE)
i = 0
for (col in x$PHONE) {
  i = i + 1
  if (!grepl("+421 ",col, fixed = TRUE) && !is.na(col)) {
    x$PHONE[i] = gsub(" ","",paste("+421",col))
  }
}
return(x)
}

test = phone_reparation(data)
