
data = Social_Enterprises_Data



addressSplit <- function(x)
{
  adresa <-x$ADDRESS
  x$Ulica = NA
  x$Cislo = NA
  #print(test)
  i = 0
  for (row in x$ADDRESS ) {
    i= i+1
    if(grepl(",", row, fixed = TRUE)){
      print(row)
      x$ADDRESS[i]<- gsub(", ", "", row)
      print(x$ADDRESS[i])
    }
    
    zaznam = strsplit(x$ADDRESS[i], " ")
    zaznam2 = zaznam[[1]][-length(zaznam[[1]])]
    x$Ulica[i] = paste(unlist(zaznam2), collapse = " ")
    x$Cislo[i] = zaznam[[1]][length(zaznam[[1]])
  }
  return(x)
}


test <- addressSplit(data)
View(test)

