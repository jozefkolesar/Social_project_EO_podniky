data = Social_Enterprises_Data
head(data)

data$WEBSITE

website_update = function(x) {
  x$WEBSITE = gsub("?(f|ht)tp(s?)://", "", x$WEBSITE)
  i = 0
  for(row in x$WEBSITE) {
    i = i + 1
    if (!grepl("www.",row, fixed = TRUE) && !is.na(row)) {
      x$WEBSITE[i] = gsub(" ","",paste("www.",row))
    }
  }
  return(x)
}

test = website_update(data)
