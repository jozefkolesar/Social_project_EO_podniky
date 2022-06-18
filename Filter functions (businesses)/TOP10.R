library(readxl)
businesses_rating <- read_excel("businesses_rating.xlsx")

products_rating <- read_excel("products_rating.xlsx")

services_rating <- read_excel("services_rating.xlsx")



businesses_rating_filter = function(dataframe) {
  orderData = dataframe[order(dataframe$rating, decreasing = TRUE),]
  #return(orderData[1:10,3]) # Vráti len názov business_title
  return(orderData[1:10,2:4]) #Vráti TOP 10 podnikov bez ID
}

View(businesses_rating_filter(businesses_rating)) #výpis podnikov

products_rating_filter = function(dataframe) {
  orderData = dataframe[order(dataframe$rating, decreasing = TRUE),]
  #return(orderData[1:10,3]) # Vráti len description
  return(orderData[1:10,2:4]) #Vráti TOP 10 produktov bez ID
}

View(products_rating_filter(products_rating)) #výpis produktov

services_rating_filter = function(dataframe) {
  orderData = dataframe[order(dataframe$rating, decreasing = TRUE),]
  #return(orderData[1:10,3]) # Vráti len description
  return(orderData[1:10,2:4]) #Vráti TOP 10 služieb bez ID
}

View(services_rating_filter(services_rating)) #výpis služieb
