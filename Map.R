library(dplyr)
library(leaflet)
library(readxl)

map_function <- function(data){
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTerrain) %>%
  setView(lat = 50, lng = 20, zoom = 6) %>%
  setMaxBounds(lng1 = -140, lat1 = -70, lng2 = 155, lat2 = 70 ) %>%
  addMarkers(data = data,
            lng = ~LONGITUDE,
            lat = ~LATITUDE,
            popup = paste(
              "<b>Názov: </b>",data$TITLE,"</b>","<br>",
              "<b>Otváracie hodiny: </b>",data$OPEN_HOURS,"<br>",
              "<b>Adresa: </b>",data$STREET_NAME," ",data$STREET_NUMBER,", ",data$CITY," ",data$ZIP_CODE,"<br>",
              "<b>Telefonné číslo: </b>",data$PHONE,"<br>",
              sep=""))
}
            
data = read_excel("data.xlsx")
map_function(data)
