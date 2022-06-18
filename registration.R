library(stringr)
library(data.table)
library(tibble)
library(readxl)
library(openxlsx)
library(tidyr)
library(stringi)

# ZMEN SI CESTU V REGISTER FUNKCII PRE FILES


# Overenie stringu + dlzka musi byt (2-20)
isTitleString <- function(input) {
  is.character(input) & length(input) == 1 & nchar(input) <= 20 & nchar(input) >= 2
}

# Overenie stringu + dlzka musi byt (2-300)
isDescString <- function(input) {
  is.character(input) & length(input) == 1 & nchar(input) <= 300 & nchar(input) >= 2
}

# Overenie ICO + dlzka musi byt 8 znakov
isORG <- function(input) {
  is.character(input) & length(input) == 1 & nchar(input) == 8
}

# Overenie validneho emailu
isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
}

# Formatovanie smerovacieho cisla
isValidPostalCode <- function(input) {
  input = gsub(" ", "", input, fixed = TRUE)
  if(nchar(input) != 6)
  {
    stri_sub(input, 4, 3) = " "
  }
  return (input)
}

# Konvertuje list produktov na cpv kody
convert <- function(list){
  newList = list()
  for(i in listOfProducts){
    cpv_desc = dictionary[dictionary$description == i, ]
    newList = append(newList, as.vector(t(cpv_desc[1,1])))
  }
  return (newList)
}

# Registracia (nacita, upravi a zapise data do suboru)
register <- function(TITLE, STORY, EMAIL, BUSINESS_CAT, ADDRESS, TYPE_RSP, ZIP_CODE, ID_ORG, listOfProducts) {
  
  data = read_excel("C:\\Users\\Tomas\\Desktop\\EO_social_project\\socialprojecteo\\Datasets\\data.xlsx")
  dictionary = read_excel("C:\\Users\\Tomas\\Desktop\\EO_social_project\\socialprojecteo\\Datasets\\main_dictionary.xlsx")
  products = read_excel("C:\\Users\\Tomas\\Desktop\\EO_social_project\\socialprojecteo\\Datasets\\ServicesProducts.xlsx")
  
  list = convert(listOfProducts)
  
  cpv_desc = dictionary[dictionary$description == BUSINESS_CAT, ]
  BUSINESS_CAT_CPV = as.vector(t(cpv_desc[1,1]))
  
  df_1 = select(data, "TITLE", "STORY", "EMAIL", "BUSINESS_CAT", "BUSINESS_CAT_CPV", "STREET_NAME", "STREET_NUMBER", "TYPE_RSP", "ZIP_CODE", "ID_ORG")
  
  STREET_NAME = gsub("(.+)\\s[^ ]+$","\\1", ADDRESS)
  STREET_NUMBER = gsub(".+\\s([^ ]+)$","\\1", ADDRESS)
  
  ZIP_CODE = isValidPostalCode(ZIP_CODE)
  
  if (isTitleString(TITLE) == TRUE && isValidEmail(EMAIL) == TRUE && isDescString(STORY) == TRUE && isORG(ID_ORG) == TRUE){
    df_1 = add_row(df_1, TITLE, STORY, EMAIL, BUSINESS_CAT, BUSINESS_CAT_CPV, STREET_NAME, STREET_NUMBER, TYPE_RSP, ZIP_CODE, ID_ORG)
    countOfRows = nrow(df_1)
    
    map <- data.frame(matrix(unlist(list), nrow=length(list), byrow=TRUE))
    map$ID = countOfRows+1
    colnames(map) = c("cpv_code","ID")
    map = map[, c(2, 1)]
    products = rbind(products, map)
    write.xlsx(products, "C:\\Users\\Tomas\\Desktop\\EO_social_project\\socialprojecteo\\Datasets\\ServicesProducts.xlsx")
    write.xlsx(df_1, "C:\\Users\\Tomas\\Desktop\\EO_social_project\\socialprojecteo\\Datasets\\data.xlsx")
    return (df_1)
  } 
  return (NULL)
}


# PRIKLAD
listOfProducts <- list("Sójové bôby", "Osivo", "Semená kvetov")
# priklad pouzitia - vrati updatovani DataFrame, ak nieco nie je validne, vrati NULL
insert = register("Aasasa","Bsasa","a@b.com","Sezamové semená","asasa 45","integracny","54862", "12345678", listOfProducts)






