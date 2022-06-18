#install.packages("stringr")
#install.packages("data.table")
#install.packages("tibble")
#install.packages("readxl")
library(stringr)
library(data.table)
library(tibble)
library(readxl)
data = read_excel("data.xlsx")
dictionary = read_excel("main_dictionary.xlsx")

# Function to find out unique values of attributes BUSINESS_CAT_CPV, BUSINESS_DESC_PRODUCTS_CPV and BUSINESS_DESC_SERVICES_CPV.
# INPUTS: DATAFRAME, ATTRIBUTE, DICTIONARY
uniqueValues = function(df, attr, dict){
  df[attr] = gsub("; ", ";", get(substitute(attr), df), fixed = TRUE)
  
  unique_values = unique(unlist(str_split(get(substitute(attr), df), pattern = ";")))
  unique_values = unique_values[!is.na(unique_values)]
  unique_values = as.data.frame(unique_values)
  colnames(unique_values) = c("cpv_code")
  
  unique_values = merge(x = unique_values, y = dict, by = "cpv_code", all.x = TRUE)
  
  return(unique_values)
}

df_unique_values = uniqueValues(data, "BUSINESS_DESC_PRODUCTS_CPV", dictionary)

# Function to filter data based on product
# INPUTS: DATAFRAME, DICTIONARY, PRODUCT
filterByProduct = function(df, dict, name){
  cpv_desc = dict[dict$description == name, ]
  cpv_code = as.vector(t(cpv_desc[1,1]))
  filter_df = df[df$BUSINESS_DESC_PRODUCTS_CPV %like% cpv_code, ]
  
  return(filter_df)
}

# Function to filter data based on service
# INPUTS: DATAFRAME, DICTIONARY, SERVICE
filterByService = function(df, dict, name){
  cpv_desc = dict[dict$description == name, ]
  cpv_code = as.vector(t(cpv_desc[1,1]))
  filter_df = df[df$BUSINESS_DESC_SERVICES_CPV %like% cpv_code, ]
  
  return(filter_df)
}

# EXAMPLES
df_example_1 = filterByProduct(data, dictionary, "Torty/zákusky")
df_example_2 = filterByProduct(data, dictionary, "Obuv")
df_example_3 = filterByService(data, dictionary, "Kalendáre")
df_example_4 = filterByService(data, dictionary, "Stavebné práce")

# Function to filter data based on products
# INPUTS: DATAFRAME, DICTIONARY, PRODUCT
filterByProducts = function(df, dict, names){
  unique_values = unique(unlist(str_split(names, pattern = "; ")))
  
  x = 1
  
  for (x in 1:length(unique_values)) {
    part = dict[dict$description == unique_values[x], ]
    
    if(x == 1){
      cpv_desc = part
      cpv_desc = cpv_desc[0,]
    }
    
    cpv_desc = rbind(cpv_desc, part)
  }
  
  cpv_code = as.vector(t(cpv_desc[,1]))
  
  x = 1
  
  for (x in 1:length(unique_values)) {
    part = df[df$BUSINESS_DESC_PRODUCTS_CPV %like% cpv_code[x], ]
    
    if(x == 1){
      filter_df = part
      filter_df = filter_df[0,]
    }
    
    filter_df = rbind(filter_df, part)
  }
  
  filter_df = unique(filter_df)
  
  return(filter_df)
}

# Function to filter data based on services
# INPUTS: DATAFRAME, DICTIONARY, SERVICE
filterByServices = function(df, dict, names){
  unique_values = unique(unlist(str_split(names, pattern = "; ")))
  
  x = 1
  
  for (x in 1:length(unique_values)) {
    part = dict[dict$description == unique_values[x], ]
    
    if(x == 1){
      cpv_desc = part
      cpv_desc = cpv_desc[0,]
    }
    
    cpv_desc = rbind(cpv_desc, part)
  }
  
  cpv_code = as.vector(t(cpv_desc[,1]))
  
  x = 1
  
  for (x in 1:length(unique_values)) {
    part = df[df$BUSINESS_DESC_SERVICES_CPV %like% cpv_code[x], ]
    
    if(x == 1){
      filter_df = part
      filter_df = filter_df[0,]
    }
    
    filter_df = rbind(filter_df, part)
  }
  
  filter_df = unique(filter_df)
  
  return(filter_df)
}

# EXAMPLES
df_example_5 = filterByProducts(data, dictionary, "Torty/zákusky; Obuv")
df_example_6 = filterByServices(data, dictionary, "Kalendáre; Stavebné práce")







