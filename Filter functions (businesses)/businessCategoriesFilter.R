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

df_unique_values = uniqueValues(data, "BUSINESS_DESC_SERVICES_CPV", dictionary)

# Function to filter data based on business category
# INPUTS: DATAFRAME, DICTIONARY, BUSINESS CATEGORY
filterByCategory = function(df, dict, name){
  cpv_desc = dict[dict$description == name, ]
  cpv_code = as.vector(t(cpv_desc[1,1]))
  filter_df = df[df$BUSINESS_CAT_CPV %like% cpv_code, ]
  
  return(filter_df)
}

# EXAMPLES
df_example_1 = filterByCategory(data, dictionary, "Ťažba dreva")
df_example_2 = filterByCategory(data, dictionary, "Pracovné odevy, špeciálne pracovné odevy a doplnky")
df_example_3 = filterByCategory(data, dictionary, "Upratovacie služby")

# Function to filter data based on business categories
# INPUTS: DATAFRAME, DICTIONARY, BUSINESS CATEGORIES
filterByCategories = function(df, dict, names){
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
    part = df[df$BUSINESS_CAT_CPV %like% cpv_code[x], ]
    
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
df_example_4 = filterByCategories(data, dictionary, "Ťažba dreva; Technické služby; Čistenie (upratovanie) budov")
df_example_5 = filterByCategories(data, dictionary, "Maloobchodné služby; Služby súvisiace s lesníctvom; Príprava staveniska")
df_example_6 = filterByCategories(data, dictionary, "Pracovné odevy, špeciálne pracovné odevy a doplnky; Ovocie, zelenina a súvisiace výrobky")
