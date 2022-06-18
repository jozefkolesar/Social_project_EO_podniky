#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("openxlsx")
library(readxl)
library(tidyverse)
library(openxlsx)
data = read_excel("data.xlsx")
main_dic = read_excel("main_dictionary.xlsx")

# Function for split products into lines and assigning a descriptions based on a cpv codes
# INPUTS: DATAFRAME, DICTIONARY
edit_products = function(df, dic){
  wout_na = df %>% filter(!is.na(df$BUSINESS_DESC_PRODUCTS_CPV))
  prod_data = select(wout_na, "ID", "BUSINESS_DESC_PRODUCTS_CPV")
  
  str_spl = strsplit(prod_data$BUSINESS_DESC_PRODUCTS_CPV, split = "; ")
  
  prod_data = data.frame(ID = rep(prod_data$ID, sapply(str_spl, length)), cpv_code = unlist(str_spl))
  
  prod_data = merge(x = prod_data, y = dic, by = "cpv_code", all.x = TRUE)
  
  prod_data = prod_data[order(prod_data$ID),] 
  
  prod_data = prod_data[,c("ID", "cpv_code", "description")]
  
  return(prod_data)
}

df_edit_products = edit_products(data, main_dic)

# Function for generating a rating
# INPUTS: DATAFRAME (output from the previous function), MIN_VALUE, MAX_VALUE, NUMBER OF DECIMAL PLACES
products_rating_gen = function(edit_products_df, min_v, max_v, round_dig){
  edit_products_df$rating = NA
  edit_products_df$rating = round(runif(nrow(edit_products_df), min = min_v, max = max_v), digits = round_dig)
  
  return(edit_products_df)
}

df_edit_products = products_rating_gen(df_edit_products, 1, 5, 1)

# Function for selecting top records
# INPUTS: DATAFRAME (output from the previous function), NUMBER OF TOP RECORDS
top_products = function(products_rating_gen_df, num_of_top){
  df_top = head(products_rating_gen_df[order(-products_rating_gen_df$rating),], num_of_top)
  
  return(df_top)
}

df_top_products = top_products(df_edit_products, 10)











