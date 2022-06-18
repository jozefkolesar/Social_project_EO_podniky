#install.packages("readxl")
#install.packages("tidyverse")
library(readxl)
library(tidyverse)
data = read_excel("data.xlsx")

# Function to filter data with emails
# INPUTS: DATAFRAME
# EMAIL
filterByEmail = function(df){
  filter_email = df %>% drop_na(EMAIL)
  
  return(filter_email)
}

# Function to filter data with phone numbers
# INPUTS: DATAFRAME
# PHONE
filterByPhone = function(df){
  filter_phone = df %>% drop_na(PHONE)
  
  return(filter_phone)
}

# Function to filter data with any social networks
# INPUTS: DATAFRAME
filterBySocialNetworks = function(df){
  filter_social_all = data[!with(data, is.na(SOCIAL_NETW_FB) & is.na(SOCIAL_NETW_OTHER)), ]
  
  return(filter_social_all)
}

# Function to filter data with Facebook
# INPUTS: DATAFRAME
filterByFacebook = function(df){
  filter_social_fb = df %>% drop_na(SOCIAL_NETW_FB)
  
  return(filter_social_fb)
}

# Function to filter data with social networks other than Facebook
# INPUTS: DATAFRAME
filterByOther = function(df){
  filter_social_other = df %>% drop_na(SOCIAL_NETW_OTHER)
  
  return(filter_social_other)
}

# EXAMPLES
df_with_mail = filterByEmail(data)
df_with_phone = filterByPhone(data)
df_with_facebook = filterByFacebook(data)
df_with_other = filterByOther(data)
df_with_social_networks = filterBySocialNetworks(data)