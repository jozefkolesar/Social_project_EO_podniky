install.packages("readxl")
library(readxl)
data = read_excel("data.xlsx")

# Function to filter data based on type RSP ("Integračný podnik","Sociálny podnik", "Všeobecný podnik", "Verejnoprospešný podnik")

filterByTypeRsp = function(df, name){
  filter_df = df[df$TYPE_RSP == name, ]
  
  return(filter_df)
}

# EXAMPLES-integracny 
df_integracny_podnik = filterByTypeRsp(data, "integračný")
df_integracno_socialny = filterByTypeRsp(data, "integračný; sociálny")
df_integracno_vseobecny = filterByTypeRsp(data, "integračný; všeobecný")
df_integracno_verejnoprospesny= filterByTypeRsp(data, "integračný; verejnoprospešný")

# EXAMPLES-socialny
df_socialny_podnik = filterByTypeRsp(data, "sociálny")
df_socialno_integracny = filterByTypeRsp(data, "sociálny; integračný")
df_sociálno_vseobecny = filterByTypeRsp(data, "sociálny; všeobecný")
df_sociálno_verejnoprospesny= filterByTypeRsp(data, "sociálny; verejnoprospešný")

# EXAMPLES-vseobecny
df_vseobecny_podnik = filterByTypeRsp(data, "všeobecný")
df_vseobecno_integracny = filterByTypeRsp(data, "všeobecný; integračný")
df_vseobecno_sociálny = filterByTypeRsp(data, "všeobecný; sociálny")
df_všeobecno_verejnoprospesny= filterByTypeRsp(data, "všeobecný; verejnoprospešný")

#EXAMPLES- verejnoprospesny
df_verejnoprospesny_podnik = filterByTypeRsp(data, "verejnoprospešný")
df_verejnoprospesno_integracny = filterByTypeRsp(data, "verejnoprospešný; integračný")
df_verejnoprospesno_sociálny = filterByTypeRsp(data, "verejnoprospešný; sociálny")
df_verejnoprospesno_vseobecny= filterByTypeRsp(data, "verejnoprospešný; všeobecný")


