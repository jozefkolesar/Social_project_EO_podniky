# install.packages("readxl")
library(readxl)
data = read_excel("data.xlsx")

# Function to filter data based on legal form ("Spoločnosť s ručením obmedzeným",
# "Družstvo", "Nadácia", "Nezisková organizácia", "Obchodná spoločnosť", "Občianske združenie")
# INPUTS: DATAFRAME, NAME
filterByLegalForm = function(df, name){
  filter_df = df[df$LEGAL_FORM == name, ]
  
  return(filter_df)
}

# EXAMPLES
df_sro = filterByLegalForm(data, "Spoločnosť s ručením obmedzeným")
df_druzstvo = filterByLegalForm(data, "Družstvo")
df_nadacia = filterByLegalForm(data, "Nadácia")
df_neziskova = filterByLegalForm(data, "Nezisková organizácia")
df_os = filterByLegalForm(data, "Obchodná spoločnosť")
df_oz = filterByLegalForm(data, "Občianske združenie")
