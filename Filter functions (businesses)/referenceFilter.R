#install.packages("readxl")
library("readxl")
data <- read_excel("df_edit.xlsx")
df = data.frame(data)

# Function to filter data with reference (TRUE) or nonreferenced (FALSE)
# INPUTS: DATAFRAME, BOOLEAN
filterByReference <- function(df, boolean) {
  if(!boolean){
    return(df[is.na(df$REFERENCE),])
  }
  return(df[!is.na(df$REFERENCE),]) 
}

# EXAMPLES
df_referenced = filterByReference(df, TRUE)
df_nonreferenced = filterByReference(df, FALSE)
