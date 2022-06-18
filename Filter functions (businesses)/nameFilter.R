install.packages("readxl")
library("readxl")

data=read_excel("Datasets/data.xlsx")

nameFilter <- function(data,titles){
  if(is.null(data)){
    print("dataframe was NULL")
    return(NULL)
  }
  if(is.null(titles)){
    print("names was NULL")
    return(data)
  }
  df=data.frame(matrix(ncol = ncol(data), nrow = 0))
  colnames(df) = colnames(data)
  for (name in titles) {
    filtered=data[data$TITLE==name, ]
    df=rbind(df,filtered)
  }
  return(df)
}

TITLES=c("Minifarma s. r. o.","Rozvojové služby BBSK s.r.o.")
filtered=filterName(data,TITLES)
