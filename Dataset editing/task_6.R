library(readxl)
library(sjmisc)

data = read_excel("data.xlsx")
data = data[-1,]

#Remove spaces
removespaces = function(df, attr){
  df[attr] = gsub(" ", "", get(substitute(attr), df), fixed = TRUE)
  
  return(df)
}

data = removespaces(data, "ID_ORG")

#VAT
editvat = function(df){
  df$VAT = trimws(df$VAT, "l")
  df$VAT = trimws(df$VAT, "r")
  
  i = 1
  
  while (i <= nrow(df)) {
    if(is.na(df[i, "VAT"])){
    }else if(is.na(df[i, "VAT"]) == FALSE){
      sel = substr(df[i, "VAT"], start = 1, stop = 2)

      if(sel == "SK"){
        df[i, "VAT"] = gsub(" ", "", df[i, "VAT"], fixed = TRUE)
      }else if(sel != "SK"){
        sent = tolower(as.vector(t(df[i, "VAT"])))
        
        frst = grepl("nie", sent, fixed = TRUE)
        secnd = grepl("dph", sent, fixed = TRUE)
        
        if(frst == TRUE && secnd == TRUE){
          df[i, "VAT"] = "Spoločnosť nie je platcom DPH!"
        }
      }
    }
    
    i = i + 1
  }
  
  return(df)
}

data = editvat(data)

#LEGAL_FORM
editlf = function(df){
  df$LEGAL_FORM = trimws(df$LEGAL_FORM, "l")
  df$LEGAL_FORM = trimws(df$LEGAL_FORM, "r")
  df$LEGAL_FORM = tolower(df$LEGAL_FORM)
  
  i = 1
  
  while(i <= nrow(df)){
    if(nchar(df[i, "LEGAL_FORM"]) <= 10){
      df[i, "LEGAL_FORM"] = gsub(" ", "", df[i, "LEGAL_FORM"], fixed = TRUE)
    }
    
    if(df[i, "LEGAL_FORM"] == "spoločnosť s ručením obmedzeným"){
      df[i, "LEGAL_FORM"] = "s.r.o."
    }
    
    i = i + 1
  }
  
  return(df)
}

data = editlf(data)

#NO_EMPL_2004 and NO_EMPL_2020
noemplgen = function(df, attr, startnum, endnum){
  df[attr] = as.numeric(get(substitute(attr), df))
  
  i = 1
  
  while (i <= nrow(df)){
    if(is.na(df[i, attr])){
      number = as.numeric(sample(startnum:endnum, 1, replace=TRUE))
      df[i, attr] = number
    }
    
    i = i + 1
  }

  return(df)
}

data = noemplgen(data, "NO_EMPL_2020", 10, 120)

#TYPE_RSP
edittypersp = function(df){
  df$TYPE_RSP = trimws(df$TYPE_RSP, "l")
  df$TYPE_RSP = trimws(df$TYPE_RSP, "r")
  
  df$TYPE_RSP = tolower(df$TYPE_RSP)
  
  df$TYPE_RSP = gsub("rsp", "", df$TYPE_RSP, fixed = TRUE)
  
  df$TYPE_RSP = gsub("podnik", "", df$TYPE_RSP, fixed = TRUE)
  df$TYPE_RSP = gsub("\\s*(\\([^()]*(?:(?1)[^()]*)*\\))", "", df$TYPE_RSP, perl=TRUE)
  
  df$TYPE_RSP = gsub(" / ", "; ", df$TYPE_RSP, fixed = TRUE)
  df$TYPE_RSP = gsub(" a ", "; ", df$TYPE_RSP, fixed = TRUE)
  df$TYPE_RSP = gsub(", ", "; ", df$TYPE_RSP, fixed = TRUE)
  
  df$TYPE_RSP = trimws(df$TYPE_RSP, "l")
  df$TYPE_RSP = trimws(df$TYPE_RSP, "r")
  
  df$TYPE_RSP = gsub("ý ", "ý; ", df$TYPE_RSP, fixed = TRUE)
  df$TYPE_RSP = gsub("y ", "y; ", df$TYPE_RSP, fixed = TRUE)
  df$TYPE_RSP = gsub("integrovany", "integračný", df$TYPE_RSP, fixed = TRUE)
  
  df$TYPE_RSP = trimws(df$TYPE_RSP, "l")
  df$TYPE_RSP = trimws(df$TYPE_RSP, "r")
  
  return(df)
}

data = edittypersp(data)







