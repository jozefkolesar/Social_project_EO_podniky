#### INSTALACIA KNIZNIC ###

# PROCEED BEFORE THE FIRST RUN OF THE APPLICATION #

# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("reactable")
# install.packages("stringr")
# install.packages("data.table")
# install.packages("tibble")
# install.packages("shinyjs")
# install.packages("sendmailR")
# install.packages("gdata")
# install.packages("stringi")
# install.packages("openxlsx")
# install.packages("sendmailR")
# install.packages("dplyr")
# install.packages("DT")
# install.packages("margittr")

# Nacitanie kniznic 
library(ggplot2)
library(readxl)
library(shiny)
library(leaflet)
library(reactable)
library(stringr)
library(data.table)
library(tibble)
library(shinyjs)
library(sendmailR)
library(gdata)
library(stringi)
library(openxlsx)
library(dplyr)
library(DT)
library(magrittr)

data = read_excel("Data/data.xlsx")
dictionary = read_excel("Data/main_dictionary.xlsx")
slpr_kat = read_excel("Data/produkty-kategorie.xlsx")

shinyServer(function(input, output, session) {
  
  #Preklik z df, z katalogu podnikov na profile
  observe({
    data = read.xlsx("Data/data.xlsx")
    
    selected_mood = reactiveVal()
    
    shinyjs::onclick("my_img1", selected_mood('Potraviny'))
    shinyjs::onclick("my_img2", selected_mood('Hotelové služby'))
    shinyjs::onclick("my_img3", selected_mood('Odevy, obuv a textil'))
    shinyjs::onclick("my_img4", selected_mood('Kozmetika a zdravie'))
    shinyjs::onclick("my_img5", selected_mood('Stavbári a remeselníci'))
    shinyjs::onclick("my_img6", selected_mood('Poľnohospodárstvo a priemysel'))
    shinyjs::onclick("my_img7", selected_mood('Nábytok a bývanie'))
    shinyjs::onclick("my_img8", selected_mood('Iné'))
    
    output$podnik_nadpiss = renderText({
      paste(selected_mood(), "", sep = "")}
    )
    
    output$daatatable = DT::renderDataTable({  
      req(selected_mood())
      
      cpv_desc = dictionary[dictionary$description == selected_mood(), ]
      cpv_code = as.vector(t(cpv_desc[1,1]))
      edit_df = data[data$hlavne_kategorie_cpv %like% cpv_code, ]
      filter_df = select(edit_df, TITLE)
      
      result_data = as.data.frame(edit_df)
      
      DT::datatable(filter_df, selection = 'single', colnames = c('Podnik' = 'TITLE'), rownames = FALSE)
    }, server = FALSE)
    
    yesCheck = function(value){
      x = value
      
      if(is.null(x)){
        x = "Nie"
      }else{
        x = "Áno"
      }
      
      return(x)
    }
    
    observe({
      if(is.null(input$btn_rating) || input$btn_rating == 0) return(NULL)
      notif_rat = NULL
      
      isolate({
        if(yesCheck(input$check_rating) == "Áno" && rv_tab$last_tab == "panel2"){  
          
          rating = read_excel("Data/businesses_rating.xlsx")
          
          cpv_desc = dictionary[dictionary$description == selected_mood(), ]
          cpv_code = as.vector(t(cpv_desc[1,1]))
          edit_df = data[data$hlavne_kategorie_cpv %like% cpv_code, ]
          filter_df = select(edit_df, TITLE)
          
          result_data = as.data.frame(edit_df)
          
          row = input$daatatable_rows_selected
          
          id = result_data[row, "ID"]
          
          res = rating[rating$ID == id,]
          
          slider_input = as.numeric(input$slider_hodnotenie)
          
          rat_num = as.numeric(res[,"rating_num"])
          rat_num = rat_num + 1
          
          rat = as.numeric(res[,"rating"])
          rat = (rat + slider_input)/rat_num
          
          rating[id, "rating_num"] = rat_num
          rating[id, "rating"] = rat
          
          write.xlsx(rating, "Data/businesses_rating.xlsx", row.names = FALSE)
          
          notif_rat = showNotification(paste("Hodnotenie bolo odoslané."), duration = 2, type = "message")
          notif_rat = NULL
          
          updateCheckboxGroupInput(session=session, inputId="check_rating", choices=c("Áno"), selected = NULL)
        }else if(yesCheck(input$check_rating) == "Áno" && rv_tab$last_tab == "homepage"){
          rating = read_excel("Data/businesses_rating.xlsx")
          
          updateCheckboxGroupInput(session=session, inputId="check_rating", choices=c("Áno"), selected = NULL)
        }
      })
    })
    
    observeEvent(input$daatatable_rows_selected, {
      cpv_desc = dictionary[dictionary$description == selected_mood(), ]
      cpv_code = as.vector(t(cpv_desc[1,1]))
      edit_df = data[data$hlavne_kategorie_cpv %like% cpv_code, ]
      filter_df = select(edit_df, TITLE)
      
      result_data = as.data.frame(edit_df)
      
      row = input$daatatable_rows_selected 
      
      output$podnik_nadpis = renderText({
        paste(result_data[row, "TITLE"], "", sep = "")}
      )
      
      output$text_katalog = renderText({
        if(is.na(result_data[row, "STORY"]) == TRUE){
          paste("Nie je k dispozícii.", sep = "")
        }else{
          paste(result_data[row, "STORY"], "", sep = "") 
        }
      })
      
      output$typ_podniku = renderText({
        if(is.na(result_data[row, "TYPE_RSP"]) == TRUE){
          paste("Nie je k dispozícii.", sep = "")
        }else{
          paste(result_data[row, "TYPE_RSP"], "", sep = "")
        }
      })
      
      output$adresa = renderText({
        paste(result_data[row, "STREET_NAME"], " ", result_data[row, "STREET_NUMBER"], ", ", result_data[row, "CITY"], ", ", result_data[row, "ZIP_CODE"], sep = "")}
      )
      
      output$gls_tel_cislo = renderText({
        if(is.na(result_data[row, "PHONE"]) == TRUE){
          paste("Nie je k dispozícii.", sep = "")
        }else{
          paste(result_data[row, "PHONE"], "", sep = "")
        }
      })
      
      output$gls_mail = renderText({
        if(is.na(result_data[row, "EMAIL"]) == TRUE){
          paste("Nie je k dispozícii.", sep = "")
        }else{
          paste(result_data[row, "EMAIL"], "", sep = "")
        }
      })
      
      output$gls_web = renderText({
        if(is.na(result_data[row, "WEBSITE"]) == TRUE){
          paste("Nie je k dispozícii.", sep = "")
        }else{
          paste(result_data[row, "WEBSITE"], "", sep = "")
        }
      })
      
      output$gls_slpr = renderText({
        paste(result_data[row, "BUSINESS_DESC"], "", sep = "")}
      )
      
      output$kategoria_nadpis = renderText({
        res = result_data[row, "hlavne_kategorie"]
        res = str_to_sentence(res)
        paste(res, "", sep = "")}
      )
      
      output$profile_produkty = DT::renderDataTable({
        data = read_excel("Data/data.xlsx")
        
        id = as.numeric(result_data[row, "ID"])
        
        res = filter(data, ID == id)
        
        produkty = as.vector(t(res$BUSINESS_DESC_PRODUCTS_CPV))
        sluzby = as.vector(t(res$BUSINESS_DESC_SERVICES_CPV))
        
        produkty = unique(unlist(str_split(produkty, pattern = "; ")))
        sluzby = unique(unlist(str_split(sluzby, pattern = "; ")))
        
        all = unique(c(produkty, sluzby))
        all = as.data.frame(na.omit(all))
        colnames(all) = c("cpv_code")
        
        dict = tail(dictionary, -8)
        
        all = merge(x = all, y = dict, by = "cpv_code", all.x = TRUE)
        all$cpv_code = NULL
        colnames(all) = c("Produkty a služby")
        
        DT::datatable(all)
      })
      
      output$stars_ui = renderUI({
        rating = read_excel("Data/businesses_rating.xlsx")
        id = result_data[row, "ID"]
        
        res = (as.numeric(rating[rating$ID == id, "rating"]) / 5) * 100
        
        style_value = sprintf("width:%s%%", res)
        tags$div(class = "full-stars", style = style_value)
      })
      
      output$rating_number = renderUI({
        rating = read_excel("Data/businesses_rating.xlsx")
        id = result_data[row, "ID"]
        
        res = round(as.numeric(rating[rating$ID == id, "rating"]), digits = 2)
        
        div(res)
      })
      
      updateTabsetPanel(session, "inTabset", selected = "profile_bus")
    })
    })
  #############################################
  
  #Preklik z df na katalog produktov
  observe({
    data = read.xlsx("Data/data.xlsx")
    
    selected_moodd = reactiveVal()
    
    shinyjs::onclick("my_img9", selected_moodd('Potraviny'))
    shinyjs::onclick("my_img10", selected_moodd('Hotelové služby'))
    shinyjs::onclick("my_img11", selected_moodd('Odevy, obuv a textil'))
    shinyjs::onclick("my_img12", selected_moodd('Kozmetika a zdravie'))
    shinyjs::onclick("my_img13", selected_moodd('Stavbári a remeselníci'))
    shinyjs::onclick("my_img14", selected_moodd('Poľnohospodárstvo a priemysel'))
    shinyjs::onclick("my_img15", selected_moodd('Nábytok a bývanie'))
    shinyjs::onclick("my_img16", selected_moodd('Iné'))
    
    output$produkt_nadpiss = renderText({
      paste(selected_moodd(), "", sep = "")}
    )
    
    output$daaatatable = DT::renderDataTable({  
      req(selected_moodd())
      
      kategoria = na.omit(slpr_kat[, selected_moodd()])
      colnames(kategoria) = c("description")
      
      res = na.omit(merge(x = kategoria, y = dictionary, by = "description", all.x = TRUE))
      res_vec = as.vector(t(res$cpv_code))
    
      col_search = transform(data, newcol = paste(BUSINESS_DESC_PRODUCTS_CPV, BUSINESS_DESC_SERVICES_CPV, sep="; "))
      col_search = select(col_search, newcol)
      
      i = 1
      value_title = c()
      value_title_df = data.frame(value_title = character())
      
      while (i <= nrow(res)) {
        value = res_vec[i]
        rows = as.numeric(which(grepl(value, col_search$newcol, fixed = TRUE)))
 
        value_title = paste(data[rows, "TITLE"], collapse = '; ')

        value_title_df = add_row(value_title_df, value_title)
        
        i = i + 1
      }
      
      res$Podniky = value_title_df$value_title
      res[res == ""] = NA
      res = na.omit(res)
      
      colnames(res) = c("Produkty a služby", "cpv_code", "Podniky")
      filter_df = select(res, "Produkty a služby", "Podniky")
      
      row.names(filter_df) = 1 : nrow(filter_df)
      
      DT::datatable(filter_df, rownames = FALSE)
      
    }, server = FALSE)
  })
  #############################################
  
  
  
  
  
  
  
  
  observe({
    toggle(selector = "#inTabset li a[data-value=panel2]")
  })
  
  
  observe({
    toggle(selector = "#inTabset li a[data-value=panel4]")
  })
  
  observe({
    toggle(selector = "#inTabset li a[data-value=profile_bus]")
  })
  
  rv_tab = reactiveValues()
  
  observeEvent(input$inTabset, {
    rv_tab$last_tab = rv_tab$current_tab
    rv_tab$current_tab = input$inTabset
  })
  
  observeEvent(input$btn_bt_cat, {
    updateTabsetPanel(session, "inTabset",
                      selected = rv_tab$last_tab)
  })

  observeEvent(input$jumpToProducts, {
    updateTabsetPanel(session, "inTabset",
                      selected = "pan_kat_prod")
  })
  
  
  observeEvent(input$jumpToBuss, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  
   observeEvent(input$button_jump_registracia, {
     updateTabsetPanel(session, "inTabset",
                       selected = "tab_registracia")
   })
   
   observeEvent(input$button_jump_kontakt, {
     updateTabsetPanel(session, "inTabset",
                       selected = "tab_kontakt")
   })

   
   # nacitanie dat a renderovanie tabulky pre TOP 10 podnikov, produktov (sucast homepage)
   output$table <- renderReactable({
     top_podniky = read_xlsx("Data/businesses_rating.xlsx") 
     
     res = head(top_podniky[order(-top_podniky$rating), ], 10)
     top_podniky = res
     res = select(res, TITLE)
     
     i = 1
     
     while (i <= nrow(res)) {
       res[i,1] = paste(i, ". ", res[i,1], sep = "")
       
       i = i + 1
     }
     
     reactable(res, resizable = TRUE, showPageSizeOptions = TRUE,sortable = FALSE,
               onClick = "expand", highlight = F,
               details = function(index) {
                 
                 div(class = "top-produkt",
                     div(class = "top-produkt-logo",
                         img(class = "top-produkt-logo-img", src = as.character(top_podniky[index, "image"]))
                     ),
                     div(class = "top-produkt-popis",
                         h3(class = "top-produkt-nazov", as.character(top_podniky[index, "TITLE"])),
                         p(as.character(top_podniky[index, "STORY"]))
                     )
                 )
               },
               rowClass = "my-row")
   })
  
  #ContactForm
  isNameLength = function(x) {
    x = trim(x)
    nchar(x) <= 200 & nchar(x) >= 1
  }
  
  isValidEmail = function(y) {
    y = trim(y)
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(y), ignore.case=TRUE) & nchar(y) >= 1
  }
  
  isBodyLength = function(z) {
    z = trim(z)
    nchar(z) <= 200 & nchar(z) >= 1
  }
 
  observe({
    if(is.null(input$button_contact) || input$button_contact == 0) return(NULL)
      notif_mess = NULL
      
      isolate({
        if(isNameLength(input$meno) == TRUE && isValidEmail(input$email_from) == TRUE && isBodyLength(input$text) == TRUE){
          from = paste0("<", trim(isolate(input$email_from)), ">", sep="")
          to = "<eobch12321@gmail.com>"
          subject = trim(isolate(input$meno))
          body = trim(isolate(input$text))                   
          mailControl=list(smtpServer="ASPMX.L.GOOGLE.COM")
          
          sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
          
          updateTextInput(session, "meno", value = "", placeholder = paste("*Zadajte vaše meno"))
          updateTextInput(session, "email_from", value = "", placeholder = paste("*Zadajte vašu e-mailovú adresu"))
          updateTextInput(session, "text", value = "", placeholder = paste("*Text"))
          
          if (!is.null(notif_mess))
            return(NULL)
          
          notif_mess = showNotification(paste("Správa bola odoslaná."), duration = 2, type = "message")
          notif_mess = NULL
        }else{
          if(isNameLength(input$meno) == FALSE){
            updateTextInput(session, "meno", value = "", placeholder = paste("*Vaše meno musí mať minimálne 1 znak!"))
          }
          
          if(isValidEmail(input$email_from) == FALSE){
            updateTextInput(session, "email_from", value = "", placeholder = paste("*Zadaný e-mail je neplatný! (požadovaný tvar je a@b.c)"))
          }
          
          if(isBodyLength(input$text) == FALSE){
            updateTextInput(session, "text", value = "", placeholder = paste("*Vaša správa musí mať minimálne 1 znak!"))
          }
        } 
      })
  })
  
  #Registration
  titleCheck = function(input) {
    input = trim(input)
    nchar(input) <= 40 & nchar(input) >= 1
  }
  
  cityCheck = function(input) {
    input = trim(input)
    is.character(input) & nchar(input) <= 40 & nchar(input) >= 1
  }
  
  streetCheck = function(input) {
    input = trim(input)
    nchar(input) <= 40 & nchar(input) >= 1
  }
  
  streetNumCheck = function(input) {
    input = trim(input)
    nchar(input) <= 20 & nchar(input) >= 1
  }
  
  postalCodeCheck = function(input) {
    input = gsub(" ", "", input, fixed = TRUE)
    if(nchar(input) != 6)
    {
      stri_sub(input, 4, 3) = " "
    }
    
    input = trim(input)
    
    str_count(input, "[0-9]") == 5
    
  }
  
  postalCodeEdit = function(input) {
    input = gsub(" ", "", input, fixed = TRUE)
    if(nchar(input) != 6)
    {
      stri_sub(input, 4, 3) = " "
    }
    
    input = trim(input)
    
    return (input)
  }
  
  emailCheck = function(input) {
    input = trim(input)
    grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(input), ignore.case=TRUE) & nchar(input) >= 1
  }
  
  checkPhoneNum = function(number){
    number = gsub(" ", "", number, fixed = TRUE)
    number = trim(number)
    nchar(number) == 10
  }
  
  katLenCheck = function(input) {
    length(input) > 0
  }
  
  slprLenCheck = function(input) {
    length(input) > 0
  }
  
  rspLenCheck = function(input) {
    length(input) > 0
  }
  
  rv_kat = reactiveValues()
  
  cpvCodesEdit = function(values){
    dict = read_excel("Data/main_dictionary.xlsx")
    
    values = unique(unlist(str_split(values, pattern = "; ")))
    
    dict = tail(dict, -8)
    
    x = 1
    
    for (x in 1:length(values)) {
      part = dict[dict$description == values[x], ]
      
      if(x == 1){
        cpv_desc = part
        cpv_desc = cpv_desc[0,]
      }
      
      cpv_desc = rbind(cpv_desc, part)
    }
    
    cpv_code = as.vector(t(cpv_desc[,1]))
    
    if(length(cpv_code) == 0){
      cpv_code = NA
    }else if(length(cpv_code) > 0){
      cpv_code = paste(cpv_code, collapse = '; ')
    }
    
    return(cpv_code)
  }
  
  mainCategories = function(values){
    prod_categories = read_xlsx("Data/produkty-kategorie.xlsx")
    
    values = gsub("; ", ";", values, fixed = TRUE)
    values = unique(unlist(str_split(values, pattern = ";")))
    
    result = c()
    
    i = 1
    
    while (i <= length(values)) {
      values[i] = trim(values[i])
      
      res = as.numeric(which(prod_categories == values[i], arr.ind=TRUE)[2])
      
      if(!is.na(res)){
        colname = colnames(prod_categories[res])
        
        result = c(result, colname)
      }
      
      i = i + 1
    }
    
    result = unique(result)
    result = tolower(paste(result, collapse = '; '))
    
    return(result)
  }
  
  mainCategoriesCpv = function(values){
    prod_categories = read_xlsx("Data/produkty-kategorie.xlsx")
    
    values = gsub("; ", ";", values, fixed = TRUE)
    values = unique(unlist(str_split(values, pattern = ";")))
    
    result = c()
    
    i = 1
    
    while (i <= length(values)) {
      values[i] = trim(values[i])
      
      res = as.numeric(which(prod_categories == values[i], arr.ind=TRUE)[2])
      
      if(!is.na(res)){
        colname = colnames(prod_categories[res])
        
        result = c(result, colname)
      }
      
      i = i + 1
    }
    
    res_df = as.data.frame(unique(result))
    colnames(res_df) = c("description")
    result = as.vector(t(merge(x = res_df, y = head(dictionary, 8), by = "description", all.x = TRUE)[,2]))
    result = paste(result, collapse = '; ')
    
    return(result)
  }
  
  observeEvent(input$kategoria_podn, {
    rv_kat$items = c(input$kategoria_podn)

    if(length(rv_kat$items) > 1){
      rv_kat$items = paste(rv_kat$items, collapse = '; ')
    }
  })
  
  rv_slpr = reactiveValues()
  
  observeEvent(input$slpr_ponuka, {
    rv_slpr$items = c(input$slpr_ponuka)
    
    if(length(rv_slpr$items) > 1){
      rv_slpr$items = paste(rv_slpr$items, collapse = '; ')
    }
  })
  
  rv_rsp = reactiveValues()
  
  observeEvent(input$rsp_id, {
    rv_rsp$items = c(input$rsp_id)
    
    if(length(rv_rsp$items) > 1){
      rv_rsp$items = paste(rv_rsp$items, collapse = '; ')
    }
  })
  
   observe({
     if(is.null(input$registration_button) || input$registration_button == 0) return(NULL)
     notif_mess_reg = NULL
     
     isolate({
       if(katLenCheck(rv_kat$items) == TRUE && slprLenCheck(rv_slpr$items) == TRUE && rspLenCheck(rv_rsp$items) == TRUE
          && titleCheck(input$nazov) == TRUE && cityCheck(input$mesto) == TRUE && streetCheck(input$ulica) == TRUE
          && streetNumCheck(input$supisne_cislo) == TRUE && postalCodeCheck(input$psc) == TRUE && emailCheck(input$email) == TRUE
          && checkPhoneNum(input$tel_cislo) == TRUE){
         
         data = read_excel("Data/data.xlsx")
         rat = read_excel("Data/businesses_rating.xlsx")
         
         ID = nrow(data) + 1
         TITLE = trim(input$nazov)
         
         if(is.null(input$popis) || input$popis == 0){
           STORY = NA
         }else{
           STORY = trim(input$popis)
         }

         CITY = trim(input$mesto)
         STREET_NAME = trim(input$ulica)
         STREET_NUMBER = trim(input$supisne_cislo)
         ZIP_CODE = postalCodeEdit(input$psc)
         EMAIL = trim(input$email)
         ID_ORG = NA
         BUSINESS_CAT = trim(tolower(rv_kat$items))
         BUSINESS_DESC = trim(tolower(rv_slpr$items))
         TYPE_RSP = trim(tolower(rv_rsp$items))
         
         help_var = cpvCodesEdit(rv_slpr$items)
         
         BUSINESS_DESC_PRODUCTS_CPV = help_var
         BUSINESS_DESC_SERVICES_CPV = help_var
         
         TITLE_BUSINESS = NA
         PHONE = trim(gsub(" ", "", input$tel_cislo, fixed = TRUE))
         WEBSITE = NA
         SOCIAL_NETW_FB = NA
         SOCIAL_NETW_OTHER = NA
         LEGAL_FORM = NA
         VAT = NA
         ID_VAT = NA
         IMAGE = NA
         REGISTR_DATE = NA
         REGION_ACTIVITY = NA
         DISTRICT_ACTIVITY = NA
         OPEN_HOURS = NA
         OBJECTIVES = NA
         BUSINESS_CAT_CPV = NA
         HIGHLIGHTS = NA
         ASOC_MEMBER = NA
         NO_EMPL_2004 = NA
         NO_EMPL_2020 = NA
         NO_DIS_EMPLOYEES = NA
         TYPE_DISABILITIES = NA
         NO_JOB_POSITION = NA
         REFERENCE = NA
         LATITUDE = NA
         LONGITUDE = NA
         hlavne_kategorie = mainCategories(rv_slpr$items)
         hlavne_kategorie_cpv = mainCategoriesCpv(rv_slpr$items)
         
         data = add_row(data, ID, TITLE, TITLE_BUSINESS, CITY, STREET_NAME, STREET_NUMBER, ZIP_CODE, EMAIL, PHONE, WEBSITE, SOCIAL_NETW_FB,
                        SOCIAL_NETW_OTHER, LEGAL_FORM, TYPE_RSP, ID_ORG, VAT, ID_VAT, IMAGE, REGISTR_DATE, REGION_ACTIVITY, DISTRICT_ACTIVITY,
                        OPEN_HOURS, OBJECTIVES, BUSINESS_CAT, BUSINESS_CAT_CPV, BUSINESS_DESC, BUSINESS_DESC_PRODUCTS_CPV, BUSINESS_DESC_SERVICES_CPV,
                        STORY, HIGHLIGHTS, ASOC_MEMBER, NO_EMPL_2004, NO_EMPL_2020, NO_DIS_EMPLOYEES, TYPE_DISABILITIES, NO_JOB_POSITION, REFERENCE,
                        LATITUDE, LONGITUDE, hlavne_kategorie, hlavne_kategorie_cpv)
         data = as.data.frame(data)
         
         rating = 1
         rating_num = 1
         image = "uni_logo.jpg"
         
         rat = add_row(rat, ID, TITLE, STORY, rating, rating_num, image)
         rat = as.data.frame(rat)
         
         write.xlsx(data, "Data/data.xlsx", row.names = FALSE)
         write.xlsx(rat, "Data/businesses_rating.xlsx", row.names = FALSE)
         
         updateTextInput(session, "nazov", value = "", label = "", placeholder = "Názov firmy")
         updateTextInput(session, "popis", value = "", label = "", placeholder = "Popis podniku")
         updateTextInput(session, "mesto", value = "", label = "", placeholder = "Mesto")
         updateTextInput(session, "ulica", value = "", label = "", placeholder = "Ulica")
         updateTextInput(session, "supisne_cislo", value = "", label = "", placeholder = "Súpisné číslo")
         updateTextInput(session, "psc", value = "", label = "", placeholder = "PSČ")
         updateTextInput(session, "email", value = "", label = "", placeholder = "E-mail")
         updateTextInput(session, "tel_cislo", value = "", label = "", placeholder = "Telefónne číslo")
         updateSelectInput(session, inputId = "rsp_id", selected = 0)
         updateSelectInput(session, inputId = "kategoria_podn", selected = 0)
         updateSelectInput(session, inputId = "slpr_ponuka", selected = 0)
         
         rv_kat$items = NULL
         rv_slpr$items = NULL
         rv_rsp$items = NULL
         
         if (!is.null(notif_mess_reg))
           return(NULL)
         
         notif_mess_reg = showNotification(paste("Registrácia bola úspešná."), duration = 2, type = "message")
         notif_mess_reg = NULL
       }else{
         if(titleCheck(input$nazov) == FALSE){
           updateTextInput(session, "nazov", value = "", placeholder = paste("Názov firmy musí mať minimálne 1 znak!"))
         }
         
         if(cityCheck(input$mesto) == FALSE){
           updateTextInput(session, "mesto", value = "", placeholder = paste("Názov mesta musí mať minimálne 1 znak!"))
         }
         
         if(streetCheck(input$ulica) == FALSE){
           updateTextInput(session, "ulica", value = "", placeholder = paste("Názov ulice musí mať minimálne 1 znak!"))
         }
         
         if(streetNumCheck(input$supisne_cislo) == FALSE){
           updateTextInput(session, "supisne_cislo", value = "", placeholder = paste("Popisné číslo musí mať minimálne 1 číslicu!"))
         }
         
         if(postalCodeCheck(input$psc) == FALSE){
           updateTextInput(session, "psc", value = "", placeholder = paste("PSČ musí mať presne 5 číslic!"))
         }
         
         if(emailCheck(input$email) == FALSE){
           updateTextInput(session, "email", value = "", placeholder = paste("E-mail musí byť v tvare a@b.c!"))
         }
         
         if(checkPhoneNum(input$tel_cislo) == FALSE){
           updateTextInput(session, "tel_cislo", value = "", placeholder = paste("Telefónne číslo musí mať 10 číslic!"))
         }
         
       }
     }) 
   })
  
  #GLobal search
   observeEvent(input$global_search, {
     data = read.xlsx("Data/data.xlsx")
     
     search_df = select(data, TITLE, BUSINESS_CAT, BUSINESS_DESC)
     
     search_df$TITLE = tolower(search_df$TITLE)
     search_df$BUSINESS_CAT = tolower(search_df$BUSINESS_CAT)
     search_df$BUSINESS_DESC = tolower(search_df$BUSINESS_DESC)
     
     res_vec = c()
     res = c()
     x = 1
     
     while (x <= ncol(search_df)) {
       res = which(grepl(tolower(input$global_search), search_df[,x], fixed = TRUE))
       
       if(length(res) != 0){
         res_vec = c(res_vec, res)
       }
       
       x = x + 1
     }
     
     res_vec = sort(unique(res_vec))
     data = data[res_vec,]
     result_data = data
     search_data = select(data, TITLE, BUSINESS_CAT, BUSINESS_DESC)
  
     output$datatable = DT::renderDataTable({
       DT::datatable(search_data, rownames = F, selection = "single", options = list(pageLength = 5,  dom = 't', scrollX = TRUE, searching=FALSE, filtering=FALSE, ordering = F),
                     colnames = c('Podnik' = 'TITLE', 'Kategória podnikania' = 'BUSINESS_CAT','Popis činnosti' = 'BUSINESS_DESC')) %>% formatStyle (columns = c(0,1,2),textAlign = "center", wordWrap = "break-word")
       }, server = FALSE)
     
     observeEvent(input$datatable_rows_selected, {
       row = input$datatable_rows_selected 
       
       output$datatable = DT::renderDataTable({
         DT::datatable(search_data, rownames = F, selection = "single", options = list(pageLength = 5,  dom = 't', scrollX = TRUE, searching=FALSE, filtering=FALSE, ordering = F),
                       colnames = c('Podnik' = 'TITLE', 'Kategória podnikania' = 'BUSINESS_CAT','Popis činnosti' = 'BUSINESS_DESC')) %>% formatStyle (columns = c(0,1,2),textAlign = "center", wordWrap = "break-word")
       }, server = FALSE)
       
       output$podnik_nadpis = renderText({
         paste(result_data[row, "TITLE"], "", sep = "")}
       )
       
       output$stars_ui = renderUI({
         rating = read_excel("Data/businesses_rating.xlsx")
         id = result_data[row, "ID"]
         
         res = (as.numeric(rating[rating$ID == id, "rating"]) / 5) * 100
         
         
         style_value = sprintf("width:%s%%", res)
         tags$div(class = "full-stars", style = style_value)
       })
       
       output$rating_number = renderUI({
         rating = read_excel("Data/businesses_rating.xlsx")
         id = result_data[row, "ID"]
         
         res = round(as.numeric(rating[rating$ID == id, "rating"]), digits = 2)
         
         div(res)
       })
       
       output$text_katalog = renderText({
         if(is.na(result_data[row, "STORY"]) == TRUE){
           paste("Nie je k dispozícii.", sep = "")
         }else{
           paste(result_data[row, "STORY"], "", sep = "")
         }
       })
       
       output$typ_podniku = renderText({
         if(is.na(result_data[row, "TYPE_RSP"]) == TRUE){
           paste("Nie je k dispozícii.", sep = "")
         }else{
           paste(result_data[row, "TYPE_RSP"], "", sep = "")
         }
       })
       
       output$adresa = renderText({
         paste(result_data[row, "STREET_NAME"], " ", result_data[row, "STREET_NUMBER"], ", ", result_data[row, "CITY"], ", ", result_data[row, "ZIP_CODE"], sep = "")}
       )
       
       output$gls_tel_cislo = renderText({
         if(is.na(result_data[row, "PHONE"]) == TRUE){
           paste("Nie je k dispozícii.", sep = "")
         }else{
           paste(result_data[row, "PHONE"], "", sep = "")
         }
       })
       
       output$gls_mail = renderText({
         if(is.na(result_data[row, "EMAIL"]) == TRUE){
           paste("Nie je k dispozícii.", sep = "")
         }else{
           paste(result_data[row, "EMAIL"], "", sep = "")
         }
       })
       
       output$gls_web = renderText({
         if(is.na(result_data[row, "WEBSITE"]) == TRUE){
           paste("Nie je k dispozícii.", sep = "")
         }else{
           paste(result_data[row, "WEBSITE"], "", sep = "")
         }
       })
       
       output$gls_slpr = renderText({
         paste(result_data[row, "BUSINESS_DESC"], "", sep = "")}
       )
       
       output$kategoria_nadpis = renderText({
         res = result_data[row, "hlavne_kategorie"]
         res = str_to_sentence(res)
         paste(res, "", sep = "")}
       )
       
       output$profile_produkty = DT::renderDataTable({
         data = read_excel("Data/data.xlsx")
         
         id = as.numeric(result_data[row, "ID"])
         
         res = filter(data, ID == id)
         
         produkty = as.vector(t(res$BUSINESS_DESC_PRODUCTS_CPV))
         sluzby = as.vector(t(res$BUSINESS_DESC_SERVICES_CPV))
         
         produkty = unique(unlist(str_split(produkty, pattern = "; ")))
         sluzby = unique(unlist(str_split(sluzby, pattern = "; ")))
         
         all = unique(c(produkty, sluzby))
         all = as.data.frame(na.omit(all))
         colnames(all) = c("cpv_code")
         
         dict = tail(dictionary, -8)
         
         all = merge(x = all, y = dict, by = "cpv_code", all.x = TRUE)
         all$cpv_code = NULL
         colnames(all) = c("Produkty a služby")
         
         DT::datatable(all)
       })
       
       updateTabsetPanel(session, "inTabset", selected = "profile_bus")
     })
   })
   
  #top_produkty <- read_xlsx("Data/top_10_products_example.xlsx")
  top_produkty <- read.csv("Data/top_10_products_example.csv", sep = ";")
  output$table2 <- renderReactable({
    reactable(top_produkty, resizable = TRUE, showPageSizeOptions = TRUE, sortable = FALSE,
              onClick = "expand", highlight = F,
              details = function(index) {
                if(index == 1){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "novesta.jpg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                } else if (index == 2){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "tampony.jpg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                } else if (index == 3){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "candies.jpg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                } else if (index == 4){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "coffee.jpg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                } else if (index == 5){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "candles.png" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                } else if (index == 6){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "decorations.jpeg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                } else if (index == 7){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "blanket.jpg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                } else if (index == 8){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "gaza.jpg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                } else if (index == 9){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "garden.jpeg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                } else if (index == 10){
                  div(class = "top-produkt",
                      div(class = "top-produkt-logo",
                          img(class = "top-produkt-logo-img", src = "prehoz.jpg" )
                      ),
                      div(class = "top-produkt-popis",
                          h3(class = "top-produkt-nazov", top_produkty[index, ]),
                          p("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")
                      )
                      
                  )
                  
                }
              },
              rowClass = "my-row")
    
  })
  
  data = read_excel("Data/data.xlsx")
  
  output$map = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 48.8, lng = 19.2, zoom = 7) %>%
      setMaxBounds(lng1 = -140, lat1 = -70, lng2 = 155, lat2 = 70 ) %>%
      addMarkers(data = data,
                 lng = ~LONGITUDE,
                 lat = ~LATITUDE,
                 popup = paste(
                   "<b>Názov: </b>",data$TITLE,"</b>","<br>",
                   "<b>Otváracie hodiny: </b>",data$OPEN_HOURS,"<br>",
                   "<b>Adresa: </b>",data$STREET_NAME," ",data$STREET_NUMBER,", ",data$CITY," ",data$ZIP_CODE,"<br>",
                   "<b>Telefonné číslo: </b>",data$PHONE,"<br>",
                   sep=""))
  })
  
  output$map_o_nas = renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 48.8, lng = 19.2, zoom = 7) %>%
      setMaxBounds(lng1 = -140, lat1 = -70, lng2 = 155, lat2 = 70 ) %>%
      addMarkers(data = data,
                 lng = ~LONGITUDE,
                 lat = ~LATITUDE,
                 popup = paste(
                   "<b>Názov: </b>",data$TITLE,"</b>","<br>",
                   "<b>Otváracie hodiny: </b>",data$OPEN_HOURS,"<br>",
                   "<b>Adresa: </b>",data$STREET_NAME," ",data$STREET_NUMBER,", ",data$CITY," ",data$ZIP_CODE,"<br>",
                   "<b>Telefonné číslo: </b>",data$PHONE,"<br>",
                   sep=""))
  })
  
})


