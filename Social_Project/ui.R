#### INSTALACIA KNIZNIC ###

# PROCEED BEFORE THE FIRST RUN OF THE APPLICATION #

#install.packages("markdown")
#install.packages("shinyWidgets")
#install.packages("dplyr")
#install.packages("leaflet")
#install.packages("ggplot2")
#install.packages("DT")
#install.packages("reactable")
#install.packages("shiny.router")
#install.packages("stringr")
#install.packages("data.table")
#install.packages("tibble")
#install.packages("readxl")
#install.packages("shinyjs")
# install.packages("sendmailR")
# install.packages("gdata")
# install.packages("stringi")
# install.packages("openxlsx")
 install.packages("sendmailR")
# install.packages("margittr")

# Nacitanie kniznic 
library(markdown)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(ggplot2)
library(DT)
library(reactable)
library(shiny.router)
library(stringr)
library(data.table)
library(tibble)
library(readxl)
library(shinyjs)
library(sendmailR)
library(gdata)
library(stringi)
library(openxlsx)
library(magrittr)

data = read_excel("Data/data.xlsx")
dictionary = read_excel("Data/main_dictionary.xlsx")
registracia_formular = read_excel("Data/unique-formular.xlsx")
reg_form = read_excel("Data/reg_form.xlsx")
slpr_kat = read_excel("Data/produkty-kategorie.xlsx")

shinyUI(fluidPage(id = "inTabset",
                  tags$link(rel = "stylesheet", type="text/css", href="style.css"),
                  navbarPage(id = "inTabset", title=div((img(class = "logo", src="logo.png"))),
                             # Zalozka DOMOV v navigacnom paneli
                             tabPanel("DOMOV", value = "homepage", 
                                      # 1. fluidRow - prva strana homepage
                                      fluidRow(class = "header-banner", 
                                               column(class = "herobanner", 12,
                                                      div(class="herobanner-header",
                                                          h1(class = "herobanner-header-nadpis", "Sociálne podniky na jednom mieste"),
                                                          p(class = "herobanner-header-text", "Na tejto stránke nájdete veľké množstvo sociálnych podnikov, ich ponuku služieb a výrobkov, či ponuku voľných pracovných miest."),
                                                          div(class="herobanner-button-block",actionButton("button_jump_registracia", "REGISTRÁCIA PODNIKU"))
                                                      )
                                                      
                                               )
                                               
                                      ),
                                      fluidRow(class = "search-results",
                                              h2(class = "search-title","Vyhľadávanie"),
                                              textInput("global_search", label = "", placeholder = ""),
                                              h2(class = "search-results-title", "Výsedky vyhľadávania"),
                                              div(class = "search-res-table", DT::dataTableOutput('datatable',width = "90%"))
                                              
                                      ),
                                      
                                      #2. fluidRow - druha strana homepage - TOP 10 PODNIKOV 
                                      fluidRow(class = "top-10-podnikov", 
                                               column(12,
                                                      # header TOP 10 PODNIKOV
                                                      div(class="main-page",
                                                          h1("TOP 10 PODNIKOV")),
                                                      # TOP PODNIKY - tabulka, logo, popis
                                                      div(class = "top-podniky",
                                                          reactableOutput('table')
                                                      )
                                               ),
                                               
                                               
                                      ),
                                      
                                      #3. fluidRow - tretia strana homepage - TOP 10 PRODUKTOV 
                                      fluidRow(class = "top-10-produktov",
                                               column(12,
                                                      # header TOP 10 PRODUKTOV
                                                      div(class="main-page",
                                                          h1("TOP 10 PRODUKTOV")),
                                                      # TOP PRODUKTY - tabulka, logo, popis
                                                      div(class = "top-produkty",
                                                          reactableOutput('table2')
                                                      )
                                               ),
                                               
                                               
                                      ),
                                      #4. fluidRow - stvrta strana homepage - mapa podnikov
                                      fluidRow(class = "mapa-podnikov",
                                               column(12,
                                                      div(class="main-page",
                                                          h1(class = "mapa-podnikov-header","MAPA PODNIKOV"),),
                                                      
                                                      div(class= "mapa",
                                                          leafletOutput("map"))
                                                      
                                               )
                                               
                                      ),
                                      fluidRow(class = "footer",
                                               column(class="foot-left",2,
                                                      img(class = "logo-foot", src = "biele.png" )
                                               ),
                                               
                                               column(class="footer-adresa",2,
                                                      p(class= "footer-adresa-text", "Jánošíkova 123, 040 01 Košice info@stovi.sk")
                                               ),
                                               column(class="footer-odkazy",5,
                                                      div(class="footer-odkazy-text",
                                                          a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "FACEBOOK"),
                                                          a(class="odkazy-soc-siete", href = "https://www.twitter.com/", "TWITTER"),
                                                          a(class="odkazy-soc-siete", href = "https://www.instagram.com/", "INSTAGRAM"),
                                                          a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "KONTAKT")
                                                      )
                                               ),
                                               column(class="foot-right",3,
                                                      div(class="footer-copyright",
                                                          div(class="footer-copyright-obrazky",
                                                              img(class= "soc-siete", src = "facebook.png" ),
                                                              img(class= "soc-siete", src = "instagram.png" ),
                                                              img(class= "soc-siete", src = "twitter.png" )
                                                          ),
                                                          p(class = "soc-siete-text","© 2021 Stovi. All rights reserved")
                                                      )
                                               )
                                               
                                               
                                      )
                                      
                             ),
                             
                             
                             # Zvysne zalozky navigacneho panelu
                             tabPanel("O NÁS", 
                                      fluidRow(class = "o-nas-banner", 
                                               column(class = "o-nas-background", 12,
                                                      div(class = "o-nas",
                                                          h1(class = "o-nas-nadpis", "O NÁS")
                                                      )   
                                                      
                                                      
                                               )
                                      ),
                                      fluidRow(class = "o-nas-popis-projekt",
                                               column(class = "o-nas-popis-left", 6,
                                                      div(class = "o-nas-popis-text",
                                                          h1(class = "o-nas-popis-nadpis", "Projekt STOVI"),
                                                          p(class = "o-nas-popis-text-left","Za projektom STOVI stoja študenti 4. ročníka Hospodárskej informatiky na Technickej univerzite v Košiciach. Projekt vznikol v rámci predmetu „Elektronické obchodovanie“. Hlavným cieľom tohto projektu bolo zviditeľniť sociálne podniky a ich produkty a služby.")
                                                      )   
                                                      
                                                      
                                               ),
                                               column(class = "o-nas-popis-right", 6,
                                                      img(class = "o-nas-logo", src = "o-nas-logo2.png"),
                                                      # p(class = "o-nas-logo-popis", "Viac ako STO podnikov na jednom mieste")
                                                      p(class = "o-nas-logo-popis",
                                                        span("Viac ako "),
                                                        span(class = "sto-farebne" , "STO "),
                                                        span("podnikov na jednom mieste")
                                                        
                                                      )   
                                                      
                                                      
                                               )
                                               
                                      ),
                                      fluidRow(class = "o-nas-popis-projekt-bottom",
                                               column(class = "o-nas-popis-bottom", 12,
                                                      p(class = "o-nas-popis-bottom-text", " Na tejto stránke nájdete veľké množstvo podnikov z rôznych odvetví z celého Slovenska. Denne aktualizujeme informácie, ktoré sú pre úspešné podnikanie prvoradé a nevyhnutné. Našim cieľom je Vaša spokojnosť a Váš úspech.")
                                                      
                                               )
                                               
                                      ),
                                      fluidRow(
                                        column(12,
                                               fluidRow(
                                                 column(12,
                                                        div(class = "main-page",
                                                            h1("Prečo práve sociálne podniky?")
                                                        )
                                                        
                                                 )
                                                 
                                               ),
                                               fluidRow(
                                                 column(class = "o-nas-preco-logo-pozadie", 12,
                                                        p(class = "o-nas-preco-text-right", "Hlavným dôvodom mediálnej podpory sociálnych podnikov je najmä vytvorenie priaznivého prostredia pre rozvoj sociálnej ekonomiky, ako aj vytváranie pozitívnych väzieb a vzťahov k sociálnym podnikom. Pretože nedostatočné prezentovanie zámeru sociálnych podnikov v spoločnosti môže mať dopad na spomalenie rozvoja sociálneho podnikania. Ďalším dôvodom, prečo sme si zvolili sociálne podniky je to, že podporujú zamestnanosť znevýhodnených skupín obyvateľstva. Propagácia týchto podnikov nie je na internete dostačujúca, nakoľko tieto podniky nemajú vysoký kapitál.")
                                                 )
                                                 
                                               )
                                        )
                                      ),
                                      
                                      fluidRow(class = "mapa-podnikov",
                                               column(12,
                                                      div(class="main-page",
                                                          h1(class = "mapa-podnikov-header","MAPA PODNIKOV"),),
                                                      
                                                      div(class= "mapa",
                                                          leafletOutput("map_o_nas"))
                                                      
                                               )
                                               
                                      ),
                                      fluidRow(class = "footer",
                                               column(class="foot-left",2,
                                                      img(class = "logo-foot", src = "biele.png" )
                                               ),
                                               
                                               column(class="footer-adresa",2,
                                                      p(class= "footer-adresa-text", "Jánošíkova 123, 040 01 Košice info@stovi.sk")
                                               ),
                                               column(class="footer-odkazy",5,
                                                      div(class="footer-odkazy-text",
                                                          a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "FACEBOOK"),
                                                          a(class="odkazy-soc-siete", href = "https://www.twitter.com/", "TWITTER"),
                                                          a(class="odkazy-soc-siete", href = "https://www.instagram.com/", "INSTAGRAM"),
                                                          a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "KONTAKT")
                                                      )
                                               ),
                                               column(class="foot-right",3,
                                                      div(class="footer-copyright",
                                                          div(class="footer-copyright-obrazky",
                                                              img(class= "soc-siete", src = "facebook.png" ),
                                                              img(class= "soc-siete", src = "instagram.png" ),
                                                              img(class= "soc-siete", src = "twitter.png" )
                                                          ),
                                                          p(class = "soc-siete-text","© 2021 Stovi. All rights reserved")
                                                      )
                                               )
                                               
                                               
                                      )
                                      
                                      
                             ),
                             
                             
                             tabPanel(value = "tab_registracia","REGISTRÁCIA PODNIKU",
                                      
                                      #1.
                                      fluidRow(class = "registracia-podniku-firstrow",
                                               column(10,
                                                      column(4,class = "slpec",
                                                             # header first column
                                                             div(class = "registracia-column-header",
                                                                 h2(class="podnadpis-registracia","Prečo registrovať podnik na STOVI.sk?")
                                                             ),
                                                             
                                                             
                                                             wellPanel(class = "registracia-wellpanel",
                                                                       h4(class = "h4-popis","Registrujte sa aj vy! Registrácia je jednoduchá a rýchla.
                                                   Predstavte svoj podnik širokej verejnosti."),
                                                                       
                                                             ),
                                                             div(class="registracia-button-block",onClick="document.getElementById('druha').scrollIntoView();",actionButton("button", "Čítať viac"))
                                                      ),   
                                                      column(4,class = "slpec",
                                                             # header second column
                                                             div(class = "registracia-column-header",
                                                                 h2(class="podnadpis-registracia","Proces schválenia registrácie")),
                                                             
                                                             
                                                             wellPanel(class = "registracia-wellpanel",
                                                                       h4(class = "h4-popis","Proces schválenia registrácie je rýchly!"
                                                                       ),
                                                             ),
                                                             div(class="registracia-button-block",onClick="document.getElementById('tretia').scrollIntoView();",actionButton("button", "Čítať viac"))
                                                      ),  
                                                      
                                                      column(4,class = "slpec",
                                                             # header third column
                                                             div(class = "registracia-column-header",
                                                                 h2(class="podnadpis-registracia","Registrácia podniku")),
                                                             
                                                             
                                                             wellPanel(class = "registracia-wellpanel",
                                                                       h4(class = "h4-popis","Vďaka jednoduchej registrácii a promptnému procesu schválenia registrácie, váš podnik zviditeľnite rýchlo."),
                                                             ),
                                                             div(class="registracia-button-block",onClick="document.getElementById('stvrta').scrollIntoView();",actionButton("button", "Čítať viac"))
                                                      ),   
                                                      
                                               ),
                                               
                                               
                                      ),
                                      
                                      #2.
                                      fluidRow(
                                        column(10,
                                               # header second section
                                               div(class = "registracia-podniku-secondrow",  id='druha',
                                                   h1("Prečo registrovať podnik na STOVI.sk?")),
                                               
                                               wellPanel(class = "registracia-preco-wellpanel wellpanel",
                                                         
                                                         h4(class = "secondrow-vrchna-cast", "Vďaka jednoduchej registrácii a promptnému procesu schválenia registrácie, váš podnik zviditeľnite rýchlo.
                                                             Táto stránka je zameraná na propagáciu sociálnych podnikov.
                                                             Pre podniky je veľmi efektívne, ak ich ľudia vedia nájsť cez internet.
                                                             Veď ako sa vraví: „V súčasnosti kto nie je na internete, akoby ani nebol.“"),
                                                         
                                                         column(7,class = "secondrow-lavy-slpec", 
                                                                h2(class="nadpis-lavy-stlpec", "Výhody registrácie"),
                                                                tags$ul(class ="zoznam",
                                                                        tags$li("povedomie o vašej firme sa rozšíri po celom Slovensku"), 
                                                                        tags$li("zvýšenie šance na získanie nových zákazníkov"), 
                                                                        tags$li("prostredníctvom funkcie „Ponuka práce“ môžete získať aj nových zamestnancov")
                                                                ),
                                                                
                                                                
                                                         ),
                                                         
                                                         
                                                         column(5,class = "secondrow-pravy-slpec", 
                                                                h4(class = "nadpis-pravy-stlpec","Pridaj sa aj ty k nám!"), 
                                                                div(class="registracia2-button-block",actionButton("button", "Katalóg podnikov"))
                                                         ),
                                               ),
                                               
                                        ),
                                        
                                      ),
                                      
                                      #3.
                                      fluidRow(class = "registracia-podniku-thirdrow", id='tretia',
                                               column(12,
                                                      
                                                      h1(class="thirdrow-header","Proces schválenia registrácie"),
                                                      wellPanel(class = "registracia-proces-wellpanel wellpanel",
                                                                
                                                                column(6,
                                                                       div(class = "proces1 proces",
                                                                           h2(class="tabulka-popis","Vyplnenie a zaslanie žiadosti o registráciu.")
                                                                       ),
                                                                       
                                                                       div(class = "proces2 proces",
                                                                           h2(class="tabulka-popis","Overenie nutných podmienok")
                                                                       ),
                                                                ), 
                                                                
                                                                column(6,class = "proces-slpec2",
                                                                       div(class = "proces3 proces",
                                                                           h2(class="tabulka-popis","Ak nejakú podmienku nespĺňate, budeme vás informovať e-mailom o zamietnutí registrácie.")
                                                                       ),
                                                                       
                                                                       div(class = "proces4 proces",
                                                                           h2(class="tabulka-popis","Hotovo!")
                                                                       ),
                                                                ),  
                                                                
                                                      ),
                                               ),
                                      ),
                                      
                                      #4. fluidRow - FORMULAR REGISTROVAŤ PODNIK
                                      fluidRow(class = "registracia-podniku-fourthrow", id='stvrta',
                                               column(12,
                                                      h1(class="fourthrow-header mapa-podnikov-header","REGISTROVAŤ PODNIK"),
                                                      wellPanel(class = "registracia-formular-wellpanel wellpanel",
                                                          
                                                        div(class= "top-formular",           
                                                            column(7, class = "registracia-stlpec1",
                                                               textInput("nazov", label = "", placeholder = "Názov firmy"),
                                                               textInput("ulica", label = "", placeholder = "Ulica"),
                                                               textInput("psc", label = "", placeholder = "PSČ"),
                                                            
                                                              
                                                             
                                                              checkboxGroupInput(inputId = "rsp_id", label = "Typ soc. podniku *", 
                                                                                   choices = c("integračný", "sociálny", "všeobecný", "verejnoprospešný"),
                                                                                   inline = TRUE,
                                                              ), 
                                                              
                                                           
                                                              selectInput(
                                                                  inputId="kategoria_podn",
                                                                  label = "Kategória podnikania *",
                                                                  choices = unique(c(registracia_formular$BUSINESS_CAT)),
                                                                  
                                                                  multiple = TRUE
                                                                  
                                                                )
                                                             
                                                            ),
                                                                
                      
                                                            column(7, class="registracia-stlpec2",
                                                                 
                                                                   textInput("mesto", label = "", placeholder = "Mesto"),
                                                                   textInput("supisne_cislo", label = "",  placeholder = "Súpisné číslo"),
                                                                   textInput("email", label = "",  placeholder = "E-mail"),
                                                                   textInput("tel_cislo", label = "",  placeholder = "Telefónne číslo"),
                                                                  
                                                                  
                                                                  selectInput(
                                                                    inputId="slpr_ponuka",
                                                                    label = "Ponuka služieb a produktov *",
                                                                    choices = unique(reg_form$produkty),
                                                                    
                                                                    multiple = TRUE
                                                                    
                                                                  
                                                                  )   
                                                            ),
                                                        ),
                                                            tags$br(),
                                                            textInput("popis", label = "", placeholder = "Popis podniku"),
                                                            span(id = "mnd","*"), span(id = "mnd-txt", "Potrebné zvoliť aspoň 1 z možností!"),
                                                            div(class="registracia-formular-button-block",actionButton("registration_button", "Odoslať")),
                                                          ),
                                               ) 
                                      ),
                                      
                                      #Footer
                                      fluidRow(class = "footer",
                                               column(class="foot-left",2,
                                                      img(class = "logo-foot", src = "biele.png" )
                                               ),
                                               
                                               column(class="footer-adresa",2,
                                                      p(class= "footer-adresa-text", "Jánošíkova 123, 040 01 Košice info@stovi.sk")
                                               ),
                                               column(class="footer-odkazy",5,
                                                      div(class="footer-odkazy-text",
                                                          a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "FACEBOOK"),
                                                          a(class="odkazy-soc-siete", href = "https://www.twitter.com/", "TWITTER"),
                                                          a(class="odkazy-soc-siete", href = "https://www.instagram.com/", "INSTAGRAM"),
                                                          a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "KONTAKT")
                                                      )
                                               ),
                                               column(class="foot-right",3,
                                                      div(class="footer-copyright",
                                                          div(class="footer-copyright-obrazky",
                                                              img(class= "soc-siete", src = "facebook.png" ),
                                                              img(class= "soc-siete", src = "instagram.png" ),
                                                              img(class= "soc-siete", src = "twitter.png" )
                                                          ),
                                                          p(class = "soc-siete-text","© 2021 Stovi. All rights reserved")
                                                      )
                                               )
                                               
                                               
                                      )
                             ),
                             
                             
                             navbarMenu("KATALÓGY", 
                                        tabPanel("Katalóg podnikov", value = "panel1",
                                                 shinyjs::useShinyjs(),
                                                 
                                                 tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                                                 
                                                 fluidRow(class = "center-div",
                                                          fluidRow(
                                                            column(12,
                                                                   div(class="main-page",
                                                                       h1(class = "herobanner-header-nadpis", "KATALÓG PODNIKOV")
                                                                   )
                                                            )
                                                          ),
                                                          
                                                          fluidRow(class = "riadok-tlacidiel",
                                                                   column(3,class = "nadpis-kategorie",
                                                                          useShinyjs(),
                                                                          div(
                                                                            p(h4("Potraviny")),
                                                                            tags$button(
                                                                              class = "button-kategorie",
                                                                              id = "web_button",
                                                                              tags$a(onclick="fakeClick('panel2')", tags$img(id = "my_img1", src = "potraviny.jpg", class = "button-image"))
                                                                            )
                                                                          )
                                                                          
                                                                   ),
                                                                   column(3, class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Hotelové a reštauračné služby")),
                                                                            tags$button(
                                                                              class = "button-kategorie",
                                                                              id = "web_button",
                                                                              tags$a(onclick="fakeClick('panel2')", tags$img(id = "my_img2", src = "hotely.jpg", class = "button-image"))
                                                                            )
                                                                          )),
                                                                   column(3, class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Odevy, obuv a textil")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel2')", tags$img(id = "my_img3", src = "odevy.jpg", class = "button-image"))
                                                                            )
                                                                          )),
                                                                   column(3,class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Kozmetika a zdravie")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel2')", tags$img(id = "my_img4", src = "kozmetika.jpg", class = "button-image"))
                                                                            )
                                                                          )
                                                                   )
                                                          ),
                                                          
                                                          fluidRow(class = "riadok-tlacidiel",
                                                                   column(3,class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Stavbári a remeselníci")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel2')", tags$img(id = "my_img5", src = "stavbari.jpg", class = "button-image"))
                                                                            )
                                                                          )
                                                                          
                                                                   ),
                                                                   column(3, class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Poľnohospodárstvo a priemysel")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel2')", tags$img(id = "my_img6", src = "polno.jpg", class = "button-image"))
                                                                            )
                                                                          )),
                                                                   column(3, class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Nábytok a bývanie")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel2')", tags$img(id = "my_img7", src = "nabytok.jpg", class = "button-image"))
                                                                            )
                                                                          )),
                                                                   column(3,class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Iné")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel2')", tags$img(id = "my_img8", src = "ine.jpg", class = "button-image"))
                                                                            )
                                                                          )
                                                                   )
                                                          ),
                                                          
                                                          fluidRow(class = "spodok",
                                                                   column(4,
                                                                          div(
                                                                            p(h2("Nenašli ste?", class = "podnadpis")),
                                                                            
                                                                          )
                                                                          
                                                                   ),
                                                                   column(4, 
                                                                          div(
                                                                            p(h3("Ak ste nenašli podnik, ktorý hľadáte, dajte nám vedieť, aby sme mohli rozšíriť náš katalóg podnikov.", class = "text-katalog")),
                                                                            
                                                                          )),
                                                                   column(4, 
                                                                          div(
                                                                            actionButton("button_jump_kontakt", "ODPORÚČTE"),
                                                                            
                                                                          ))
                                                                   
                                                          ),
                                                 ),
                                                 
                                                 #Footer
                                                 fluidRow(class = "footer",
                                                          column(class="foot-left",2,
                                                                 img(class = "logo-foot", src = "biele.png" )
                                                          ),
                                                          
                                                          column(class="footer-adresa",2,
                                                                 p(class= "footer-adresa-text", "Jánošíkova 123, 040 01 Košice info@stovi.sk")
                                                          ),
                                                          column(class="footer-odkazy",5,
                                                                 div(class="footer-odkazy-text",
                                                                     a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "FACEBOOK"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.twitter.com/", "TWITTER"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.instagram.com/", "INSTAGRAM"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "KONTAKT")
                                                                 )
                                                          ),
                                                          column(class="foot-right",3,
                                                                 div(class="footer-copyright",
                                                                     div(class="footer-copyright-obrazky",
                                                                         img(class= "soc-siete", src = "facebook.png" ),
                                                                         img(class= "soc-siete", src = "instagram.png" ),
                                                                         img(class= "soc-siete", src = "twitter.png" )
                                                                     ),
                                                                     p(class = "soc-siete-text","© 2021 Stovi. All rights reserved")
                                                                 )
                                                          )
                                                          
                                                          
                                                 )
                                                 
                                                 
                                        ),
                                        
                                        
                                        tabPanel("Katalóg produktov", value = "pan_kat_prod",
                                                 shinyjs::useShinyjs(),
                                                 
                                                 tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                                                 
                                                 fluidRow(class = "center-div",
                                                          fluidRow(
                                                            column(12,
                                                                   div(class="main-page",
                                                                       h1(class = "herobanner-header-nadpis", "KATALÓG PRODUKTOV")
                                                                   )
                                                            )
                                                          ),
                                                          
                                                          fluidRow(class = "riadok-tlacidiel",
                                                                   column(3,class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Potraviny")),
                                                                            tags$button(
                                                                              class = "button-kategorie",
                                                                              id = "web_button",
                                                                              tags$a(onclick="fakeClick('panel4')", tags$img(id = "my_img9", src = "potraviny.jpg", class = "button-image"))
                                                                            )
                                                                          )
                                                                          
                                                                   ),
                                                                   column(3, class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Hotelové a reštauračné služby")),
                                                                            tags$button(
                                                                              class = "button-kategorie",
                                                                              id = "web_button",
                                                                              tags$a(onclick="fakeClick('panel4')", tags$img(id = "my_img10", src = "hotely.jpg", class = "button-image"))
                                                                            )
                                                                          )),
                                                                   column(3, class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Odevy, obuv a textil")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel4')", tags$img(id = "my_img11", src = "odevy.jpg", class = "button-image"))
                                                                            )
                                                                          )),
                                                                   column(3,class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Kozmetika a zdravie")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel4')", tags$img(id = "my_img12", src = "kozmetika.jpg", class = "button-image"))
                                                                            )
                                                                          )
                                                                   )
                                                          ),
                                                          
                                                          fluidRow(class = "riadok-tlacidiel",
                                                                   column(3,class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Stavbári a remeselníci")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel4')", tags$img(id = "my_img13", src = "stavbari.jpg", class = "button-image"))
                                                                            )
                                                                          )
                                                                          
                                                                   ),
                                                                   column(3, class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Poľnohospodárstvo a priemysel")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel4')", tags$img(id = "my_img14", src = "polno.jpg", class = "button-image"))
                                                                            )
                                                                          )),
                                                                   column(3, class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Nábytok a bývanie")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel4')", tags$img(id = "my_img15", src = "nabytok.jpg", class = "button-image"))
                                                                            )
                                                                          )),
                                                                   column(3,class = "nadpis-kategorie",
                                                                          div(
                                                                            p(h4("Iné")),
                                                                            tags$button(
                                                                              id = "web_button",
                                                                              class = "button-kategorie",
                                                                              tags$a(onclick="fakeClick('panel4')", tags$img(id = "my_img16", src = "ine.jpg", class = "button-image"))
                                                                            )
                                                                          )
                                                                   )
                                                          ),
                                                          
                                                          fluidRow(class = "spodok",
                                                                   column(4,
                                                                          div(
                                                                            p(h2("Nenašli ste?", class = "podnadpis")),
                                                                            
                                                                          )
                                                                          
                                                                   ),
                                                                   column(4, 
                                                                          div(
                                                                            p(h3("Ak ste nenašli produkt, ktorý hľadáte, dajte nám vedieť, aby sme mohli rozšíriť náš katalóg produktov.", class = "text-katalog")),
                                                                            
                                                                          )),
                                                                   column(4, 
                                                                          div(
                                                                            actionButton("button", "ODPORÚČTE"),
                                                                            
                                                                          ))
                                                                   
                                                          ),
                                                 ),
                                                 
                                                 #Footer
                                                 fluidRow(class = "footer",
                                                          column(class="foot-left",2,
                                                                 img(class = "logo-foot", src = "biele.png" )
                                                          ),
                                                          
                                                          column(class="footer-adresa",2,
                                                                 p(class= "footer-adresa-text", "Jánošíkova 123, 040 01 Košice info@stovi.sk")
                                                          ),
                                                          column(class="footer-odkazy",5,
                                                                 div(class="footer-odkazy-text",
                                                                     a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "FACEBOOK"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.twitter.com/", "TWITTER"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.instagram.com/", "INSTAGRAM"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "KONTAKT")
                                                                 )
                                                          ),
                                                          column(class="foot-right",3,
                                                                 div(class="footer-copyright",
                                                                     div(class="footer-copyright-obrazky",
                                                                         img(class= "soc-siete", src = "facebook.png" ),
                                                                         img(class= "soc-siete", src = "instagram.png" ),
                                                                         img(class= "soc-siete", src = "twitter.png" )
                                                                     ),
                                                                     p(class = "soc-siete-text","© 2021 Stovi. All rights reserved")
                                                                 )
                                                          )
                                                          
                                                          
                                                 )),
                                        
                                        tabPanel(title = "Panel2", value = "panel2", 
                                                 actionButton('jumpToBuss', 'Späť na katalóg'),
                                                 textOutput("podnik_nadpiss"),
                                                 DT::dataTableOutput("daatatable")),
                                        
                                        tabPanel(title = "Panel4", value = "panel4", 
                                                 actionButton('jumpToProducts', 'Späť na katalóg'),
                                                 textOutput("produkt_nadpiss"),
                                                 DT::dataTableOutput("daaatatable")),
                                        
                                        tabPanel(title = "Profile", value = "profile_bus", 
                                                 
                                                 fluidRow(class = "o-nas-banner", 
                                                          column(class = "podnik-background-image", 12,
                                                                 div(class = "o-nas",
                                                                     textOutput("kategoria_nadpis") %>% 
                                                                       tagAppendAttributes(class = "kategoria-nadpis", style= "text-align: center")
                                                                 )   
                                                                 
                                                                 
                                                          )
                                                 ), 
                                                 
                                                 fluidRow(class = "podnik-info-fluidrow", 
                                                          column(5,
                                                                 actionButton("btn_bt_cat", "Späť", style = "margin-top: 20px;"),
                                                                 div(class = "lavy_podnik",
                                                                     textOutput("podnik_nadpis") %>% 
                                                                       tagAppendAttributes(class = 'podnik-nadpis'),
                                                                    
                                                                     
                                                                     tags$div(class = "ratings",
                                                                              tags$div(class = "empty-stars",
                                                                                       uiOutput("stars_ui"),
                                                                              )
                                                                     ),
                                                                     div(style = "text-align: center", 
                                                                         h4(style="display:inline-block","Celkové hodnotenie: "),
                                                                         uiOutput(style="display:inline-block;font-weight: bold;font-size: 20px;","rating_number")
                                                                     ),
                                                                     div(style = "padding-top: 50px;" ,
                                                                         h3("Ohodnoťte tento podnik:"),   
                                                                         div(class = "slider",
                                                                              sliderInput("slider_hodnotenie", "", min = 0, max = 5, step = 0.5, value = 3)
                                                                         ),
                                                                         checkboxGroupInput("check_rating", "Skutočne chcete odoslať hodnotenie?", choices=c("Áno"), selected=NULL),
                                                                         actionButton("btn_rating", "Odoslať hodnotenie"),
                                                                         
                                                                     )
                                                                     
                                                                 )
                                                                 
                                                          ),
                                                          column(7,
                                                                 h2(class = "podnadpisy","Popis"),
                                                                 textOutput("text_katalog") %>% 
                                                                   tagAppendAttributes(class = "text-podnik", style= "text-align: center"),
                                                                 
                                                                 div(style= "padding-top: 15px;", 
                                                                     h3(class = "podnadpisy", style = "display: inline","Typ podniku: "),
                                                                     textOutput("typ_podniku") %>% 
                                                                       tagAppendAttributes(class = "text-podnik"),
                                                                 ),
                                                                 
                                                                 h2(class = "podnadpisy", "Produkty a služby"),
                                                                 textOutput("gls_slpr") %>% 
                                                                   tagAppendAttributes(class = "text-podnik"),
                                                                 
                                                                 h2(class = "podnadpisy","Kontakt"),
                                                                 fluidRow(style = "background-color: white;",
                                                                          column(6,
                                                                                 img(class = "kontakt-image", src = ("registracia_icon.png")),
                                                                                 textOutput("adresa") %>% 
                                                                                   tagAppendAttributes(class = "text-podnik", style = "display: inline;")
                                                                          ),
                                                                          column(6,
                                                                                 div(h3(class = "text-podnik", style= "display: inline","Tel. číslo: "),
                                                                                     textOutput("gls_tel_cislo") %>% 
                                                                                       tagAppendAttributes(class = "text-podnik", style = "display: inline;")
                                                                                 ),
                                                                                 div(h3(class = "text-podnik",style= "display: inline","Email: "),
                                                                                     textOutput("gls_mail") %>% 
                                                                                       tagAppendAttributes(class = "text-podnik", style = "display: inline;")
                                                                                 ),
                                                                                 div(h3(class = "text-podnik",style= "display: inline","Webová stránka: "),
                                                                                     textOutput("gls_web") %>% 
                                                                                       tagAppendAttributes(class = "text-podnik", style = "display: inline;")
                                                                                 )
                                                                                 
                                                                          ),
                                                                 ),
                                                                 
                                                          )
                                                 ),
                                                 
                                                 fluidRow(class = "fluidrow_produkty_podniku",
                                                          h2(style = "font-weight: bold", "Produkty a služby podniku:"),
                                                          br(),
                                                          DT::dataTableOutput("profile_produkty")
                                                 ),
                                                 
                                                 #Footer
                                                 fluidRow(class = "footer",
                                                          column(class="foot-left",2,
                                                                 img(class = "logo-foot", src = "biele.png" )
                                                          ),
                                                          
                                                          column(class="footer-adresa",2,
                                                                 p(class= "footer-adresa-text", "Jánošíkova 123, 040 01 Košice info@stovi.sk")
                                                          ),
                                                          column(class="footer-odkazy",5,
                                                                 div(class="footer-odkazy-text",
                                                                     a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "FACEBOOK"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.twitter.com/", "TWITTER"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.instagram.com/", "INSTAGRAM"),
                                                                     a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "KONTAKT")
                                                                 )
                                                          ),
                                                          column(class="foot-right",3,
                                                                 div(class="footer-copyright",
                                                                     div(class="footer-copyright-obrazky",
                                                                         img(class= "soc-siete", src = "facebook.png" ),
                                                                         img(class= "soc-siete", src = "instagram.png" ),
                                                                         img(class= "soc-siete", src = "twitter.png" )
                                                                     ),
                                                                     p(class = "soc-siete-text","© 2021 Stovi. All rights reserved")
                                                                 )
                                                          )
                                                          
                                                          
                                                 )
                                        )
                             ),
                             tabPanel(value = "tab_kontakt", "KONTAKT",
                                      fluidRow(class = "kontakt-firstrow",
                                               tags$head(
                                                 tags$style(
                                                   HTML(".shiny-notification {
                                                            position:fixed;
                                                            top: 45%;
                                                            left: 45%;
                                                            background-color: rgba(0, 255, 0, 0.3) !important;
                                                            color: black;
                                                            font-weight: bold;
                                                            padding: 50px 50px;
                                                            border-radius: 25px;
                                                          }
                                                        "
                                                   )
                                                 )
                                               ),
                                               column(12,
                                                  column(8,class = "kontakt-left", 
                                                      h1(class="kontakt-header","KONTAKT"),
                                                      wellPanel(class = "kontakt-wellpanel wellpanel",
                                                                
                                                                textInput("meno", label = "", placeholder = "*Zadajte vaše meno"),
                                                                textInput("email_from", label = "",  placeholder = "*Zadajte vašu e-mailovú adresu"),
                                                                textInput("text", label = "",  placeholder = "*Text"),
                                                                div(class="registracia-formular-button-block",actionButton("button_contact", "Odoslať")),
                                                    ), 
                                                  ),
                                                  
                                                   column(4,class = "kontakt-right",
                                                      wellPanel(class = "kontakt-right-wellpanel1 wellpanel kontakty-wellpanel",
                                                              img(class = "kontakt-obrazky", src = "kontakty-point.png"),
                                                              p(h3(class = "kontakt-wellpanel-header","SÍDLO")),
                                                              p(class = "kontakt-wellpanel-udaje","123 Jánošikova, 040 01 Košice"),
                                                                
                                                      ),
                                                      
                                                      wellPanel(class = "kontakt-right-wellpanel2 wellpanel kontakty-wellpanel",
                                                              img(class = "kontakt-obrazky", src = "kontakty-phone.png"),  
                                                              p(h3(class = "kontakt-wellpanel-header","TELEFÓN")),
                                                              p(class = "kontakt-wellpanel-udaje","+421 978 742 333"),
                                                      ),
                                                      
                                                      wellPanel(class = "kontakt-right-wellpanel3 wellpanel kontakty-wellpanel", 
                                                              img(class = "kontakt-obrazky", src = "kontakty-mail.png"),
                                                              p(h3(class = "kontakt-wellpanel-header","E-MAIL")), 
                                                              p(class = "kontakt-wellpanel-udaje","janko.hrasko@gmail.com"),
                                                      ),
                                                    ),  
                                                                
                                                  
                                               ),
                                      ),
                                      
                                      
                                      
                                      fluidRow(class = "footer",
                                               column(class="foot-left",2,
                                                      img(class = "logo-foot", src = "biele.png" )
                                               ),
                                               
                                               column(class="footer-adresa",2,
                                                      p(class= "footer-adresa-text", "Jánošíkova 123, 040 01 Košice info@stovi.sk")
                                               ),
                                               column(class="footer-odkazy",5,
                                                      div(class="footer-odkazy-text",
                                                          a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "FACEBOOK"),
                                                          a(class="odkazy-soc-siete", href = "https://www.twitter.com/", "TWITTER"),
                                                          a(class="odkazy-soc-siete", href = "https://www.instagram.com/", "INSTAGRAM"),
                                                          a(class="odkazy-soc-siete", href = "https://www.facebook.com/", "KONTAKT")
                                                      )
                                               ),
                                               column(class="foot-right",3,
                                                      div(class="footer-copyright",
                                                          div(class="footer-copyright-obrazky",
                                                              img(class= "soc-siete", src = "facebook.png" ),
                                                              img(class= "soc-siete", src = "instagram.png" ),
                                                              img(class= "soc-siete", src = "twitter.png" )
                                                          ),
                                                          p(class = "soc-siete-text","© 2021 Stovi. All rights reserved")
                                                      )
                                               )
                                               
                                               
                                      )
                             )
                  )
)
)
