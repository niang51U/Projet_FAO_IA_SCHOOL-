rm(list=ls())
##définir l'emplacement actuel du fichier comme répertoire de travail par défaut dans 
## la programmation R
library(rstudioapi)
current_path = rstudioapi::getActiveDocumentContext()$path 
Encoding(current_path) <- "UTF-8"
setwd(dirname(current_path ))

source("global.R", encoding='UTF-8')

#Query SQL
## connect to database (don't forget to disconnect)
DB <- dbConnect(MySQL(), 
                user="root", 
                host="localhost",
                password="Traore160410@", 
                dbname="projet_fao")
dbListTables(DB) # list tables

### importer un tableau de MySQL dans R
dispo_alim_habitant <- dbReadTable(DB, "dispo_alim_habitant") 

### Exécuter une requête SQL
dispo_alim_pays <- 
  dbGetQuery(DB, "SELECT * FROM 
	         (SELECT 
            ROW_NUMBER() OVER(PARTITION BY année) AS POSITION,
            année,
            pays,
            percentage_protein_habitant
            FROM 
                (SELECT année, pays, SUM(percentage_protein_habitant) As percentage_protein_habitant
                  FROM dispo_alim_habitant
                  GROUP BY année, pays
                  ORDER BY année, percentage_protein_habitant) As temp) As result;")

## Déconnexion

dbDisconnect(DB)

## Enfin notre SHINY APP
dbHeader <- dashboardHeader(title = "Food and Agriculture Organization of the United Nations", titleWidth = "39%",
                            tags$li(a(href = 'http://www.fao.org/home/en',
                                      img(src = 'https://www.dextrainternational.com/wp-content/uploads/2018/10/201412051145391417754739548138732b4030.jpg',
                                          title = "FAO", height = "80px"),
                                      style = "padding-top:0px; padding-bottom:0px; padding-right:0px; padding-left:0px;"),
                                    class = "dropdown"),
                            tags$li(class="dropdown",
                                    tags$style(".main-header{max-height:80px}"),
                                    tags$style(".main-header .logo {height: 80px; line-height: 80px !important;}")
                            )
)

ui <- dashboardPage(skin = "black",
                    dbHeader,
                    dashboardSidebar(collapsed = TRUE,
                                     tags$style(".left-side, .main-sidebar {padding-top: 80px}"),
                                     sidebarMenu(
                                       menuItem("Accueil", tabName ="accueil",icon = icon("home")),
                                       menuItem("Céréales", tabName ="cereal",icon = icon("bar-chart-o")),
                                       menuItem("Disponibilité alimentaire", tabName ="dispo_alim", icon = icon("table")),
                                       menuItem("Disponibilité mondiale", tabName ="dispo_int",icon = icon("fas fa-chart-line")),
                                       menuItem("Sous nutrition", tabName ="sous-nutrition",icon = icon("stats", lib = "glyphicon")),
                                       menuItem("Equilibre_prod", tabName ="prod", icon = icon("table")),
                                       menuItem("Dispo_alim_habitant", tabName ="dispo_alim_hbt", icon = icon("table")),
                                       
                                       HTML(paste0(
                                         "<br><br><br><br><br><br><br><br><br>",
                                         "<table style='margin-left:auto; margin-right:auto;'>",
                                         "<tr>",
                                         "<td style='padding: 5px;'><a href='https://fr-fr.facebook.com/UNFAO/' target='_blank'><i class='fab fa-facebook-square fa-lg'></i></a></td>",
                                         "<td style='padding: 5px;'><a href='https://www.youtube.com/user/FAOoftheUN' target='_blank'><i class='fab fa-youtube fa-lg'></i></a></td>",
                                         "<td style='padding: 5px;'><a href='https://twitter.com/fao' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
                                         "<td style='padding: 5px;'><a href='https://www.instagram.com/fao/?hl=fr' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
                                         "</tr>",
                                         "</table>",
                                         "<br>"),
                                         HTML(paste0(
                                           "<script>",
                                           "var today = new Date();",
                                           "var yyyy = today.getFullYear();",
                                           "</script>",
                                           "<p style = 'text-align: center;'><small>&copy; - <a href='http://www.fao.org/home/en' target='_blank'>fao.fr</a> - <script>document.write(yyyy);</script></small></p>")
                                         ))
                                       
                                       
                                       
                                       
                                     )
                    ),
                    dashboardBody(
                      #includeCSS("www/style.css"),
                      tabItems(
                        tabItem(tabName="accueil",
                                fluidPage(
                                  
                                  fluidRow(
                                    box(width=12,solidHeader = T,
                                        
                                        
                                        align="justify",
                                        p(h4("Cette application a pour objectif de visualiser les données dans une plateforme web interactive.")),
                                        p(h4("Elle repond aux besoins de la FAO pour un outil simple lors de la prise de décision sur une problématique.")),
                                        br()
                                       )
                                  ),
                                  
                                  fluidRow(
                                    box(width=12,
                                        DTOutput("result"),
                                        style = "overflow-x:auto; "
                                    )
                                  )
                                  
                                  
                                  
                                )
                                
                                
                        ),
                        
                        tabItem(tabName="cereal",
                                fluidPage(
                                  
                                  fluidRow(
                                    box(title="Proportion des céréales pour l’alimentation animale", collapsible = T, collapsed = F,
                                        width = 7, solidheader =T, status = "warning",# height = 170,
                                        column(3,
                                              gaugeOutput("prop1")
                                        ),
                                        
                                        column(3,
                                               gaugeOutput("prop2")
                                               
                                        ),
                                        
                                        column(3,
                                               gaugeOutput("prop3")
                                               
                                        ),
                                        
                                        column(3,
                                               gaugeOutput("prop4")
                                               
                                        ),
                                        
                                            DTOutput("tab_prop_ani")
                                      
                                    ),
                                    box(collapsible = T, collapsed = F,
                                        width = 5, solidheader =T, status = "warning", #height = 460,
                                        plotlyOutput("graphi_prop_ani")
                                    )
                                    
                                  )
                              )
                        ),
                        tabItem(tabName="dispo_alim",
                                fluidPage(
                                  
                                  fluidRow(
      
                                    box(width = 12,
                                        fluidRow(
                                        column(3,
                                               selectInput("select_produit", "choose a product:", 
                                                           choices = sort(unique(aliments_plus_caloriques_ou_plus_proteines$produit)))
                                        ),
                                        column(3,
                                               
                                               radioButtons("disp", "Affichage :",
                                                            choices = c(produit_seul = "filter",
                                                                        All = "all"),
                                                            selected = "filter")
                                               
                                            )
                                       ),
                                       br(),
                                       fluidRow(
                                         column(4,offset=5,
                                                downloadButton(outputId="tab_dl", label='Telecharger',class = "btn-primary")
                                         )
                                         
                                       )
                                     )
                                  ),
                                  
                                  fluidRow(
                                    box(width=12,
                                        DTOutput("Top_20_aliments"),
                                        style = "overflow-x:auto; "
                                    )
                                  )
                                  
                                  
                                  
                                )
                        ),
                        
                        tabItem(tabName="dispo_int",
                                fluidPage(
                                # Main panel for displaying outputs ----
                                mainPanel(
                                  
                                  # Output: Tabset w/ plot, summary, and table ----
                                  tabsetPanel(type = "tabs", 
                                              tabPanel("Plot", 
                                                       box(width = 7,
                                                           plotlyOutput("graphi_dispo_int_kcal")),
                                                       box(width = 5,
                                                           plotlyOutput("graphi_dispo_int_kgprotein"))),
                                              tabPanel("Table",fluidPage(
                                                       box(title="Disponibilité intérieure mondiale pour les produits végétaux", collapsible = T, collapsed = F,
                                                           width = 12, solidheader =T, status = "warning",# height = 170,
                                                           DTOutput("dispo_int_vegetal")),
                                                       fluidRow(
                                                         column(4,offset=5,
                                                                downloadButton(outputId="tab_dispo_int", label='Telecharger',class = "btn-primary")
                                                         )
                                                         
                                                       ))),
                                              tabPanel("Tous végétariens", fluidPage(
                                                fluidRow(
                                                  box(width = 12,
                                                      title = "Combien d'humains pourraient être nourris si toute la disponibilité intérieure mondiale de produits végétaux était utilisée pour de la nourriture ? Donnez les résultats en termes de calories, puis de protéines, et exprimez ensuite ces 2 résultats en pourcentage de la population mondiale.")
                                                ),
                                                fluidRow(
                                                  box(width = 12,
                                                  column(4,
                                                         selectInput("resultat", "Résultats:",
                                                                      choices = c("en termes de calories", "en termes de protéines"),
                                                                      selected = "en termes de calories")))
                                                ),
                                               fluidRow(
                                                 
                                                 box(width=12,solidHeader = T,
                                                     
                                                     verbatimTextOutput("summary")
                                                     
                                                 )
                                               )
                                              )),
                                              tabPanel("Tous bien nourris", 
                                                       fluidPage(
                                                         fluidRow(
                                                           box(width = 12,
                                                               title = "Combien d'humains pourraient être nourris avec la disponibilité alimentaire mondiale ? Donnez les résultats en termes de calories, puis de protéines, et exprimez ensuite ces 2 résultats en pourcentage de la population mondiale")
                                                         ),
                                                         fluidRow(
                                                           box(width = 12,
                                                               column(4,
                                                                      selectInput("resultat2", "Résultats:",
                                                                                  choices = c("en termes de calories", "en termes de protéines"),
                                                                                  selected = "en termes de calories")))
                                                         ),
                                                         fluidRow(
                                                           
                                                           box(width=12,solidHeader = T,
                                                               
                                                               verbatimTextOutput("Tous_bien_nourris")
                                                               
                                                           )
                                                         )
                                                       ))
                                  ), style='width: 1000px; height: 1000px'
                                )
                        )
                      ),
                      
                      tabItem(tabName="sous-nutrition",
                              fluidPage(
                                mainPanel(
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Pays en sous-nutrition",fluidPage(
                                                box(collapsible = T, collapsed = F,
                                                    width = 12, solidheader =T, status = "warning",# height = 170,
                                                    DTOutput("pays_en_sous_nutrition")),
                                                fluidRow(
                                                  column(4,offset=5,
                                                         downloadButton(outputId="tab_pays_en_sous_nutrition", label='Telecharger',class = "btn-primary"))
                                                  )
                                              )
                                              ),
                                            tabPanel("Produits plus exportés",
                                                     fluidPage(
                                                       box(title = "Liste des 15 produits les plus exportés par ce groupe de pays", collapsible = T, collapsed = F,
                                                           width = 12, solidheader =T, status = "warning",# height = 170,
                                                           DTOutput("produits_plus_exportes")),
                                                       fluidRow(
                                                         column(4,offset=5,
                                                                downloadButton(outputId="tab_produits_plus_exportes", label='Telecharger',class = "btn-primary"))
                                                       )
                                                       )
                                                     ),
                                            tabPanel("Top 3 produits", 
                                                     fluidPage(
                                                       box(width = 12, solidheader =T, status = "warning",# height = 170,
                                                           DTOutput("Top_3_produits")),
                                                       fluidRow(
                                                         column(4,offset=5,
                                                                downloadButton(outputId="tab_Top_3_produits", label='Telecharger',class = "btn-primary"))
                                                       )
                                                     )),
                                            tabPanel("Exportations de manioc en Thaïlande",
                                                     fluidPage(
                                                       box(width = 12, solidheader =T, status = "warning",# height = 170,
                                                           DTOutput("exportations_Thailande")),
                                                       fluidRow(
                                                         column(4,offset=5,
                                                                downloadButton(outputId="tab_exportations_Thailande", label='Telecharger',class = "btn-primary"))
                                                       )
                                                     )),
                                            tabPanel("Sous-nutrition en Thaïlande",
                                                     fluidPage(
                                                       box(width = 12, solidheader =T, status = "warning",# height = 170,
                                                           DTOutput("sous_nutrition_Thailande")),
                                                       fluidRow(
                                                         column(4,offset=5,
                                                                downloadButton(outputId="tab_sous_nutrition_Thailande", label='Telecharger',class = "btn-primary"))
                                                       )
                                                     )
                                              
                                            )
                                  ), style='width: 1000px; height: 1000px'
                               )
                              
                             )     
                      ),
                      
                      tabItem(tabName="prod",
                              fluidPage(
                                fluidRow(
                                  
                                  box(width = 12,
                                      fluidRow(
                                        column(3,
                                               selectInput("select_pays", "Pays:", 
                                                           choices = sort(unique(equilibre_prod$pays)))
                                        ),
                                        column(3,
                                               uiOutput('select_produit2')
                                        ),
                                        column(3,
                                               
                                               radioButtons("disp2", "Affichage :",
                                                            choices = c(produit_seul = "filter2",
                                                                        All = "all"),
                                                            selected = "filter2")
                                               
                                        )
                                      ),
                                      br(),
                                      fluidRow(
                                        column(4,offset=5,
                                               downloadButton(outputId="tab_equilibre_prod", label='Telecharger',class = "btn-primary")
                                        )
                                        
                                      )
                                  )
                                ),
                                
                                fluidRow(
                                  box(width=12,
                                      DTOutput("equilibre_prod"),
                                      style = "overflow-x:auto; "
                                  )
                                )
                              )
                      ),
                      
                      tabItem(tabName="dispo_alim_hbt",
                              fluidPage(
                                
                                fluidRow(
                                  box(width = 12,
                                      title = "Pour chaque année disponible, les 10 pays ayant le plus faible ratio disponibilité alimentaire/habitant en termes de protéines (en kg) par habitant.")
                                ),
                                fluidRow(
                                  box(width = 12,
                                  column(3,
                                         selectInput("select_annee", "Année :", 
                                                     choices = sort(unique(dispo_alim_pays$année)))
                                  ),
                                  column(3,
                                         numericInput("nrows", "How many countries to show?", 10)
                                  ),
                                  column(3,
                                         
                                         radioButtons("disp3", "Affichage :",
                                                      choices = c(Année_seul = "filter3",
                                                                  All = "all"),
                                                      selected = "filter3")
                                         ),
                                  column(3,
                                         downloadButton(outputId="tab_dispo_alim_habt", label='Telecharger',class = "btn-primary")
                                  )
                                )
                                ),
                                fluidRow(
                                  box(width=12,
                                      DTOutput("dispo_alim_habt"),
                                      style = "overflow-x:auto; "
                                  )
                                )
                              )
                      )
                      
                    )
                )
                    
)






server <- function(input, output) {
  
  ## Affichage de la dataset globale dans la shiny app
  output$result <- renderDT(datatable(
    
    data = {
      
      return(datatable(result, rownames = F,
                       options = list(
                         pageLength = 4,
                         initComplete = JS('function(setting, json) { alert("BIENVENUE!  Vous pouvez analyser vos résultats en toute confiance"); }')
                       ))
             )}
  ))
  
  ## Proportion des céréales pour l'alimentation animale
  
  output$prop1 <- renderGauge({
    prop_animal <- cereals_for_feed_food$prop_animale[cereals_for_feed_food$année == 2014]
    gauge(prop_animal, min = 0, max = 100, symbol = '%', 
          label = "2014",gaugeSectors(
            success = NULL, warning = c(0,100), danger = NULL
          ))
  })
  
  output$prop2 <- renderGauge({
    prop_animal <- cereals_for_feed_food$prop_animale[cereals_for_feed_food$année == 2015]
    gauge(prop_animal, min = 0, max = 100, symbol = '%', 
          label = "2015",gaugeSectors(
            success = NULL, warning = c(0,100), danger = NULL
          ))
  })
  
  output$prop3 <- renderGauge({
    prop_animal <- cereals_for_feed_food$prop_animale[cereals_for_feed_food$année == 2016]
    gauge(prop_animal, min = 0, max = 100, symbol = '%', 
          label = "2016",gaugeSectors(
            success = NULL, warning = c(0,100), danger = NULL
          ))
  })
  
  output$prop4 <- renderGauge({
    prop_animal <- cereals_for_feed_food$prop_animale[cereals_for_feed_food$année == 2017]
    gauge(prop_animal, min = 0, max = 100, symbol = '%', 
          label = "2017",gaugeSectors(
            success = NULL, warning = c(0,100), danger = NULL
          ))
  })
  
  output$graphi_prop_ani <- renderPlotly({
    ggplotly(ggplot(cereals_for_feed_food, aes(x=année, y=prop_animale))+
               geom_line(colour="blue") +
               xlab("année")
            )
    })
  
  output$tab_prop_ani <- renderDT(datatable(
    data = {
      return(datatable(cereals_for_feed_food, rownames = F))
    }
  ))
  
  ## Disponibilité alimentaire : Top 20 des aliments
  output$Top_20_aliments <- renderDT(datatable(filter = "top",
                                        data = {
                                          
                                          if(input$disp == "filter"){
                                            return(datatable(subset(aliments_plus_caloriques_ou_plus_proteines, produit==input$select_produit),rownames = F))
                                            
                                          }else{
                                            return(datatable(aliments_plus_caloriques_ou_plus_proteines,rownames=F))
                                            
                                          }
                                          
                                        }
  )
  )
  
  #### TELECHARGEMENT DU TABLEAU
  
  output$tab_dl <- downloadHandler(
    
    filename = function() {
      paste('Dispo_alim', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      if(input$disp == "filter"){
        
        write.csv2(subset(aliments_plus_caloriques_ou_plus_proteines, produit==input$select_produit), file, fileEncoding = 'LATIN1', row.names = F)
        
      }else{
        write.csv2(aliments_plus_caloriques_ou_plus_proteines, file, fileEncoding = 'LATIN1', row.names = F)
        
      }
      
      
    }
  )
  
  ## Disponibilité intérieure mondiale en végétaux
  dispo_int_vegetal <- dispo_int_vegetal %>%
    select(année, dom_sup_Kcal, dom_sup_kgprot)
  
  output$dispo_int_vegetal <- renderDT(datatable(
    data = {
      return(datatable(dispo_int_vegetal, rownames = F))
    }
  ))
  
  ## TELECHARGEMENT DU TABLEAU
  
  output$tab_dispo_int <- downloadHandler(
    
    filename = function() {
      paste('dispo_int_vegetal', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      write.csv2(dispo_int_vegetal, file, fileEncoding = 'LATIN1', row.names = F)
    }
  )
  
  ## visualization
  output$graphi_dispo_int_kcal <- renderPlotly({
    ggplotly(
      ggplot(dispo_int_vegetal, aes(x=année, y=dom_sup_Kcal))+
        geom_line(colour="blue") +
        scale_x_continuous(name="year") +
        scale_y_continuous(name="domestic supply en kcal") 
    )
  })
  
  output$graphi_dispo_int_kgprotein <- renderPlotly({
    ggplotly(
      ggplot(dispo_int_vegetal, aes(x=année, y=dom_sup_kgprot))+
        geom_line(colour="green") +
        scale_x_continuous(name="year") +
        scale_y_continuous(name="domestic supply Kg protéines") 
    )
  })
  

  output$summary <- renderPrint({
    
    if(input$resultat == "en termes de calories"){
      cat(
        "2014 : Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux", "\n" ,"(en termes calorifiques) : 13.9 Millards, soit 196.1 % de la population mondiale","\n","\n",
        
        "2015 : Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux", "\n", "(en termes calorifiques) : 14.22 Millards, soit 198.3 % de la population mondiale","\n","\n",
        
        "2016 : Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux", "\n", "(en termes calorifiques) : 14.54 Millards, soit 200.6 % de la population mondiale","\n","\n",
        
        "2017 : Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux", "\n", "(en termes calorifiques) : 15.05 Millards, soit 205.3 % de la population mondiale"
        
      )}else{
        
        cat(
          "2014 : Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux", "\n", "(en termes de protéines) : 13.88 Millards, soit 195.8 % de la population mondiale","\n","\n",
          "2015 : Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux", "\n", "(en termes de protéines) : 14.18 Millards, soit 197.7 % de la population mondiale","\n","\n",
          "2016 : Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux", "\n", "(en termes de protéines) : 14.86 Millards, soit 205 % de la population mondiale","\n","\n",
          "2017 : Population potentiellement nourrie par la disponibilité intérieure en produits issus de végétaux", "\n", "(en termes de protéines) : 15.47 Millards, soit 211.1 % de la population mondiale"
        )
      }
    })
  
  
  output$Tous_bien_nourris <- renderPrint({
    
    if(input$resultat2 == "en termes de calories"){
      cat(
        "2014 : Population potentiellement nourrie par la disponibilité alimentaire mondiale", "\n", "(en termes calorifiques) : 8.12 Millards, soit 114.5 % de la population mondiale", "\n","\n",
        
        "2015 : Population potentiellement nourrie par la disponibilité alimentaire mondiale", "\n", "(en termes calorifiques) : 8.24 Millards, soit 115 % de la population mondiale", "\n","\n",
        
        "2016 : Population potentiellement nourrie par la disponibilité alimentaire mondiale", "\n", "(en termes calorifiques) : 8.35 Millards, soit 115.2 % de la population mondiale", "\n","\n",
        
        "2017 : Population potentiellement nourrie par la disponibilité alimentaire mondiale", "\n", "(en termes calorifiques) : 8.48 Millards, soit 115.7 % de la population mondiale"
        
      )}else{
        
        cat(
          "2014 : Population potentiellement nourrie par la disponibilité alimentaire mondiale", "\n", "(en termes de protéines) : 10.28 Millards, soit 145 % de la population mondiale","\n","\n",
          "2015 : Population potentiellement nourrie par la disponibilité alimentaire mondiale", "\n", "(en termes de protéines) : 10.47 Millards, soit 146 % de la population mondiale" ,"\n","\n",
          "2016 : Population potentiellement nourrie par la disponibilité alimentaire mondiale", "\n", "(en termes de protéines) : 10.65 Millards, soit 147 % de la population mondiale" ,"\n","\n",
          "2017 : Population potentiellement nourrie par la disponibilité alimentaire mondiale", "\n", "(en termes de protéines) : 10.86 Millards, soit 148.2 % de la population mondiale"
        )
      }
  })
  
  ## Pays en sous-nutrition
  
  output$pays_en_sous_nutrition <- renderDT(datatable(
    data = {
      return(datatable(liste_pays_en_sous_nutrition, rownames = F,
                       options = list(
                         pageLength = 7
                       )))
    }
  ))
  
  output$tab_pays_en_sous_nutrition <- downloadHandler(
    
    filename = function() {
      paste('pays_en_sous_nutrition', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      write.csv2(liste_pays_en_sous_nutrition, file, fileEncoding = 'LATIN1', row.names = F)
    }
  )
  
  
  output$produits_plus_exportes <- renderDT(datatable(
    data = {
      return(datatable(Liste_15_produits_plus_exportes, rownames = F,
                       options = list(
                         pageLength = 6)
                       ))
    }
  ))
  
  output$tab_produits_plus_exportes <- downloadHandler(
    
    filename = function() {
      paste('Liste_produits_plus_exportés', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      write.csv2(Liste_15_produits_plus_exportes, file, fileEncoding = 'LATIN1', row.names = F)
    }
  )
  
  output$Top_3_produits <- renderDT(datatable(
    data = {
      return(datatable(Importations_par_produit, rownames = F,
                       options = list(
                         pageLength = 7
                       )))
    }
  ))
  
  output$tab_Top_3_produits <- downloadHandler(
    
    filename = function() {
      paste('Importations_par_produit', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      write.csv2(Importations_par_produit, file, fileEncoding = 'LATIN1', row.names = F)
    }
  )
  
  output$exportations_Thailande <- renderDT(datatable(
    data = {
      return(datatable(Exportations_Manioc_en_Thaïlande, rownames = F))
    }
  ))
  
  output$tab_exportations_Thailande <- downloadHandler(
    
    filename = function() {
      paste('exportations manioc en Thailande', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      write.csv2(Exportations_Manioc_en_Thaïlande, file, fileEncoding = 'LATIN1', row.names = F)
    }
  )
  
  output$sous_nutrition_Thailande <- renderDT(datatable(
    data = {
      return(datatable(Sous_nutrition_en_Thaïlande, rownames = F))
    }
  ))
  
  output$tab_sous_nutrition_Thailande <- downloadHandler(
    
    filename = function() {
      paste('Sous nutrition en Thailande', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      write.csv2(Sous_nutrition_en_Thaïlande, file, fileEncoding = 'LATIN1', row.names = F)
    }
  )
  
  ## Table Equilibre prod
  
  #ON GENERE LE WIDGET SELECT_PRODUIT ONGLET EQUILIBRE PROD
  output$select_produit2 <- renderUI({
    selectInput('select_produit2', "Produit : ",
                sort(unique(as.character(equilibre_prod$produit[which(equilibre_prod$produit != '' & equilibre_prod$pays == input$select_pays)])))
    )
  })
  
  output$equilibre_prod <- renderDT(datatable(filter = "top",
    data = {
      if(input$disp2 == "filter2"){
        return(datatable(subset(equilibre_prod, produit == input$select_produit2 & pays == input$select_pays), rownames = F))
      }else{
        return(datatable(subset(equilibre_prod, pays == input$select_pays), rownames = F, options = list(pageLength = 10)))
      }
      
    }
  ))
  
  output$tab_equilibre_prod <- downloadHandler(
    
    filename = function() {
      paste('Equilibre prod', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      if(input$disp2 == "filter2"){
        
        write.csv2(subset(equilibre_prod, produit==input$select_produit2 & pays == input$select_pays), file, fileEncoding = 'LATIN1', row.names = F)
        
      }else{
        write.csv2(subset(equilibre_prod, pays == input$select_pays), file, fileEncoding = 'LATIN1', row.names = F)
        
      }
    })
  
  ## Disponibilité alimentaire par habitant
  
  output$dispo_alim_habt <- renderDT(datatable(filter = "top",
                                              data = {
                                                if(input$disp3 == "filter3"){
                                                  return(datatable(subset(dispo_alim_pays, année == input$select_annee & POSITION <= input$nrows), rownames = F))
                                                }else{
                                                  return(datatable(subset(dispo_alim_pays, POSITION <= input$nrows), rownames = F, options = list(pageLength = 10)))
                                                }
                                              }))
  
  output$tab_dispo_alim_habt <- downloadHandler(
    
    filename = function() {
      paste('dispo alim par habitant', Sys.Date(), '.csv', sep='')
    },
    
    content = function(file) {
      
      write.csv2(subset(dispo_alim_pays, POSITION <= input$nrows), file, fileEncoding = 'LATIN1', row.names = F)

    })
  
  
  }


shinyApp(ui, server)



