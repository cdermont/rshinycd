shinyUI(fluidPage(
  
  titlePanel("Abstimmungsresultate 1866-2017"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Kantonsresultate",
             fluidRow(
               column(3, 
                      selectizeInput("selection", 
                                     label = h4("Abstimmung"), 
                                     choices = levels(data$vote), 
                                     selected = "24.09.2017 Reform der Altersvorsorge 2020", 
                                     multiple = FALSE), 
                      HTML("<b><a href='http://claudermont.ch'>claudermont.ch</a></br>
                           Datenquellen:</b> 
                           <a href='https://bfs.admin.ch/'>
                           BfS</a>")
                      ),
               column(9, HTML("</br>"),
                      div(
                        style = "position:relative",
                        plotOutput("scatter", width="600px", height = "600px",
                                   hover = hoverOpts("plot_hover", 
                                                     delay = 100, delayType = "debounce")),
                        uiOutput("hover_info")
                      )
               )
                      )
             ),
    tabPanel("Vergleich",
             fluidRow(
               column(3, 
                      selectizeInput("selectionx", 
                                     label = h4("Abstimmung (X-Achse)"), 
                                     choices = levels(data$vote),
                                     selected = "24.09.2017 Reform der Altersvorsorge 2020", 
                                     multiple = FALSE), 
                      selectizeInput("selectiony", 
                                     label = h4("Abstimmung (Y-Achse)"), 
                                     choices = levels(data$vote), 
                                     selected = "24.09.2017 Zusatzfinanzierung AHV über MwSt.", 
                                     multiple = FALSE,
                                     options = NULL), 
                      radioButtons("key", 
                                   label = h4("Vergleich"),
                                   choices = c("Ja-Anteil" = "Ja",
                                               "Stimmbeteiligung" = "Stimmbeteiligung"),
                                   selected="Ja"),
                      HTML("<b><a href='http://claudermont.ch'>claudermont.ch</a></br>
                           Datenquellen:</b> 
                           <a href='https://bfs.admin.ch/'>
                           BfS</a>")
                      ),
               column(9, HTML("</br>"), 
                      div(
                        style = "position:relative",
                        plotOutput("scattercompare", width="600px", height = "600px",
                                   hover = hoverOpts("plot_hover2", 
                                                     delay = 100, delayType = "debounce")),
                        uiOutput("hover_info2")
                      )
               )
                      )
             )
)
))      