shinyUI(fluidPage(
  
  titlePanel("Stimmbeteiligungstrends nach Alterskategorie und Geschlecht"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Zeittrends",
      fluidRow(
        column(3, 
          selectInput("selection", 
                      label = h4("Variable"),
                      choices = list(
                        "Stimmbeteiligung" = "overall",
                        "Männer" = "m",
                        "Frauen" = "f",
                        "Unterschied Frauen/Männer" = "gap",
                        "Abweichung nach Altersgruppe" = "ref"
          )),
          radioButtons("radio",
                       label = h4("Einheit"),
                       choices = levels(factor(data$entity))
          ),
          checkboxGroupInput("check", 
                             label=h4("Alterskategorie"), 
                             choices = levels(factor(data$key)),
                             selected = "Stimmbeteiligung"
          ), 
          HTML("<b><a href='http://claudermont.ch'>claudermont.ch</a></br>
               Datenquellen:</b> 
               <a href='http://www.ne.ch/autorites/CHAN/elections-votations/stat/Pages/accueil.aspx'>
               Kt. Neuenburg</a>/
               <a href='http://www.stadtluzern.ch/de/dokumente/wahlenundabstimmungen/abstimmungsresultate/welcome.php?show=allrecent'>
               Stadt Luzern</a></br>")
        ),
        column(9, 
          div(
            style = "position:relative",
            plotOutput("linePlot",
                       hover = hoverOpts(id = "plot_hover")),
            uiOutput("hover_info")
          )
        )
      )
    ),
    tabPanel("Vergleich",
      fluidRow(
        column(3, 
          selectInput("selectionv", 
                      label = h4("Variable"),
                      choices = list(
                        "Stimmbeteiligung" = "overall",
                        "Männer" = "m",
                        "Frauen" = "f",
                        "Unterschied Frauen/Männer" = "gap",
                        "Abweichung nach Altersgruppe" = "ref"
          )),
          radioButtons("radiov", 
                       label = h4("Alterskategorie"),
                       choices = levels(factor(data$key)),
                       selected = "Stimmbeteiligung"
          ), 
          HTML("<b><a href='http://claudermont.ch'>claudermont.ch</a></br>
               Datenquellen:</b> 
               <a href='http://www.ne.ch/autorites/CHAN/elections-votations/stat/Pages/accueil.aspx'>
               Kt. Neuenburg</a>/
               <a href='http://www.stadtluzern.ch/de/dokumente/wahlenundabstimmungen/abstimmungsresultate/welcome.php?show=allrecent'>
               Stadt Luzern</a></br>")
        ),
        column(9, 
          div(
            style = "position:relative",
            plotOutput("pairedlinePlot",
                       hover = hoverOpts(id = "plot_hover")),
            uiOutput("hover_infop")
          )
        )
      )
    )
  )
))      