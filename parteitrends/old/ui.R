shinyUI(fluidPage(
  
  titlePanel("Kantonale Parteitrends"),
  
  tabsetPanel(
    type = "tabs",
    
    tabPanel("Absolute Sitzverschiebungen seit 2015",
      fluidRow(
        column(3, 
          selectizeInput("selection",
                         label = h4("Parteien"),
                         choices = c("FDP","CVP","SP","SVP","GPS",
                                     "GLP","BDP","EVP","CSP","PdA.AL.Sol.PSA",
                                     "EDU.SD.Lega.MCR","FDP.LPS","LPS",
                                     "Links","Mitte","Rechts"),
                         selected = c("SP","CVP","FDP","SVP"),
                         multiple=T
          ),
          HTML("Punkt: Sitzverschiebung pro Wahl,</br>
                Linie: kumulierte Veränderung.</br></br>
                Die Parteien erscheinen in der Reihenfolge, in der sie ausgewählt werden.</br>
                FDP.LPS = FDP und LPS. </br>
                Links = SP, GPS, PdA.AL.Sol.PSA.</br>
                Mitte = CVP, BDP, GLP, EVP, CSP.</br>
                Rechts = SVP, FDP, EDU.SD.Lega.MCR.</br>
                Übrige = automatisch berechnet.</br></br>
                <b>Umsetzung: <a href='https://twitter.com/claudermont'>@claudermont</a>, 
                <a href='https://twitter.com/sandroluescher'>@sandroluescher</a></br>
                Datenquelle:</b> <a href='https://www.bfs.admin.ch/bfs/de/home/statistiken/politik/wahlen/kantonale-parlamenswahlen.html'>Bundesamt für Statistik</a></br>")
        ),
        column(9, 
          div(
            style = "position:relative",
            plotOutput("trendPlot", height="560px")
          )
        )
      )
    ),
    
    tabPanel("Relative Sitzverschiebungen seit 2015",
             fluidRow(
               column(3, 
                      selectizeInput("selectionline",
                                     label = h4("Parteien"),
                                     choices = c("FDP","CVP","SP","SVP","GPS",
                                                 "GLP","BDP","EVP","CSP","PdA.AL.Sol.PSA",
                                                 "EDU.SD.Lega.MCR","FDP.LPS","LPS",
                                                 "Links","Mitte","Rechts"),
                                     selected = c("SP","CVP","FDP","SVP"),
                                     multiple=T
                      ),
                      HTML("Linie: kumulierte Veränderung.</br>
                           1 = Delegationsstärke 2015 in allen Parlamenten.</br></br>
                           FDP.LPS = FDP und LPS. </br>
                           Links = SP, GPS, PdA.AL.Sol.PSA.</br>
                           Mitte = CVP, BDP, GLP, EVP, CSP.</br>
                           Rechts = SVP, FDP, EDU.SD.Lega.MCR.</br></br>
                           <b>Umsetzung: <a href='https://twitter.com/claudermont'>@claudermont</a></br>
                           Datenquelle:</b> <a href='https://www.bfs.admin.ch/bfs/de/home/statistiken/politik/wahlen/kantonale-parlamenswahlen.html'>Bundesamt für Statistik</a></br>")
                      ),
               column(9, 
                      div(
                        style = "position:relative",
                        plotOutput("trendlinePlot", height="560px")
                      )
               )
            )
    ),
    
    tabPanel("Parteistärken über die Zeit",
             fluidRow(
               column(3, 
                      selectInput("cant",
                                  label = h4("Kantone"),
                                     choices = c("Aargau","Appenzell A. Rh.", "Basel-Landschaft",
                                                 "Basel-Stadt","Bern","Freiburg","Genf","Glarus",
                                                 "Graubünden","Jura","Luzern","Neuenburg",
                                                 "Nidwalden","Obwalden","Schaffhausen","Schwyz",
                                                 "Solothurn","St. Gallen","Tessin","Thurgau","Uri",
                                                 "Waadt","Wallis","Zürich","Zug"),
                                     selected = c("Aargau")
                      ),
                      checkboxInput("full", 
                                    "Auch Kleinstparteien anzeigen", 
                                    FALSE),
                      HTML("Historische Delegationsstärke der Parteien in den kantonalen Parlamenten.</br></br>
                           <b>Umsetzung: <a href='https://twitter.com/claudermont'>@claudermont</a></br>
                           Datenquelle:</b> <a href='https://www.bfs.admin.ch/bfs/de/home/statistiken/politik/wahlen/kantonale-parlamenswahlen.html'>Bundesamt für Statistik</a></br>")
                      ),
               column(9, 
                      div(
                        style = "position:relative",
                        plotOutput("cantPlot", height="560px")
                      )
               )
            )
    )
  )
))      