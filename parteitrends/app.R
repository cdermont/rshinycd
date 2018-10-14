#shiny::runGitHub("rshinycd", "cdermont", subdir = "parteitrends/")

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.table("www/data.csv", sep=";", header=T, 
                   stringsAsFactors = F, fileEncoding = 'UTF-8')
data$Datum <- as.Date(data$Datum, format="%d.%m.%Y")
data$Zeit <- as.POSIXct(paste(data$Datum, data$Zeit, sep=" "), 
                        format="%Y-%m-%d %H:%M:%S")

cantonal <- read.table("www/cantonalhistory.csv", sep=";", header=T, 
                       stringsAsFactors = F, fileEncoding="UTF-8")

ui <- fluidPage(
  
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
)

server <- function(input, output){
  
  # Tab 1
  getData <- reactive({
    d <- data[2:nrow(data), c(1:6, which(names(data) %in% input$selection))]    
    d$"weitere" <- 0-rowSums(d[,7:length(d)])
    d <- gather(d, "partei", "diff", 7:length(d))
    d <- d %>% group_by(partei) %>% mutate(trend=cumsum(diff))
  })
  
  output$trendPlot <- renderPlot({
    
    d <- getData()
    d$partei <- factor(d$partei, levels=c(input$selection, "weitere"))
    
    p.data <- ggplot(d, aes(x=reorder(Kanton, id), color=partei)) +
      geom_hline(aes(yintercept=0), linetype=2, color="grey27") +
      geom_line(aes(y=trend, group=partei), size=1.25, alpha=0.5) +
      geom_pointrange(aes(y=diff, ymin=0, ymax=diff)) +
      geom_point(aes(y=diff)) +
      geom_point(aes(y=trend), size=0.6) +
      scale_color_manual(values = 
                           c("FDP"="darkblue",
                             "LPS"="lightblue",
                             "FDP.LPS"="darkblue",
                             "CVP"="orange",
                             "SP"="firebrick",
                             "SVP"="yellowgreen",
                             "GLP"="lawngreen",
                             "BDP"="gold",
                             "GPS"="forestgreen",
                             "EVP"="yellow",
                             "CSP"="darkorange",
                             "PdA.AL.Sol.PSA"="maroon1",
                             "EDU.SD.Lega.MCR"="indianred4",
                             "Links"="orangered4",
                             "Mitte"="goldenrod",
                             "Rechts"="olivedrab",
                             "weitere"="grey")) +
      facet_wrap(~partei) +
      labs(title="Sitzverschiebung bei kantonalen Parlamentswahlen seit 2015", 
           subtitle="Darstellung: Clau Dermont",
           y="Sitzverschiebung",
           x="Punkt: Sitzverschiebung pro Wahl, Linie: kumulierte Veränderung") +
      theme_minimal() +
      theme(text = element_text(size=12),
            axis.text.x = element_text(angle = 60, hjust = 1),
            plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
            legend.position="none", 
            strip.text.x=element_text(face="bold"))
    p.data
    
  })
  
  # Tab 2
  getDataLinetrend <- reactive({
    dh <- data[1, which(names(data) %in% input$selectionline)]
    dh <- gather(dh, "partei", "ref")
    d <- data[, c(1:6, which(names(data) %in% input$selectionline))]    
    d <- gather(d, "partei", "diff", 7:length(d))
    d <- d %>% group_by(partei) %>% mutate(trend=cumsum(diff)) 
    d <- full_join(d, dh, by="partei")
  })
  
  output$trendlinePlot <- renderPlot({
    
    d <- getDataLinetrend()
    d$change <- d$trend/d$ref
    d$partei <- factor(d$partei, levels=c(input$selectionline))
    
    p.trendline <- ggplot(d, aes(x=reorder(Kanton, id), y=change, color=partei)) +
      geom_hline(aes(yintercept=1), linetype=2, color="grey27") +
      geom_line(aes(y=change, group=partei), size=1.25, alpha=0.6) +
      geom_point(aes(y=change, group=partei), size=0.6, alpha=0.6) +
      scale_color_manual(values = 
                           c("FDP"="darkblue",
                             "LPS"="lightblue",
                             "FDP.LPS"="darkblue",
                             "CVP"="orange",
                             "SP"="firebrick",
                             "SVP"="yellowgreen",
                             "GLP"="lawngreen",
                             "BDP"="gold",
                             "GPS"="forestgreen",
                             "EVP"="yellow",
                             "CSP"="darkorange",
                             "PdA.AL.Sol.PSA"="maroon1",
                             "EDU.SD.Lega.MCR"="indianred4",
                             "Links"="orangered4",
                             "Mitte"="goldenrod",
                             "Rechts"="olivedrab",
                             "weitere"="grey")) +
      labs(title="Sitzverschiebungen pro Partei nach Delegationsstärke seit 2015", 
           subtitle="Darstellung: Clau Dermont",
           y="Kumulierte Veränderung",
           x="Kantonale Wahlen", 
           color="Partei") +
      theme_minimal() +
      theme(text = element_text(size=12),
            axis.text.x = element_text(angle = 60, hjust = 1),
            plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
            strip.text.x=element_text(face="bold")) +
      guides(colour = guide_legend(override.aes = list(alpha=1)))
    p.trendline
    
  })
  
  # Tab 3
  getDataCant <- reactive({
    d <- cantonal[which(cantonal$Kanton %in% input$cant), ] 
    
    if (input$full==TRUE) {
      d <- d[,1:28]
    } else {
      d <- d[,c(1:14,28:30)]
    }
    
    d <- gather(d, "partei", "sitze", 4:length(d))
    d <- na.omit(d)
  })
  
  output$cantPlot <- renderPlot({
    
    d <- getDataCant()
    d$sitze <- as.numeric(d$sitze)
    d$partei <- factor(d$partei, levels=c(
      "kleine.Linksparteien", "PdA", "POCH", "PSA", "Sol.", 
      "FGA", "GPS", "SP", "DSP", 
      "LdU", "CSP", "EVP", "GLP", "CVP", "BDP", 
      "LPS", "FDP", "SVP",  "Dem.", "EDU", 
      "Lega", "MCR", "SD", "Rep.", "FPS", "LS", "kleine.Rechtsparteien",
      "weitere"
    ))
    
    p.history <- ggplot(d, aes(x=Wahljahr, y=sitze, fill=partei)) +
      geom_area(position="stack", alpha=0.8) +
      scale_x_continuous() +
      scale_fill_manual(values = 
                          c("kleine.Linksparteien"="maroon1", 
                            "PdA"="maroon1", "POCH"="gray20",
                            "PSA"="coral2", "Sol."="orangered",
                            "FGA"="cyan", "GPS"="forestgreen",
                            "SP"="firebrick", "DSP"="magenta", 
                            "LdU"="brown1", "CSP"="lavenderblush",
                            "EVP"="yellow", "GLP"="lawngreen",
                            "CVP"="orange", "BDP"="lightgoldenrod",
                            "FDP"="darkblue", "LPS"="lightblue",
                            "SVP"="yellowgreen", "Dem."="salmon", 
                            "EDU"="plum3", "Lega"="tomato3", 
                            "MCR"="khaki", "SD"="sienna", 
                            "Rep."="saddlebrown", "FPS"="lightpink3", 
                            "LS"="lemonchiffon", "kleine.Rechtsparteien"="sienna",
                            "weitere"="grey")) +
      labs(title=paste0("Sitzverteilung über die Zeit, Kanton ", input$cant), 
           subtitle="Darstellung: Clau Dermont",
           y="Sitze",
           x="Zeit", 
           fill="Partei") +
      theme_minimal() +
      theme(text = element_text(size=12),
            plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm")) +
      guides(colour = guide_legend(override.aes = list(alpha=1)))
    p.history
    
  })
}

shinyApp(ui = ui, server = server)

