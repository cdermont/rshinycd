#shiny::runGitHub("rshinycd", "cdermont", subdir = "turnoutr/")

library(shiny)
library(ggplot2)
library(plotly)

data <- read.table("www/turnout.csv", sep=",", header=T, stringsAsFactors = F)
data$date <- as.Date(as.character(data$date), "%Y-%m-%d")

ui <- fluidPage(
  
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
)

server <- function(input, output){
  
  getData <- reactive({
    d <- data[which(data$entity %in% input$radio),]
    d <- d[which(d$code %in% input$selection),]
    d <- d[which(d$key %in% input$check),]
  })
  getDataPaired <- reactive({
    d <- data[which(data$key %in% input$radiov),]
    d <- d[which(d$code %in% input$selectionv),]
  })
  
  getScale <- reactive({
    if (input$selection=="gap") {
      scale <- c(-0.25, 0.1)
    } else if (input$selection=="ref") {
      scale <- c(-0.25, 0.25)
    } else {
      scale <- c(0,1)
    }
  })
  
  getScalePaired <- reactive({
    if (input$selectionv=="gap") {
      scale <- c(-0.25, 0.1)
    } else if (input$selectionv=="ref") {
      scale <- c(-0.25, 0.25)
    } else {
      scale <- c(0,1)
    }
  })
  
  # Tab 1
  output$linePlot <- renderPlot({
    
    d <- getData()
    scale <- getScale()
    
    p <- ggplot(d, aes(date, turnout, color=key)) + 
      geom_line(aes(group=key, linetype=key), 
                alpha=0.9, size=1) +
      geom_point(alpha=0.9, size=2) +
      scale_y_continuous(labels=scales::percent_format(), limits=scale) +
      scale_x_date() +
      scale_color_manual(values=c(
        "18 bis 19 Jahre" = "#a6cee3", 
        "20 bis 29 Jahre" = "#1f78b4",  
        "30 bis 39 Jahre" = "#b2df8a",  
        "40 bis 49 Jahre" = "#33a02c",  
        "50 bis 59 Jahre" = "#fd9a99",  
        "60 bis 69 Jahre" = "#e31a1c", 
        "70 bis 79 Jahre" = "#fdbf6f",  
        "80+ Jahre" = "#ff7f00",        
        "Stimmbeteiligung" = "black"
      )) +
      scale_linetype_manual(values=c(
        "18 bis 19 Jahre" = 4, 
        "20 bis 29 Jahre" = 2,  
        "30 bis 39 Jahre" = 1,  
        "40 bis 49 Jahre" = 3,  
        "50 bis 59 Jahre" = 2,  
        "60 bis 69 Jahre" = 3, 
        "70 bis 79 Jahre" = 2,  
        "80+ Jahre" = 4,        
        "Stimmbeteiligung" = 1
      )) +
      labs(title=paste("Partizipation: ", input$radio, sep=""),
           subtitle="Darstellung: Clau Dermont",
           y="Stimmbeteiligung", 
           x="Abstimmungsdatum",
           color="Alterskategorie", 
           linetype="Alterskategorie") +
      theme_minimal() +
      theme(legend.position="bottom")
    p
  })
  
  output$hover_info <- renderUI({
    d <- getData()
    hover <- input$plot_hover
    point <- nearPoints(d, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Abstimmung: </b>", point$date, "<br/>",
                    "<b> Alterskategorie: </b>", point$key, "<br/>",
                    "<b> Beteiligung: </b>", point$turnout*100, "%<br/>"
      )))
    )
  })
  
  #Tab 2
  output$pairedlinePlot <- renderPlot({
    
    d <- getDataPaired()
    scale <- getScalePaired()
    
    p <- ggplot(d, aes(date, turnout, color=entity)) + 
      geom_line(aes(group=entity, linetype=entity), 
                alpha=0.9, size=1) +
      geom_point(alpha=0.9, size=2) +
      scale_y_continuous(labels=scales::percent_format(), limits=scale) +
      scale_x_date() +
      scale_color_manual(values=c(
        "Kanton Neuenburg" = "#a6cee3", 
        "Stadt Luzern" = "#1f78b4"
      )) +
      labs(title="Partizipation im Vergleich",
           subtitle="Darstellung: Clau Dermont",
           y="Stimmbeteiligung", 
           x="Abstimmungsdatum",
           color="Einheit", 
           linetype="Einheit") +
      theme_minimal() +
      theme(legend.position="bottom")
    p
  })
  
  output$hover_infop <- renderUI({
    d <- getDataPaired()
    hover <- input$plot_hover
    point <- nearPoints(d, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Einheit: </b>", point$entity, "<br/>",
                    "<b> Abstimmung: </b>", point$date, "<br/>",
                    "<b> Beteiligung: </b>", point$turnout*100, "%<br/>"
      )))
    )
  })
}

shinyApp(ui = ui, server = server)
