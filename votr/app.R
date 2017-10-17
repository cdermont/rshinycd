#shiny::runGitHub("rshinycd", "cdermont", subdir = "votr/")

library(shiny)
library(ggplot2)
library(plotly)

data <- read.table("www/votedata.csv", sep=";", header=T, stringsAsFactors = F, fileEncoding = "UTF-8")
data[,c("suffrage","Stimmbeteiligung","Ja")] <- 
  lapply(c("suffrage","Stimmbeteiligung","Ja"), function(x) as.numeric(data[,x]))
data$vote <- factor(data$vote, as.character(unique(data$vote)))

ui <- fluidPage(
  
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
)

server <- function(input, output){
  
  getData <- reactive({
    d <- data[which(data$vote %in% input$selection),]
  })

  getDataCompare <- reactive({
    d <- data[which(data$vote %in% c(input$selectionx, input$selectiony)),]
    d$vote <- as.character(d$vote)
    d$vote[d$vote==input$selectionx] <- "x"
    d$vote[d$vote==input$selectiony] <- "y"
    d <- reshape(d, direction="wide", timevar="vote", idvar="canton", 
                 v.names=c("Stimmbeteiligung","Ja","suffrage"))
    names(d) <- gsub(paste0(input$key,"."), "", names(d), fixed = TRUE)
    d <- d
    })
  
  # Tab 1
  output$scatter <- renderPlot({
    
    d <- getData()

    p <- ggplot(d) + 
      geom_point(aes(Ja, Stimmbeteiligung, fill=ling, size=sqrt(suffrage)*2),
                 alpha=0.8, color="black", shape=21) +
      scale_x_continuous(labels=scales::percent_format(), limits=c(0,1)) +
      scale_y_continuous(labels=scales::percent_format(), limits=c(0,1)) +
      scale_fill_brewer(palette="Set1") +
      labs(title=paste("Kantonsresultate: ", input$selection, sep=""),
           subtitle="Darstellung: Clau Dermont",
           y="Stimmbeteiligung", 
           x="Ja in %") +
      theme_minimal() +
      theme(legend.position="none",
            text=element_text(size=15))
    p
  })
  
  #Tab 2
  output$scattercompare <- renderPlot({
    
    d <- getDataCompare()
    
    p <- ggplot(d) + 
      geom_point(aes(x, y, fill=ling, size=sqrt(suffrage.x)*2),
                 alpha=0.9, color="black", shape=21) +
#     geom_smooth(aes(x, y), method=lm, se=F) +
      scale_x_continuous(labels=scales::percent_format(), limits=c(0,1)) +
      scale_y_continuous(labels=scales::percent_format(), limits=c(0,1)) +
      scale_fill_brewer(palette="Set1") +
      labs(title="Vorlagenvergleich",
           subtitle=paste0(input$key, " in %. Darstellung: Clau Dermont"),
           x=paste0(substr(input$selectionx, 1, 45)), 
           y=paste0(substr(input$selectiony, 1, 45))) +
      theme_minimal() +
      theme(legend.position="none",
            text=element_text(size=15))
    p
  })
  
  output$hover_info <- renderUI({
    d <- getData()
    hover <- input$plot_hover
    point <- nearPoints(d, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Kanton: </b>", point$canton, "<br/>",
                    "<b> Ja: </b>", point$Ja*100, "%<br/>",
                    "<b> Beteiligung: </b>", point$Stimmbeteiligung*100, "%")))
    )
  })
  output$hover_info2 <- renderUI({
    d <- getDataCompare()
    hover <- input$plot_hover2
    point <- nearPoints(d, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Kanton: </b>", point$canton, "<br/>",
                    "<b> X: </b>", point$x*100, "%<br/>",
                    "<b> Y: </b>", point$y*100, "%")))
    )
  })
  
}

shinyApp(ui = ui, server = server)
