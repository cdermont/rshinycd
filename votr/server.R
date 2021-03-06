library(shiny)
library(ggplot2)
library(tidyr)
library(plotly)

data <- read.table("www/votedata.csv", sep=";", header=T, stringsAsFactors = F, fileEncoding = "UTF-8")
data[,c("suffrage","Stimmbeteiligung","Ja")] <- 
  lapply(c("suffrage","Stimmbeteiligung","Ja"), function(x) as.numeric(data[,x]))
data$vote <- factor(data$vote, as.character(unique(data$vote)))

shinyServer(function(input, output){
  
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

  getMap <- reactive({
    chmap <- shp_df[["g1k15"]]
    d <- data[which(data$vote %in% input$selection),]
    mapdata <- full_join(chmap, d, by=c("KTNR"="nr"))
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
  
  output$map <- renderPlot({
    
    mapdata <- getMap()
    
    p <- ggplot(mapdata, aes(x = long, y = lat, group = group, fill=Ja)) +
      geom_polygon(color="white") +
      coord_equal() +
      labs(title=paste("Kantonsresultate: ", input$selection, sep=""),
           subtitle="Darstellung: Clau Dermont",
           fill="Ja in %", y="", x="") +
      scale_fill_gradient2(low="#CC0000", mid="grey95", high="#0066CC", midpoint = 0.5) +
      theme_minimal() +
      theme(panel.grid.major=element_blank(), 
            panel.grid.minor=element_blank(),
            axis.text=element_blank(),
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
  
})