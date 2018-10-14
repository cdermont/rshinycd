cantonal <- read.table("www/cantonalhistory.csv", sep=";", header=T, 
                       stringsAsFactors = F, fileEncoding = "UTF-8")

d <- cantonal[which(cantonal$Kanton %in% input$cant), ] 

d <- d[,c(1:14,28:30)]

d <- gather(d, "partei", "sitze", 4:length(d))
d <- na.omit(d)

d$sitze <- as.numeric(d$sitze)
d$partei <- factor(d$partei, levels=c(
  "kleine.Linksparteien", "PdA", "POCH", "PSA", "Sol.", 
  "FGA", "GPS", "SP", "DSP", "weitere",
  "kleine.Rechtsparteien", "LS","FPS", "Rep.", 
  "SD", "MCR", "Lega", "EDU", "Dem.", "SVP",  "FDP", "LPS",  
  "BDP", "CVP", "GLP", "EVP", "CSP", "LdU"
))

d$li <- 0
d$li[d$partei=="kleine.Linksparteien" | d$partei=="PdA" | 
       d$partei=="POCH" | d$partei=="PSA" | d$partei=="Sol." |
       d$partei=="FGA" | d$partei=="GPS" | d$partei=="SP" |
       d$partei=="DSP"] <- 1

d$sitze[d$li==0] <- (0-d$sitze[d$li==0])

g<-ggplot() + 
  geom_area(data=d[d$li==1,],
            aes(x=Wahljahr,
                y=sitze,
                fill=partei),
            stat="identity", 
            position="stack", 
            alpha=0.8) +
  geom_area(data=d[d$li==0,],
            aes(x=Wahljahr,
                y=sitze,
                fill=partei),
            stat="identity", 
            position="stack", 
            alpha=0.8) +
  coord_flip() +
  scale_x_reverse() +
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
       y="Sitze",
       x="Zeit", 
       fill="Partei") +
  theme_minimal() +
  theme(text = element_text(size=12),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm")) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))
g