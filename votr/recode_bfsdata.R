library(tidyverse)
library(shiny)
library(ggswissmaps)


data <- read.table("www/votedata_new.csv", sep=";", header=T, stringsAsFactors = F)

data[,c("suffrage","Stimmbeteiligung","Ja", "Nein", "japrct", "valid", "Abgegebene.Stimmen")] <- 
  lapply(c("suffrage","Stimmbeteiligung","Ja", "Nein", "japrct", "valid", "Abgegebene.Stimmen"), 
         function(x) as.numeric(data[,x]))

data$japrct <- data$japrct/100
data$Stimmbeteiligung <- data$Stimmbeteiligung/100

data$result <- ifelse(data$Ja>data$Nein, "Ja", "Nein")
data$resultnum <- ifelse(data$Ja>data$Nein, 1, 0)

data$halb <- 1
data$halb[data$canton=="Appenzell Ausserrhoden" |
            data$canton=="Appenzell Innerrhoden" | 
            data$canton=="Basel-Landschaft" | 
            data$canton=="Basel-Stadt" | 
            data$canton=="Nidwalden" | 
            data$canton=="Obwalden"] <- 0.5

data$resultnum <- data$resultnum*data$halb

  
data <- data %>% 
  filter(canton!="Schweiz") %>% 
  select(vote, canton, resultnum) %>% 
  na.omit() %>% 
  group_by(vote) %>% 
  summarise(staendeja = sum(resultnum)) %>% 
  full_join(data, by="vote")


write.table(data, "www/votedata.csv", sep=";", row.names = F, fileEncoding = "UTF-8")
