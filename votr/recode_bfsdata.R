library(tidyverse)
library(shiny)
library(ggswissmaps)
library(lubridate)


data <- read.table("www/votedata_new.csv", sep=";", header=T, stringsAsFactors = F)

data[,c("suffrage","Stimmbeteiligung","Ja.Stimmen", "Nein.Stimmen", "Ja", "valid", "Abgegebene.Stimmen")] <- 
  lapply(c("suffrage","Stimmbeteiligung","Ja.Stimmen", "Nein.Stimmen", "Ja", "valid", "Abgegebene.Stimmen"), 
         function(x) as.numeric(data[,x]))

data$Ja <- data$Ja/100
data$Stimmbeteiligung <- data$Stimmbeteiligung/100

data$result <- ifelse(data$Ja.Stimmen>data$Nein.Stimmen, "Ja", "Nein")
data$resultnum <- ifelse(data$Ja.Stimmen>data$Nein.Stimmen, 1, 0)

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

# context
context <- read.table("www/context.csv", sep=";", header=T, stringsAsFactors = F)
data <- full_join(data, context, by="canton")

# sort
data$datum <- dmy(substr(data$vote, 1, 10))
data <- arrange(data, desc(datum))

data <- arrange(data, canton, desc(datum))
data <- fill(data, suffrage)

data <- arrange(data, desc(datum))

write.table(data, "www/votedata.csv", sep=";", row.names = F, fileEncoding = "UTF-8")
