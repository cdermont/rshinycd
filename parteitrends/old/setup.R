
pacman::p_load(tidyr,dplyr,ggplot2)

setwd("~/Git/projects/parteitrends")
data <- read.table("www/data-new.csv", sep=";", header=T, stringsAsFactors = F)

trends <- data

trends <- separate(trends, Wahl, c("Kanton", "Datum"), sep=" ", remove=F)
trends$Datum <- as.Date(trends$Datum, format="%d.%m.%Y")
trends$id <- seq(1, nrow(trends), 1)

trends$FDP.diff <- trends$FDP.neu - trends$FDP.2015
trends$LPS.diff <- trends$LPS.neu - trends$LPS.2015
trends$FDP.LPS.diff <- trends$FDP.LDP.neu - trends$FDP.LPS.2015
trends$CVP.diff <- trends$CVP.neu - trends$CVP.2015
trends$SP.diff <- trends$SP.neu - trends$SP.2015
trends$SVP.diff <- trends$SVP.neu - trends$SVP.2015
trends$GLP.diff <- trends$GLP.neu - trends$GLP.2015
trends$BDP.diff <- trends$BDP.neu - trends$BDP.2015
trends$GPS.diff <- trends$GPS.neu - trends$GPS.2015
trends$EVP.diff <- trends$EVP.neu - trends$EVP.2015
trends$CSP.diff <- trends$CSP.neu - trends$CSP.2015
trends$EDU.SD.Lega.MCR.diff <- trends$EDU.SD.Lega.MCR.neu - trends$EDU.SD.Lega.MCR.2015
trends$PdA.AL.Sol.PSA.diff <- trends$PdA.AL.Sol.PSA.neu - trends$PdA.AL.Sol.PSA.2015
trends$Links.diff <- trends$Links.neu - trends$Links.2015
trends$Mitte.diff <- trends$Mitte.neu - trends$Mitte.2015
trends$Rechts.diff <- trends$Rechts.neu - trends$Rechts.2015
trends$ueb.diff <- trends$ueb.neu - trends$ueb.2015

trends.l <- select(trends, id, Wahl, Kanton, Datum, Total, ends_with(".diff"))

colnames(trends.l) <- c("id","Wahl","Kanton","Datum","Total","FDP","LDP","FDP.LDP",
                     "CVP","SP","SVP","GLP","BDP","GPS","EVP","CSP","PdA","EDU","AL",
                     "Links","Mitte","Rechts","weitere")
write.table(trends.l, file="www/trends.csv",sep=";", row.names=F)

trends.l <- gather(trends.l, "partei", "diff", 6:22)
trends.l <- trends.l %>% group_by(partei) %>% mutate(trend=cumsum(diff))

trends.l$partei <- factor(trends.l$partei, 
                          levels=c("FDP","LDP","FDP.LDP",
                                   "CVP","SP","SVP","GLP","BDP","GPS","EVP",
                                   "CSP","PdA","EDU","AL",
                                   "Links","Mitte","Rechts","übrige"))

write.table(trends.l, file="www/trendslong.csv",sep=";", row.names=F)

#####

data <- read.table("www/cantonalhistory-old.csv", sep=";", header=T, stringsAsFactors = F)

data <- unique(data)

write.table(data, file="www/cantonalhistory.csv", sep=";", row.names=F)
