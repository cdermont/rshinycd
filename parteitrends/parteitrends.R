rm(list = ls())

if (!require("pacman", quietly=T)) install.packages("pacman")
pacman::p_load('ggplot2','dplyr','tidyr')

trends <- read.table("www/data.csv", sep=";", header=T, fileEncoding = 'UTF-8')

data <- trends

parties <- c('FDP','CVP','SP','SVP','BDP')

d <- data[2:nrow(data), c(1:6, which(names(data) %in% parties))]    
d$"weitere" <- 0-rowSums(d[,7:length(d)])
d <- gather(d, "partei", "diff", 7:length(d))
d <- d %>% group_by(partei) %>% mutate(trend=cumsum(diff))

d$partei <- factor(d$partei, levels=c(parties, "weitere"))

ggplot(d, aes(x=reorder(Kanton, id), color=partei)) +
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
       subtitle="Daten: Bundesamt für Statistik. Darstellung: Clau Dermont",
       y="Sitzverschiebung",
       x="Punkt: Sitzverschiebung pro Wahl, Linie: kumulierte Veränderung") +
  theme_minimal() +
  theme(text = element_text(size=15, family='Archivo Narrow'),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        legend.position="none", 
        strip.text.x=element_text(face="bold"))

ggsave("parteitrends.png", dpi=300,
       units=c("cm"),
       width=28, height=21)


# ohne weitere
parties <- c('FDP','CVP','SP','SVP','BDP', 'GPS')

d <- data[2:nrow(data), c(1:6, which(names(data) %in% parties))]    
d <- gather(d, "partei", "diff", 7:length(d))
d <- d %>% group_by(partei) %>% mutate(trend=cumsum(diff))

d$partei <- factor(d$partei, levels=c(parties))

ggplot(d, aes(x=reorder(Kanton, id), color=partei)) +
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
       subtitle="Daten: Bundesamt für Statistik. Darstellung: Clau Dermont",
       y="Sitzverschiebung",
       x="Punkt: Sitzverschiebung pro Wahl, Linie: kumulierte Veränderung") +
  theme_minimal() +
  theme(text = element_text(size=15, family='Archivo Narrow'),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        legend.position="none", 
        strip.text.x=element_text(face="bold"))

ggsave("20190317_parteitrends.png", dpi=300,
       units=c("cm"),
       width=28, height=21)