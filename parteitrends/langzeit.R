library(tidyverse)
library(cowplot)
#library(extrafont)
#loadfonts(device="win")

setwd("//Nas-sowi.campus.unibe.ch/sowi/homes/ipw/dermont/Git/r_shinycd/parteitrends")

cantonal <- read.table("www/cantonalhistory.csv", sep=";", header=T, 
                       stringsAsFactors = F, fileEncoding="UTF-8")

# select parties
parties <- c("FDP","CVP","SP","SVP","GPS")
d <- cantonal[1:nrow(cantonal), c(1:3, which(names(cantonal) %in% parties))]    
d$"weitere" <- d[,3]-rowSums(d[,4:length(d)])
d <- select(d, -Total)

# generate trend
dx <- d %>%
  gather("Partei", "Sitze", 3:length(d)) %>% 
  group_by(Kanton, Partei) %>% 
  mutate(No = row_number(), 
         Trend = ifelse(No == 1, 0, Sitze - lag(Sitze))) %>% 
  ungroup() %>% 
  group_by(Partei, Wahljahr) %>% 
  mutate(Trend.sumyear = sum(Trend, na.rm=T)) %>% 
  ungroup() %>% 
  select(-Kanton, -Sitze, -No, -Trend) %>% 
  unique() %>% 
  arrange(Partei, Wahljahr) 

dx$Partei <- factor(dx$Partei, levels=c("FDP", "CVP", "SP", "SVP", "GPS", "weitere"))

# first plot: same as since 2015
dx %>% 
  group_by(Partei) %>% 
  mutate(Trend.sumyear = ifelse(is.na(Trend.sumyear), 0, Trend.sumyear), 
         Trend.longtime = cumsum(Trend.sumyear)) %>% 
  ggplot(aes(x=Wahljahr, color=Partei)) +
  geom_hline(aes(yintercept=0), linetype=2, color="grey27") +
  geom_line(aes(y=Trend.longtime, group=Partei), size=1, alpha=0.5) +
  geom_pointrange(aes(y=Trend.sumyear, ymin=0, ymax=Trend.sumyear)) +
  geom_point(aes(y=Trend.sumyear), size=1) +
  geom_point(aes(y=Trend.longtime), size=0.6) +
  scale_color_manual(values = 
                       c("FDP"="darkblue",
                         "CVP"="orange",
                         "SP"="firebrick",
                         "SVP"="yellowgreen",
                         "GPS"="forestgreen",
                         "weitere"="grey")) +
  facet_wrap(~Partei) +
  labs(title="Mandatsstärke bei kantonalen Parlamentswahlen seit 1968", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS.",
       y="Mandatsstärke",
       x="Punkt: Sitzverschiebung pro Jahr, Linie: kumulierte Veränderung") +
  theme_minimal() +
  theme(text = element_text(size=12),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        legend.position="none", 
        strip.text.x=element_text(face="bold"), 
        text=element_text(family='Archivo Narrow'))

ggsave("img/trendplot.png",
       dpi=300,
       units=c("cm"),
       width=21, height=9)


# second plot = eigth year lag in line, 
dx %>% 
  group_by(Partei) %>% 
  mutate(Trend.sumyear = ifelse(is.na(Trend.sumyear), 0, Trend.sumyear), 
         Trend.longtime = Trend.sumyear + 
           ifelse(is.na(lag(Trend.sumyear)), 0, lag(Trend.sumyear)) +
           ifelse(is.na(lag(Trend.sumyear, n=2)), 0, lag(Trend.sumyear, n=2)) +
           ifelse(is.na(lag(Trend.sumyear, n=3)), 0, lag(Trend.sumyear, n=3)) +
           ifelse(is.na(lag(Trend.sumyear, n=4)), 0, lag(Trend.sumyear, n=4)) + 
           ifelse(is.na(lag(Trend.sumyear, n=5)), 0, lag(Trend.sumyear, n=5)) +
           ifelse(is.na(lag(Trend.sumyear, n=6)), 0, lag(Trend.sumyear, n=6)) +
           ifelse(is.na(lag(Trend.sumyear, n=7)), 0, lag(Trend.sumyear, n=7))) %>% 
  ggplot(aes(x=Wahljahr, color=Partei)) +
  geom_hline(aes(yintercept=0), linetype=2, color="grey27") +
  geom_line(aes(y=Trend.longtime, group=Partei), size=1, alpha=0.5) +
  geom_pointrange(aes(y=Trend.sumyear, ymin=0, ymax=Trend.sumyear)) +
  geom_point(aes(y=Trend.sumyear), size=1) +
  geom_point(aes(y=Trend.longtime), size=0.6) +
  scale_color_manual(values = 
                       c("FDP"="darkblue",
                         "CVP"="orange",
                         "SP"="firebrick",
                         "SVP"="yellowgreen",
                         "GPS"="forestgreen",
                         "weitere"="grey")) +
  facet_wrap(~Partei) +
  labs(title="Sitzverschiebungen bei kantonalen Parlamentswahlen seit 1968", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS.",
       y="Sitzverschiebungen",
       x="Punkt: Sitzverschiebung pro Jahr, Linie: kumulierte Veränderung der letzten acht Jahre") +
  theme_minimal() +
  theme(text = element_text(size=12),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        legend.position="none", 
        strip.text.x=element_text(face="bold"), 
        text=element_text(family='Archivo Narrow'))

ggsave("img/trendplot_8years.png",
       dpi=300,
       units=c("cm"),
       width=21, height=9)


# generate trendline, no points, initial included
parties <- c("FDP", "CVP", "SP", "SVP", "EVP", "GLP", 
             "BDP", "GPS")
d <- cantonal[1:nrow(cantonal), c(1:3, which(names(cantonal) %in% parties))]    
d$"weitere" <- d[,3]-rowSums(d[,4:length(d)])
d <- select(d, -Total)

initial <- matrix(ncol=2, nrow=length(parties)+1)
colnames(initial) <- c("Partei", "Start")
initial[,1] <- c(parties, "weitere")
initial[,2] <- c(767,827,566,291,33,0,0,0,329)
initial <- data.frame(initial)
initial$Start <- as.numeric(as.character(initial$Start))

dx <- d %>%
  gather("Partei", "Sitze", 3:length(d)) %>% 
  group_by(Kanton, Partei) %>% 
  mutate(No = row_number(), 
         Trend = ifelse(No == 1, 0, Sitze - lag(Sitze))) %>% 
  ungroup() %>% 
  group_by(Partei, Wahljahr) %>% 
  mutate(Trend.sumyear = sum(Trend, na.rm=T)) %>% 
  ungroup() %>% 
  select(-Kanton, -Sitze, -No, -Trend) %>% 
  unique() %>% 
  arrange(Partei, Wahljahr)
  
dx$Partei <- factor(dx$Partei, levels=c("FDP", "CVP", "SP", "SVP", "EVP", "GLP", 
                                        "BDP", "GPS", "weitere"))

dline <- dx %>% 
  group_by(Partei) %>% 
  mutate(Trend.sumyear = ifelse(is.na(Trend.sumyear), 0, Trend.sumyear), 
         Trend.longtime = cumsum(Trend.sumyear, na.rm=T)) %>% 
  full_join(initial, by="Partei") %>% 
  mutate(Trend.longtime = Start + Trend.longtime) %>% 
  filter(Trend.longtime > 0)

trend <- ggplot(dline, aes(x=Wahljahr, color=Partei)) +
  geom_line(aes(y=Trend.longtime, group=Partei), size=1, alpha=0.5) +
  geom_point(aes(y=Trend.longtime), size=0.6) +
  scale_color_manual(values = 
                       c("FDP"="darkblue",
                         "CVP"="orange",
                         "SP"="firebrick",
                         "SVP"="yellowgreen",
                         "GPS"="forestgreen",
                         "GLP"="lawngreen",
                         "BDP"="gold",
                         "EVP"="yellow",
                         "weitere"="grey")) +
  labs(title="Mandatsstärke bei kantonalen Parlamentswahlen seit 1968", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS.",
       y="Mandatsstärke",
       x="") +
  theme_minimal() +
  theme(text = element_text(size=12),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        strip.text.x=element_text(face="bold"), 
        text=element_text(family='Archivo Narrow'))

yann <- axis_canvas(trend, axis = "y") +
  geom_text(data = filter(dline, Wahljahr==2018),
            aes(y = Trend.longtime, label = paste0(" ", Partei), color = Partei),
            x = 0, hjust = 0, size = 12/.pt) +
  scale_color_manual(values = 
                       c("FDP"="darkblue",
                         "CVP"="orange",
                         "SP"="firebrick",
                         "SVP"="yellowgreen",
                         "GPS"="forestgreen",
                         "GLP"="lawngreen",
                         "BDP"="gold",
                         "EVP"="yellow2",
                         "weitere"="grey"))
  
trend_ann <- insert_yaxis_grob(trend + theme(legend.position = "none"), yann,
                                    width = grid::unit(0.1, "null"))
ggdraw(trend_ann)

ggsave("img/trendplot_nopoints.png",
       dpi=300,
       units=c("cm"),
       width=21, height=9)