library(tidyverse)
library(cowplot)
library(extrafont)
loadfonts(device="win")

setwd("~/Git/r_shinycd/parteitrends")

cantonal <- read.table("www/cantonalhistory.csv", sep=";", header=T, 
                       stringsAsFactors = F, fileEncoding="UTF-8")

elects <- read.table("www/cant_elections.csv", sep=";", header=T, 
                       stringsAsFactors = F, fileEncoding="UTF-8")

cantonal <- full_join(cantonal, filter(elects, Wahljahr > 1967), by=c("Kanton", "Wahljahr"))
cantonal <- select(cantonal, Kanton, Wahljahr, Wahldatum, everything())

cantonal$Wahldatum <- lubridate::dmy(cantonal$Wahldatum)

# select parties
parties <- c("FDP","CVP","SP","SVP","GPS")
d <- cantonal[, c(1:4, which(names(cantonal) %in% parties))]    
d$"weitere" <- d[,4]-rowSums(d[,5:length(d)])
d <- select(d, -Total)

# generate trend per year
dx <- d %>%
  gather("Partei", "Sitze", 4:length(d)) %>% 
  group_by(Kanton, Partei) %>% 
  mutate(No = row_number(), 
         Trend = ifelse(No == 1, 0, Sitze - lag(Sitze))) %>% 
  ungroup() %>% 
  group_by(Partei, Wahljahr) %>% 
  mutate(Trend.sumyear = sum(Trend, na.rm=T)) %>% 
  ungroup() %>% 
  select(-Kanton, -Sitze, -No, -Trend, -Wahldatum) %>% 
  unique() %>% 
  arrange(Partei, Wahljahr) 

dx$Partei <- factor(dx$Partei, levels=c("FDP", "CVP", "SP", "SVP", "GPS", "weitere"))

# plot zero: since 2015
dx %>% 
  filter(Wahldatum>lubridate::ymd("2015-11-08")) %>% 
  group_by(Partei) %>% 
  mutate(Trend.sumyear = ifelse(is.na(Trend.sumyear), 0, Trend.sumyear), 
         Trend.longtime = cumsum(Trend.sumyear)) %>% 
  ggplot(aes(x=Wahljahr, color=Partei)) +
  geom_hline(aes(yintercept=0), linetype=2, color="grey27") +
  geom_line(aes(y=Trend.longtime, group=Partei), size=1, alpha=0.5) +
  geom_pointrange(aes(y=Trend.sumyear, ymin=0, ymax=Trend.sumyear), size=0.3) +
  geom_point(aes(y=Trend.longtime), size=0.6) +
  scale_color_manual(values = 
                       c("FDP"="darkblue",
                         "CVP"="orange",
                         "SP"="firebrick",
                         "SVP"="yellowgreen",
                         "GPS"="forestgreen",
                         "weitere"="grey")) +
  facet_wrap(~Partei) +
  labs(title="Gewinne und Verluste bei kantonalen Parlamentswahlen seit 1968", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS, 1968-2018.",
       y="Mandatsveränderung",
       x="Punkt: Mandatsveränderung pro Jahr, Linie: kumulierte Veränderung") +
  theme_minimal() +
  theme(text = element_text(size=16, family='Archivo Narrow'),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        legend.position="none", 
        strip.text.x=element_text(face="bold"))

ggsave("img/trendplot.png",
       dpi=300,
       units=c("cm"),
       width=40, height=20)


# first plot: same as since 2015
dx %>% 
  group_by(Partei) %>% 
  mutate(Trend.sumyear = ifelse(is.na(Trend.sumyear), 0, Trend.sumyear), 
         Trend.longtime = cumsum(Trend.sumyear)) %>% 
  ggplot(aes(x=Wahljahr, color=Partei)) +
  geom_hline(aes(yintercept=0), linetype=2, color="grey27") +
  geom_line(aes(y=Trend.longtime, group=Partei), size=1, alpha=0.5) +
  geom_pointrange(aes(y=Trend.sumyear, ymin=0, ymax=Trend.sumyear), size=0.3) +
  geom_point(aes(y=Trend.longtime), size=0.6) +
  scale_color_manual(values = 
                       c("FDP"="darkblue",
                         "CVP"="orange",
                         "SP"="firebrick",
                         "SVP"="yellowgreen",
                         "GPS"="forestgreen",
                         "weitere"="grey")) +
  facet_wrap(~Partei) +
  labs(title="Gewinne und Verluste bei kantonalen Parlamentswahlen seit 1968", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS, 1968-2018.",
       y="Mandatsveränderung",
       x="Punkt: Mandatsveränderung pro Jahr, Linie: kumulierte Veränderung") +
  theme_minimal() +
  theme(text = element_text(size=16, family='Archivo Narrow'),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        legend.position="none", 
        strip.text.x=element_text(face="bold"))

ggsave("img/trendplot.png",
       dpi=300,
       units=c("cm"),
       width=40, height=20)


# second plot = eigth year lag in line, 
dx %>% 
  group_by(Partei) %>% 
  mutate(Trend.sumyear = ifelse(is.na(Trend.sumyear), 0, Trend.sumyear), 
         Trend.longtime = Trend.sumyear + 
           ifelse(is.na(lag(Trend.sumyear)), 0, lag(Trend.sumyear)) +
           ifelse(is.na(lag(Trend.sumyear, n=2)), 0, lag(Trend.sumyear, n=2)) +
           ifelse(is.na(lag(Trend.sumyear, n=3)), 0, lag(Trend.sumyear, n=3)) +
           ifelse(is.na(lag(Trend.sumyear, n=4)), 0, lag(Trend.sumyear, n=4))) %>% 
  ggplot(aes(x=Wahljahr, color=Partei)) +
  geom_hline(aes(yintercept=0), linetype=2, color="grey27") +
  geom_line(aes(y=Trend.longtime, group=Partei), size=1, alpha=0.5) +
  geom_pointrange(aes(y=Trend.sumyear, ymin=0, ymax=Trend.sumyear), size=0.3) +
  geom_point(aes(y=Trend.longtime), size=0.6) +
  scale_color_manual(values = 
                       c("FDP"="darkblue",
                         "CVP"="orange",
                         "SP"="firebrick",
                         "SVP"="yellowgreen",
                         "GPS"="forestgreen",
                         "weitere"="grey")) +
  facet_wrap(~Partei) +
  labs(title="Gewinne und Verluste bei kantonalen Parlamentswahlen seit 1968", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS.",
       y="Mandatsveränderung",
       x="Punkt: Mandatsveränderung pro Jahr, Linie: kumulierte Veränderung der letzten vier Jahre") +
  theme_minimal() +
  theme(text = element_text(size=16, family='Archivo Narrow'),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        legend.position="none", 
        strip.text.x=element_text(face="bold"))

ggsave("img/trendplot_4years.png",
       dpi=300,
       units=c("cm"),
       width=40, height=20)


# third plot: same as since 2015, no lag line, each election
dx <- d %>%
  gather("Partei", "Sitze", 4:length(d)) %>% 
  group_by(Kanton, Partei) %>% 
  mutate(No = row_number(), 
         Trend = ifelse(No == 1, 0, Sitze - lag(Sitze))) %>% 
  ungroup() %>% 
  select(-Sitze, -No) %>% 
  unique() %>% 
  arrange(Partei, Wahldatum) 

dx$Partei <- factor(dx$Partei, levels=c("FDP", "CVP", "SP", "SVP", "GPS", "weitere"))

dx %>% 
  group_by(Partei) %>% 
  filter(Wahljahr > 1972) %>% 
  mutate(Trend = ifelse(is.na(Trend), 0, Trend)) %>% 
  ggplot(aes(x=Wahldatum, color=Partei)) +
  geom_hline(aes(yintercept=0), linetype=2, color="grey27") +
  geom_pointrange(aes(y=Trend, ymin=0, ymax=Trend), size=0.3, position=position_dodge(width=0.2)) +
  scale_color_manual(values = 
                       c("FDP"="darkblue",
                         "CVP"="orange",
                         "SP"="firebrick",
                         "SVP"="yellowgreen",
                         "GPS"="forestgreen",
                         "weitere"="grey")) +
  facet_wrap(~Partei) +
  labs(title="Gewinne und Verluste bei kantonalen Parlamentswahlen seit 1972", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS, 1968-2018.",
       y="Mandatsveränderung",
       x="Punkt: Mandatsveränderung pro Wahl") +
  theme_minimal() +
  theme(text = element_text(size=16, family='Archivo Narrow'),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        legend.position="none", 
        strip.text.x=element_text(face="bold"))

ggsave("img/trendplot_onlypoints_election.png",
       dpi=300,
       units=c("cm"),
       width=40, height=20)

# generate trendline, no points, initial included
parties <- c("FDP", "CVP", "SP", "SVP", "EVP", "GLP", 
             "BDP", "GPS")
d <- cantonal[, c(1:4, which(names(cantonal) %in% parties))]    
d$"weitere" <- d[,4]-rowSums(d[,5:length(d)])
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
         Trend.longtime = cumsum(Trend.sumyear)) %>% 
  full_join(initial, by="Partei") %>% 
  mutate(Trend.longtime = Start + Trend.longtime) %>% 
  filter(Trend.longtime > 0)

trend <- dline %>% 
  filter(Wahljahr > 1971) %>% 
  ggplot(aes(x=Wahljahr, color=Partei)) +
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
  labs(title="Mandatsstärke in den kantonalen Parlamenten seit 1968", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS.",
       y="Mandatsstärke",
       x="") +
  theme_minimal() +
  theme(text = element_text(size=16, family='Archivo Narrow'),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        strip.text.x=element_text(face="bold"))

yann <- axis_canvas(trend, axis = "y") +
  geom_text(data = filter(dline, Wahljahr==2018),
            aes(y = Trend.longtime, label = paste0(" ", Partei), color = Partei),
            x = 0, hjust = 0, size = 12/.pt, 
            family='Archivo Narrow') +
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
       width=40, height=20)


# by election instead of year
dx <- d %>%
  gather("Partei", "Sitze", 4:length(d)) %>% 
  group_by(Kanton, Partei) %>% 
  mutate(No = row_number(), 
         Trend = ifelse(No == 1, 0, Sitze - lag(Sitze))) %>% 
  ungroup() %>% 
  select(-Sitze, -No) %>% 
  unique() %>% 
  arrange(Partei, Wahldatum)

dx$Partei <- factor(dx$Partei, levels=c("FDP", "CVP", "SP", "SVP", "EVP", "GLP", 
                                        "BDP", "GPS", "weitere"))

dline <- dx %>% 
  group_by(Partei) %>% 
  mutate(Trend = ifelse(is.na(Trend), 0, Trend), 
         Trend.longtime = cumsum(Trend)) %>% 
  full_join(initial, by="Partei") %>% 
  mutate(Trend.longtime = Start + Trend.longtime) %>% 
  filter(Trend.longtime > 0, !is.na(Wahldatum))

trend <- ggplot(dline, aes(x=Wahldatum, color=Partei)) +
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
  labs(title="Mandatsstärke in den kantonalen Parlamenten seit 1968", 
       subtitle="Darstellung: Clau Dermont. Daten: BFS.",
       y="Mandatsstärke",
       x="") +
  theme_minimal() +
  theme(text = element_text(size=16, family='Archivo Narrow'),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm"),
        strip.text.x=element_text(face="bold"))

yann <- axis_canvas(trend, axis = "y") +
  geom_text(data = filter(dline, Wahldatum==lubridate::ymd("2018-04-15")),
            aes(y = Trend.longtime, label = paste0(" ", Partei), color = Partei),
            x = 0, hjust = 0, size = 12/.pt, 
            family='Archivo Narrow') +
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

ggsave("img/trendplot_nopoints_election.png",
       dpi=300,
       units=c("cm"),
       width=40, height=20)

