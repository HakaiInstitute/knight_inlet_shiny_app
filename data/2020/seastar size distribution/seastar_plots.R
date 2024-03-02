#Written Ondine Pontier
#Last Update made October 2020

setwd("G:/My Drive/Hakai Nearshore/Nanwakolas Diving/Data")

rm(list=ls())
graphics.off()

lapply(c("tidyr", "plyr", "dplyr", "ggplot2", "magrittr",
         "lubridate", "knitr", "tidyverse", "reshape2", "readr"), library, character.only = T)

transects <- read.csv("transects.csv")
transects$size_radius<- as.numeric(as.character(transects$size_radius))

#isolates pycnos
pycnos <- transects %>%
  subset(species == "Pycnopodia helianthoides")


means <- ddply(pycnos, "site", summarise, grp.mean=mean(size_radius))

#plots size class vs density
ggplot(pycnos, aes(x= size_radius, color= site)) +
  geom_histogram(binwidth=1, fill="white", boundary = 0)+
  geom_vline(data=means, aes(xintercept=grp.mean, color=site),
             linetype="dashed")+
  facet_grid( site ~.)

#plots sites separately
pycnos_kni2 <- pycnos %>%
  subset(site == "KNI2")

ggplot(pycnos_kni2, aes(x= size_radius)) +
  geom_histogram(binwidth=1, boundary = 0)+
  theme_bw()

pycnos_kni9 <- pycnos %>%
  subset(site == "KNI9")

ggplot(pycnos_kni9, aes(x= size_radius)) +
  geom_histogram(binwidth=1, boundary = 0)+
  theme_bw()
