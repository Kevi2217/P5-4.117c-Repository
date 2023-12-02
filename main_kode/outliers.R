library(ggplot2)
library(dplyr)

summary(homedata)

names(homedata)

homedata %>%
  mutate(pris_salg = pris_salg/1000000)

ggplot(homedata, aes(kommunenavn, pris_salg)) +
  geom_boxplot()
