library(ggplot2)
library(dplyr)

summary(homedata)

names(homedata)

ggplot(homedata, aes(kommunenavn, pris_salg)) +
  geom_boxplot()
