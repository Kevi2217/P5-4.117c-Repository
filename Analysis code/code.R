library(ggplot2)
library(dplyr)

ggplot(cityhomes, aes(Pris_Salg,Areal_Bolig)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


