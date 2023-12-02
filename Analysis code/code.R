library(ggplot2)
library(dplyr)

head(cityhomes)
tail(cityhomes)

summary(cityhomes)

ggplot(cityhomes, aes(Pris_Salg,Areal_Bolig)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


summary(cityhomes)
