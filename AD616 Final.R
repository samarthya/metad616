historical <- read.csv("SBT Loans Historical Data.csv")
hist(historical$Stated.expected.sales)
install.packages("fitdistrplus")
library(fitdistrplus)
norm_model <- fitdist(historical$Stated.expected.sales, "norm")
binom_model <- fitdist(historical$Stated.expected.sales, "binom")
pois_model <- fitdist(historical$Stated.expected.sales, "pois")
lnorm_model <- fitdist(historical$Stated.expected.sales, "lnorm")
weibull_model <- fitdist(historical$Stated.expected.sales, "weibull")
gofstat(list(norm_model, binom_model, pois_model, lnorm_model, weibull_model))
plot(lnorm_model)
lnorm_model$estimate
hist(historical$Realized..actual..sales)
cor(historical$Stated.expected.sales, historical$Realized..actual..sales)
install.packages("dplyr")
library(dplyr)
historical_optimistic <- historical %>%
  filter(Leading.indicator.of.economic.outlook == 1)
historical_neutral <- historical %>%
  filter(Leading.indicator.of.economic.outlook == 0)
historical_pessimistic <- historical %>%
  filter(Leading.indicator.of.economic.outlook == -1)
cor(historical_optimistic$Stated.expected.sales, historical_optimistic$Realized..actual..sales)
cor(historical_neutral$Stated.expected.sales, historical_neutral$Realized..actual..sales)
cor(historical_pessimistic$Stated.expected.sales, historical_pessimistic$Realized..actual..sales)
install.packages("ggplot2")
library(ggplot2)
ggplot(data = historical) +
  geom_point(mapping = aes(x = Stated.expected.sales, y = Realized..actual..sales)) +
  geom_smooth(mapping = aes(x = Stated.expected.sales, y = Realized..actual..sales)) +
  xlab("Expected Sales") + ylab("Actual Sales")
historical.lm <- lm(historical$Realized..actual..sales ~ historical$Stated.expected.sales + factor(historical$Leading.indicator.of.economic.outlook))
summary(historical.lm)
