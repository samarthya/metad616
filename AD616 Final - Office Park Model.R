historical <- read.csv("SBT Loans Historical Data.csv")
library(ggplot2)
library(car)
library(dplyr)
library(readr)
library(fitdistrplus)
library(scales) 

#check historical data structure
View(historical)
str(historical)
summary(historical)


#historgram of realized actual sales
ggplot(historical, aes(x = Realized..actual..sales)) +
  geom_histogram(binwidth = 5000, fill = "blue", color = "black") +
  labs(title = "Distribution of Realized Actual Sales",x = "Realized Sales ($)",y = "Number of Projects") +scale_x_continuous(labels = comma) +
  theme_minimal(base_size = 13)

#regression model
model <- lm(Realized..actual..sales ~ Stated.expected.sales + Leading.indicator.of.economic.outlook, data = historical)
summary(model)

#residual plot for regression model
ggplot(data = data.frame(Fitted = fitted(model), Residuals = resid(model)), aes(x = Fitted, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot of Regression Model", x = "Fitted Sales($)", y = "Residuals($)") + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13)

#distributions to expected sales
lnorm_model <- fitdist(historical$Stated.expected.sales, "lnorm")
norm_model <- fitdist(historical$Stated.expected.sales, "norm")
weibull_model <- fitdist(historical$Stated.expected.sales, "weibull")
#compare and graphs
gofstat(list(lnorm_model, norm_model, weibull_model))
plot(lnorm_model)
lnorm_model$estimate 

#density plot
ggplot() +
  geom_density(data = historical, aes(x = Stated.expected.sales), fill = "blue", alpha = 0.4) +
  geom_density(data = historical, aes(x = Realized..actual..sales), fill = "red", alpha = 0.4) +
  labs(title = "Density of Expected vs Actual Sales",x = "Sales ($)", y = "Density") + scale_x_continuous(labels = comma) + scale_y_continuous(labels = scientific_format(digits = 2)) +
  theme_minimal(base_size = 13)

#Scatterplot
ggplot(historical, aes(x = Stated.expected.sales, y = Realized..actual..sales, color = factor(Leading.indicator.of.economic.outlook))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Expected vs Actual Sales by Economic Outlook",
       x = "Expected Sales ($)", y = "Actual Sales ($)", color = "Outlook (-1=Pess, 0=Neutral, 1=Pos)") + 
  scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 13)

#loan acceptance
interest_rate<- data.frame(Rate = c(7, 8, 9, 10, 11),Probability = c(1.00, 0.75, 0.50, 0.25, 0.00))

ggplot(interest_rate, aes(x = factor(Rate), y = Probability)) +
  geom_col(fill = "blue") +
  labs(title = "Loan Acceptance Probability by Interest Rate",
       x = "Interest Rate (%)", y = "Acceptance Probability")+ scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 13)

cor(historical$Stated.expected.sales, historical$Realized..actual..sales)
historical_optimistic <- historical %>%
  filter(Leading.indicator.of.economic.outlook == 1)
historical_neutral <- historical %>%
  filter(Leading.indicator.of.economic.outlook == 0)
historical_pessimistic <- historical %>%
  filter(Leading.indicator.of.economic.outlook == -1)
cor(historical_optimistic$Stated.expected.sales, historical_optimistic$Realized..actual..sales)
cor(historical_neutral$Stated.expected.sales, historical_neutral$Realized..actual..sales)
cor(historical_pessimistic$Stated.expected.sales, historical_pessimistic$Realized..actual..sales)
ggplot(data = historical) +
  geom_point(mapping = aes(x = Stated.expected.sales, y = Realized..actual..sales)) +
  geom_smooth(mapping = aes(x = Stated.expected.sales,  y = Realized..actual..sales)) +
  xlab("Expected Sales") + ylab("Actual Sales")
n <- 10000
office_model <- function(n, mean, sd) {
  rlnorm(n, log(mean * (1 + sd^2 / mean^2)^0.5), log(1 + sd^2 / mean^2)^0.5)
}
expected_sim <- office_model(n, mean(historical$Stated.expected.sales), sd(historical$Stated.expected.sales))
prob <- runif(n, 0, 1)
indicators_sim <- ifelse(prob < 12/32, -1, ifelse(prob > 21/32, 1, 0))
df_sim <- data.frame(Stated.expected.sales = expected_sim, Leading.indicator.of.economic.outlook = indicators_sim)
df_sim$Predicted_Actual <- predict(model, df_sim)
hist(df_sim$Predicted_Actual)
mean(df_sim$Predicted_Actual)
mean(df_sim$Predicted_Actual)-qnorm(0.95)*sd(df_sim$Predicted_Actual)/(n^0.5)
mean(df_sim$Predicted_Actual)+qnorm(0.95)*sd(df_sim$Predicted_Actual)/(n^0.5)
min(df_sim$Predicted_Actual)
max(df_sim$Predicted_Actual)
free_cash_flows <- 37875 - 24375 - 9000 - 12500
cumulative_fcf <- 8636.03
loan_inflow <- 38375
df_sim$cFCF <- free_cash_flows + (df_sim$Predicted_Actual - cumulative_fcf - loan_inflow)
hist(df_sim$cFCF)
mean(df_sim$cFCF)
mean(df_sim$cFCF)-qnorm(0.95)*sd(df_sim$cFCF)/sqrt(n)
mean(df_sim$cFCF)+qnorm(0.95)*sd(df_sim$cFCF)/sqrt(n)
min(df_sim$cFCF)
max(df_sim$cFCF)





