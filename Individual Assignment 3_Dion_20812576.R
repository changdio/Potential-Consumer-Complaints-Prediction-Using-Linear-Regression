library(tidyverse)
library(ggplot2)
library(psych)
library(scales)
library(ggiraphExtra)
library(car)

df <- read.csv("complaints.csv")

df <- df %>% 
  mutate(Date.received = as.Date(Date.received)) %>% 
  mutate(year.month = format(Date.received, '%Y-%m')) %>% 
  group_by(year.month) %>% 
  summarize(complaints.count = n())

# Plot 
ggplot(df, aes(y=complaints.count, x=year.month)) +
  geom_point() +
  labs(x="Date", y="Number of complaints") +
  scale_x_discrete(breaks = c("2012-01","2013-01","2014-01","2015-01","2016-01","2017-01","2018-01","2019-01","2020-01","2021-01"))

# Remove outliers
df.no <- df %>%
  filter(row_number() <= n()-12)


# Plot 
ggplot(df.no, aes(y=complaints.count, x=year.month)) +
  geom_point() +
  labs(x="Date", y="Number of complaints") +
  scale_x_discrete(breaks = c("2012-01","2013-01","2014-01","2015-01","2016-01","2017-01","2018-01","2019-01","2020-01")) +
  theme(axis.text.x = element_text(size = 8))

# Encode date to numeric
df.no$date.num <- 1:nrow(df.no)
df.no <- df.no %>% relocate(date.num)
df.no

# Plot 
ggplot(df.no, aes(y=complaints.count, x=date.num)) +
  geom_point() +
  labs(x="Date", y="Number of complaints") 

# Linear regression model
complaints.model <- lm(complaints.count ~ date.num, data = df.no)
complaints.model
summary(complaints.model)

# Graph plot
ggPredict(complaints.model, se=TRUE)

complaints.model$coefficients

# For the year 2022
newValues = tibble(date.num=c(122,123,124,125,126,127,128,129,130,131,132,133))

predict.lm(complaints.model,newValues)

# Combined number of complaints from all months of 2022
sum(predict.lm(complaints.model,newValues))

complaints.model$residuals

# Durbin-Watson test
durbinWatsonTest(complaints.model)

plot(complaints.model, which=1)
plot(complaints.model, which=2)

qqnorm(rstandard(complaints.model))
qqline(rstandard(complaints.model))
