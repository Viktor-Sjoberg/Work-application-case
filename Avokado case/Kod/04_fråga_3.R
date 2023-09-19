setwd("/Users/viktorsjoberg/Desktop/Avokado case")
library(dplyr)   
library(readr)
library(tidyverse)
library(ggplot2)
library(forecast)  


avocado.losangeles <- read_rds("Data/avocado.losangeles.rds")
avo.la <- avocado.losangeles[, c("LosAngeles.Date", "LosAngeles.net.income")]
avo.la <- avo.la %>% 
  rename("date" = "LosAngeles.Date",
         "net.income" = "LosAngeles.net.income")

avo.la$date <- as.Date(avo.la$date, format = "%m/%d/%y")

la.ts <- ts(avo.la$net.income, start = c(2015, 01),
            end = c(2018, 03), frequency = 52)


LA.modell <- auto.arima(la.ts)
LA.forecast <- forecast(LA.modell, h = 52)
max.value <- max(LA.forecast$mean)
min.value <- min(LA.forecast$mean)


prediction <- autoplot(LA.forecast) +
  geom_hline(yintercept = max.value, color = "blue") +
  geom_hline(yintercept = min.value, color = "red") +
  scale_y_continuous(labels = comma)

prediction
ggsave("img/förutsägelse.png",  plot = prediction, width = 12, height = 6, bg = "white", device = "png")



