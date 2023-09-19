setwd("/Users/viktorsjoberg/Desktop/Avokado case")
library(dplyr)   
library(readr)
library(ggplot2)
library(scales)
library(tidyverse)

avocado.fin <- read_rds("Data/avocado.fin.rds")
avocado.organic <- read_rds("Data/avocado.organic.rds")
avocado.conventional <- read_rds("Data/avocado.conventional.rds")
avocado.land <- read_rds("Data/avocado.land.rds")


#################################################################################### Type  #################################################################################### 


organic.modell <- lm(net.income ~ type, data = avocado.land)
summary(organic.modell)

medel.organic <- mean(avocado.organic$net.income)
medel.organic

medel.conventional <- mean(avocado.conventional$net.income)
medel.conventional

pris.organic <- mean(avocado.organic$AveragePrice)
pris.organic

pris.conventional <- mean(avocado.conventional$AveragePrice)
pris.conventional

########################################## Net.income ########################################## 

grouped_data.type <- avocado.land %>% group_by(year, type)
annual_avg.type <- grouped_data.type %>% summarize(AnnualAverage = mean(net.income))
split_data.type <- split(grouped_data.type, grouped_data.type$type)
annual_avg.type <- lapply(split_data.type, function(x) summarize(x, AnnualAverage = mean(net.income)))

combined_data.type <- bind_rows(annual_avg.type)
sorted_data.type <- combined_data.type %>% arrange(desc(AnnualAverage))
top_type <- sorted_data.type %>% top_n(5, AnnualAverage)

y.type <- split(combined_data.type, f = combined_data.type$type)

avocado.conventional.short <- filter(combined_data.type, type == "conventional")
avocado.organic.short <- filter(combined_data.type, type == "organic")

dataframes.type <- c("avocado.organic.short", "avocado.conventional.short")

combined_data_img.type <- data.frame()

for (df_name in dataframes.type) {
  df <- get(df_name)
  combined_data_img.type <- rbind(combined_data_img.type, df)
}
combined_data_img.type$year <- as.numeric(combined_data_img.type$year)
Arlig.medelpris.type <- ggplot(combined_data_img.type, aes(x = year, y = AnnualAverage, color = type)) +
  geom_line() +
  labs(x = "Year", y = "Annual Average", color = "type") +
  scale_color_discrete(name = "type") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(Arlig.medelpris.type)
ggsave("img/Årligt_medelpris.type.png", plot = Arlig.medelpris.type, width = 12, height = 6, bg = "white", device = "png")


########################################## Medelpris ########################################## 

grouped_data.type.price <- avocado.land %>% group_by(year, type)
annual_avg.type.price <- grouped_data.type.price %>% summarize(AnnualAverage = mean(AveragePrice))
split_data.type.price <- split(grouped_data.type.price, grouped_data.type.price$type)
annual_avg.type.price <- lapply(split_data.type.price, function(x) summarize(x, AnnualAverage = mean(AveragePrice)))

combined_data.type.price <- bind_rows(annual_avg.type.price)
sorted_data.type.price <- combined_data.type.price %>% arrange(desc(AnnualAverage))
top_type.price <- sorted_data.type.price %>% top_n(5, AnnualAverage)

y.type <- split(combined_data.type.price, f = combined_data.type.price$type)

avocado.conventional.short.price <- filter(combined_data.type.price, type == "conventional")
avocado.organic.short.price <- filter(combined_data.type.price, type == "organic")

dataframes.type.price <- c("avocado.organic.short.price", "avocado.conventional.short.price")

combined_data_img.type.price <- data.frame()

for (df_name in dataframes.type.price) {
  df <- get(df_name)
  combined_data_img.type.price <- rbind(combined_data_img.type.price, df)
}
combined_data_img.type.price$year <- as.numeric(combined_data_img.type.price$year)
Arlig.medelpris.type.price <- ggplot(combined_data_img.type.price, aes(x = year, y = AnnualAverage, color = type)) +
  geom_line() +
  labs(x = "Year", y = "Annual Average", color = "type") +
  scale_color_discrete(name = "type") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(Arlig.medelpris.type.price)
ggsave("img/Årligt_medelpris.type.price.png", plot = Arlig.medelpris.type.price, width = 12, height = 6, bg = "white", device = "png")


#################################################################################### PLU  #################################################################################### 


arlig_income.plu <- aggregate(cbind(net.income.46, net.income.25, net.income.70) ~ year, data = avocado.land, FUN = mean)
arlig_medelpris_plu <- ggplot(arlig_income.plu, aes(x = year)) +
  geom_line(aes(y = net.income.46, color = "Avocado 46")) +
  geom_line(aes(y = net.income.25, color = "Avocado 25")) +
  geom_line(aes(y = net.income.70, color = "Avocado 70")) +
  labs(x = "År", y = "Genomsnittlig försäljning", color = "Avokadosort") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(arlig_medelpris_plu)
ggsave("img/Årlig_medelpris_plu.png", plot =arlig_medelpris_plu, width = 12, height = 6, bg = "white", device = "png")

#################################################################################### Kombinerat  #################################################################################### 

avg_income.org.comb <- aggregate(cbind(net.income.46, net.income.25, net.income.70) ~ year, data = avocado.organic, FUN = mean)
avg_income.conv.comb <- aggregate(cbind(net.income.46, net.income.25, net.income.70) ~ year, data = avocado.conventional, FUN = mean)

arlig_medel_pris_plu_organic <- ggplot(data = avg_income.org.comb, aes(x = year)) +
  geom_line(aes(y = net.income.46, color = "46")) +
  geom_line(aes(y = net.income.25, color = "25")) +
  geom_line(aes(y = net.income.70, color = "70")) +
  labs(x = "Datum", y = "Snittpris", color = "organic") +
  scale_color_manual(values = c("46" = "blue", "25" = "red", "70" = "green")) +
  ggtitle("Snittpris för organiska avokado") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(arlig_medel_pris_plu_organic)
ggsave("img/Årlig_medelpris_organic_plu.png", plot =arlig_medel_pris_plu_organic, width = 12, height = 6, bg = "white", device = "png")

arlig_medel_pris_plu_conventional <- ggplot(data = avg_income.conv.comb, aes(x = year)) +
  geom_line(aes(y = net.income.46, color = "46")) +
  geom_line(aes(y = net.income.25, color = "25")) +
  geom_line(aes(y = net.income.70, color = "70")) +
  labs(x = "År", y = "Snittpris", color = "conventional") +
  scale_color_manual(values = c("46" = "blue", "25" = "red", "70" = "green")) +
  ggtitle("Snittpris för konventionella avokado") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(arlig_medel_pris_plu_conventional)
ggsave("img/Årlig_medelpris_conventional_plu.png", plot =arlig_medel_pris_plu_conventional, width = 12, height = 6, bg = "white", device = "png")



