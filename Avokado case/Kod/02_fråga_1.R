setwd("/Users/viktorsjoberg/Desktop/Avokado case")
library(dplyr)   
library(readr)
library(ggplot2)
library(scales)
library(tidyverse)
library(broom)



avocado.fin <- read_rds("Data/avocado.fin.rds")
avocado.land <- read_rds("Data/avocado.land.rds")
avocado.region <- read_rds("Data/avocado.region.rds")
avocado.state <- read_rds("Data/avocado.state.rds")
avocado.city <- read_rds("data/avocado.city.rds")


################### fin  ###################
grouped_data <- avocado.fin %>% group_by(year, region)
annual_avg <- grouped_data %>% summarize(AnnualAverage = mean(net.income))
split_data <- split(grouped_data, grouped_data$region)
annual_avg <- lapply(split_data, function(x) summarize(x, AnnualAverage = mean(net.income)))

combined_data <- bind_rows(annual_avg)
sorted_data <- combined_data %>% arrange(desc(AnnualAverage))
top_regions <- sorted_data %>% top_n(5, AnnualAverage)

y <- split(combined_data, f = combined_data$region)

avocado.california.short <- filter(combined_data, region == "California")
avocado.totalus.short <- filter(combined_data, region == "TotalUS")
avocado.northeast.short <- filter(combined_data, region == "Northeast")
avocado.southcentral.short <- filter(combined_data, region == "SouthCentral")
avocado.west.short <- filter(combined_data, region == "West") 



dataframes <- c("avocado.california.short", "avocado.totalus.short",
                "avocado.northeast.short", "avocado.southcentral.short",
                "avocado.west.short")

combined_data_img <- data.frame()

for (df_name in dataframes) {
  df <- get(df_name)
  combined_data_img <- rbind(combined_data_img, df)
}
combined_data_img$year <- as.numeric(combined_data_img$year)
Arlig.medelpris <- ggplot(combined_data_img, aes(x = year, y = AnnualAverage, color = region)) +
  geom_line() +
  labs(x = "Year", y = "Annual Average", color = "Region") +
  scale_color_discrete(name = "Region") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(Arlig.medelpris)
ggsave("img/Årligt_medelpris.png", plot = Arlig.medelpris, width = 12, height = 6, bg = "white", device = "png")

################### region ###################

grouped_data.region <- avocado.region %>% group_by(year, region)
annual_avg.region <- grouped_data.region %>% summarize(AnnualAverage = mean(net.income))
split_data.region <- split(grouped_data.region, grouped_data.region$region)
annual_avg.region <- lapply(split_data.region, function(x) summarize(x, AnnualAverage = mean(net.income)))

combined_data.region <- bind_rows(annual_avg.region)
sorted_data.region <- combined_data.region %>% arrange(desc(AnnualAverage))
top_regions.region <- sorted_data.region %>% top_n(5, AnnualAverage)

y.region <- split(combined_data.region, f = combined_data.region$region)

avocado.california.short.region <- filter(combined_data.region, region == "California")
avocado.greatlakes.short.region <- filter(combined_data.region, region == "GreatLakes")
avocado.northeast.short.region <- filter(combined_data.region, region == "Northeast")
avocado.southcentral.short.region <- filter(combined_data.region, region == "SouthCentral")
avocado.west.short.region <- filter(combined_data.region, region == "West") 



dataframes.region <- c("avocado.california.short.region", "avocado.greatlakes.short.region",
                "avocado.northeast.short.region", "avocado.southcentral.short.region",
                "avocado.west.short.region")

combined_data_img.region <- data.frame()

for (df_name in dataframes.region) {
  df <- get(df_name)
  combined_data_img.region <- rbind(combined_data_img.region, df)
}
combined_data_img.region$year <- as.numeric(combined_data_img.region$year)
Arlig.medelpris.region <- ggplot(combined_data_img.region, aes(x = year, y = AnnualAverage, color = region)) +
  geom_line() +
  labs(x = "Year", y = "Annual Average", color = "Region") +
  scale_color_discrete(name = "Region") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(Arlig.medelpris.region)
ggsave("img/Årligt_medelpris.region.png", plot = Arlig.medelpris.region, width = 12, height = 6, bg = "white", device = "png")




################### state  ###################
grouped_data.state <- avocado.state %>% group_by(year, region)
annual_avg.state <- grouped_data.state %>% summarize(AnnualAverage = mean(net.income))
split_data.state <- split(grouped_data.state, grouped_data.state$region)
annual_avg.state <- lapply(split_data.state, function(x) summarize(x, AnnualAverage = mean(net.income)))

combined_data.state <- bind_rows(annual_avg.state)
sorted_data.state <- combined_data.state %>% arrange(desc(AnnualAverage))
top_regions.state <- sorted_data.state %>% top_n(5, AnnualAverage)

y.state <- split(combined_data.state, f = combined_data.state$region)

avocado.california.short.state <- filter(combined_data.state, region == "California")
avocado.newyork.short.state <- filter(combined_data.state, region == "NewYork")
avocado.southcarolina.short.state <- filter(combined_data.state, region == "SouthCarolina")
avocado.westtexnewmexico.short.state <- filter(combined_data.state, region == "WestTexNewMexico")

dataframes.state <- c("avocado.california.short.state", "avocado.newyork.short.state",
                "avocado.southcarolina.short.state", "avocado.westtexnewmexico.short.state")

combined_data_img.state <- data.frame()

for (df_name in dataframes.state) {
  df <- get(df_name)
  combined_data_img.state <- rbind(combined_data_img.state, df)
}
combined_data_img.state$year <- as.numeric(combined_data_img.state$year)
Arlig.medelpris.state <- ggplot(combined_data_img.state, aes(x = year, y = AnnualAverage, color = region)) +
  geom_line() +
  labs(x = "Year", y = "Annual Average", color = "Region") +
  scale_color_discrete(name = "State") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(Arlig.medelpris.state)
ggsave("img/Årligt_medelpris.state.png", plot = Arlig.medelpris.state, width = 12, height = 6, bg = "white", device = "png")







################### city  ###################

grouped_data.city <- avocado.city %>% group_by(year, region)
annual_avg.city <- grouped_data.city %>% summarize(AnnualAverage = mean(net.income))
split_data.city <- split(grouped_data.city, grouped_data.city$region)
annual_avg.city <- lapply(split_data.city, function(x) summarize(x, AnnualAverage = mean(net.income)))

combined_data.city <- bind_rows(annual_avg.city)
sorted_data.city <- combined_data.city %>% arrange(desc(AnnualAverage))
top_regions.city <- sorted_data.city %>% top_n(5, AnnualAverage)

y.city <- split(combined_data.city, f = combined_data.city$region)

avocado.baltimorewashington.short.city <- filter(combined_data.city, region == "BaltimoreWashington")
avocado.chicago.short.city <- filter(combined_data.city, region == "Chicago")
avocado.dallasftworth.short.city <- filter(combined_data.city, region == "DallasFtWorth")
avocado.losangeles.short.city <- filter(combined_data.city, region == "LosAngeles")
avocado.sanfrancisco.short.city <- filter(combined_data.city, region == "SanFrancisco") 



dataframes.city <- c("avocado.baltimorewashington.short.city", "avocado.chicago.short.city",
                "avocado.dallasftworth.short.city", "avocado.losangeles.short.city",
                "avocado.sanfrancisco.short.city")

combined_data_img.city <- data.frame()

for (df_name in dataframes.city) {
  df <- get(df_name)
  combined_data_img.city <- rbind(combined_data_img.city, df)
}
combined_data_img.city$year <- as.numeric(combined_data_img.city$year)
Arlig.medelpris.city <- ggplot(combined_data_img.city, aes(x = year, y = AnnualAverage, color = region)) +
  geom_line() +
  labs(x = "Year", y = "Annual Average", color = "Region") +
  scale_color_discrete(name = "Region") +
  theme_minimal()+
  scale_y_continuous(labels = comma)
print(Arlig.medelpris.city)
ggsave("img/Årligt_medelpris.city.png", plot = Arlig.medelpris.city, width = 12, height = 6, bg = "white", device = "png")


model <- lm(net.income ~ region + year + `Total Volume` + `AveragePrice`, data = avocado.city)
model_summary <- tidy(model)
top_regions_model <- model_summary %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(p.value)) %>%
  top_n(10)

top_regions_model


