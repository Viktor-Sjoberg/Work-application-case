setwd("/Users/viktorsjoberg/Desktop/Avokado case")
library(readr)
library(dplyr)  

avocado <- read_csv("Data/avocado (1).csv")

avocado$net.income <- avocado$AveragePrice*avocado$`Total Volume` 
avocado <- avocado %>% 
  rename("avo46" = "4046",
         "avo25" = "4225",
         "avo70" = "4770")

avocado$net.income.46 <- avocado$AveragePrice*avocado$avo46
avocado$net.income.25 <- avocado$AveragePrice*avocado$avo25
avocado$net.income.70 <- avocado$AveragePrice*avocado$avo70

write_rds(avocado, "Data/avocado.fin.rds")




########################### Plats ########################### 
avocado.land <- subset(avocado, region %in% c("TotalUS"))
write_rds(avocado.land, "Data/avocado.land.rds")

avocado.region <- subset(avocado, region %in% c("West", "Southeast", "SouthCentral", "Plains", "NorthernNewEngland", "Northeast", "Midsouth", "GreatLakes"))
write_rds(avocado.region, "Data/avocado.region.rds")

avocado.state <- subset(avocado, region %in% c("California", "NewYork", "SouthCarolina", "WestTexNewMexico"))
write_rds(avocado.state, "Data/avocado.state.rds")

avocado.city <- subset(avocado, region %in% c("Albany", "Atlanta", "BaltimoreWashington", "Boise", "Boston", "BuffaloRochester", "Charlotte", "Chicago", 
                                              "CincinnatiDayton", "Columbus", "DallasFtWorth", "Denver", "Detroit", "GrandRapids",  
                                              "HarrisburgScranton", "HartfordSpringfield", "Houston", "Indianapolis", "Jacksonville", "LasVegas",
                                              "LosAngeles", "Louisville", "MiamiFtLauderdale", "Nashville", "NewOrleansMobile", "Orlando",
                                              "Philadelphia", "PhoenixTucson", "Pittsburgh", "Portland", "RaleighGreensboro", "RichmondNorfolk",
                                              "Roanoke", "Sacramento", "SanDiego", "SanFrancisco", "Seattle", "Spokane", "StLouis", "Syracuse", "Tampa"
                                              ))
write_rds(avocado.city, "Data/avocado.city.rds")


########################### Typ ########################### 

avocado.conventional <- subset(avocado.land, type %in% "conventional")
write_rds(avocado.conventional, "Data/avocado.conventional.rds")

avocado.organic <- subset(avocado.land, type %in% "organic")
write_rds(avocado.organic, "Data/avocado.organic.rds")


##########################################################
x <- split(avocado, f = avocado$region)

avocado.albany <- data.frame(x["Albany"]) # Stad

avocado.atlanta <- data.frame(x["Atlanta"]) # Stad

avocado.baltimorewashington <- data.frame(x["BaltimoreWashington"]) # Stad

avocado.boise <- data.frame(x["Boise"]) # Stad

avocado.boston <- data.frame(x["Boston"]) # Stad

avocado.buffalorochester <- data.frame(x["BuffaloRochester"]) # Stad 

avocado.california <- data.frame(x["California"]) # Stat 

avocado.charlotte <- data.frame(x["Charlotte"]) # Stad

avocado.chicago <- data.frame(x["Chicago"]) # Stad

avocado.cincinnatidayton <- data.frame(x["CincinnatiDayton"]) # Stad
 
avocado.columbus <- data.frame(x["Columbus"]) # Stad

avocado.dallasftworth <- data.frame(x["DallasFtWorth"]) # Stad

avocado.denver <- data.frame(x["Denver"]) # Stad

avocado.detroit <- data.frame(x["Detroit"]) # Stad

avocado.grandrapids <- data.frame(x["GrandRapids"]) # Stad

avocado.greatlakes <- data.frame(x["GreatLakes"]) # Region

avocado.harrisburgscranton <- data.frame(x["HarrisburgScranton"]) # Stad

avocado.hartfordspringfield <- data.frame(x["HartfordSpringfield"]) # Stad

avocado.houston <- data.frame(x["Houston"]) # Stad 

avocado.indianapolis <- data.frame(x["Indianapolis"]) # Stad

avocado.jacksonville <- data.frame(x["Jacksonville"]) # Stad

avocado.lasvegas <- data.frame(x["LasVegas"]) # Stad

avocado.losangeles <- data.frame(x["LosAngeles"]) # Stad
write_rds(avocado.losangeles, "Data/avocado.losangeles.rds")
grouped_data.la <- avocado.losangeles %>% group_by(LosAngeles.year)
annual_avg.la <- grouped_data.la %>% summarize(AnnualAverage = mean(LosAngeles.net.income))
split_data.la <- split(grouped_data.la, grouped_data.la$LosAngeles.region)
annual_avg.la <- lapply(split_data.la, function(x) summarize(x, AnnualAverage = mean(LosAngeles.net.income)))

combined_data.la <- bind_rows(annual_avg.la)
sorted_data.la <- combined_data.la %>% arrange(desc(AnnualAverage))

sorted_data.la <- sorted_data.la %>% 
  rename("year" = "LosAngeles.year",)
write_rds(sorted_data.la, "Data/LA.YEARLY.rds")



avocado.louisville <- data.frame(x["Louisville"]) # Stad

avocado.miamiftlauderdale <- data.frame(x["MiamiFtLauderdale"]) # Stad 

avocado.midsouth <- data.frame(x["Midsouth"]) # Region

avocado.nashville <- data.frame(x["Nashville"]) # Stad 

avocado.neworleansmobile <- data.frame(x["NewOrleansMobile"]) # Stad

avocado.newyork <- data.frame(x["NewYork"]) # Stat

avocado.northeast <- data.frame(x["Northeast"]) # Region 

avocado.northernnewengland <- data.frame(x["NorthernNewEngland"]) # Region 

avocado.orlando <- data.frame(x["Orlando"]) # Stad 

avocado.philadelphia <- data.frame(x["Philadelphia"]) # Stad 

avocado.phoenixtucson <- data.frame(x["PhoenixTucson"]) # Stad 

avocado.pittsburgh <- data.frame(x["Pittsburgh"]) # Stad

avocado.plains <- data.frame(x["Plains"]) # region 

avocado.portland <- data.frame(x["Portland"]) # Stad 

avocado.raleighgreensboro <- data.frame(x["RaleighGreensboro"]) # Stad

avocado.richmondnorfolk <- data.frame(x["RichmondNorfolk"]) # Stad

avocado.roanoke <- data.frame(x["Roanoke"]) # stad

avocado.sacramento <- data.frame(x["Sacramento"]) # Stad

avocado.sandiego <- data.frame(x["SanDiego"]) # Stad

avocado.sanfrancisco <- data.frame(x["SanFrancisco"]) # Stad

avocado.seattle <- data.frame(x["Seattle"]) # Stad
 
avocado.southcarolina <- data.frame(x["SouthCarolina"]) # Stat

avocado.southcentral <- data.frame(x["SouthCentral"]) # Region

avocado.southeast <- data.frame(x["Southeast"]) # Region

avocado.spokane <- data.frame(x["Spokane"]) # Stad

avocado.stlouis <- data.frame(x["StLouis"]) # stad

avocado.syracuse <- data.frame(x["Syracuse"]) # stad

avocado.tampa <- data.frame(x["Tampa"]) # stad 

avocado.totalus <- data.frame(x["TotalUS"]) # Land

avocado.west <- data.frame(x["West"]) # Region

avocado.westtexnewmexico <- data.frame(x["WestTexNewMexico"]) #stat 

###################################################################