setwd("/Users/viktorsjoberg/Desktop/Avokado case")
library(readr)
library(ggplot2)
library(png)


### FRÅGA 1 
svar1 <- readPNG("img/Årligt_medelpris.city.png")
plot(as.raster(svar1)) 
### SVAR: Los angeles 

## FRÅGA 2 
svar2 <- readPNG("img/Årlig_medelpris_plu.png")
plot(as.raster(svar2)) 
## SVAR PLU 46

## FRÅGA 3
svar3 <- readPNG("img/förutsägelse.png")
plot(as.raster(svar3)) 


