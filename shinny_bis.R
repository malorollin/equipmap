library(leaflet)
#library(maps)
library(rgdal)
library(sp)
library(tidyverse)
library(readxl)
#library(lmvar)


indicateurs <- read_csv("Data/indicateur3.csv")
data_dpt <- read_csv("Data/equipment_final.csv")[-1]


departments_shp <- readOGR( dsn=getwd(), layer = "departements-20140306-100m")
departments_shp <- departments_shp[!departments_shp$code_insee %in%c("971", "972", "973", "974", "975", "976", "69M"),]
departments_shp <- departments_shp[order(departments_shp$code_insee),]

equipment_category <- c("Justice","Justice","Justice","Justice","Justice","Justice","Divers","Divers","Transport","Transport","Transport","Transport","Healthcare","Healthcare","Healthcare","Healthcare","Healthcare","Healthcare","Healthcare","Education","Education","Education","Global","Global","Sport","Sport","Sport","Sport","Sport","Sport","Sport","Sport","Sport")
eq_per_category <- data.frame(eq = names(data_dpt)[-1], cat = equipment_category)
