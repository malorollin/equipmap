library(leaflet)
library(maps)
library(rgdal)
library(sp)
library(tidyverse)

tot <- read.csv("equipment.csv")

departments_shp <- readOGR( dsn=getwd(), layer = "departements-20140306-100m")
departments_shp <- departments_shp[!departments_shp$code_insee %in%c("971", "972", "973", "974", "975", "976", "69M"),]
departments_shp <- departments_shp[order(departments_shp$code_insee),]

equipment_category <- c("Divers","Divers","Divers","Divers","Divers","Divers","Divers","Divers","Transport","Transport","Transport","Transport","Healthcare","Healthcare","Healthcare","Healthcare","Healthcare","Healthcare","Healthcare")
eq_per_category <- data.frame(eq = names(tot)[-c(1,2)], cat = equipment_category)

get_map <- function(eq, data=tot){
  dist <- data %>% pull(eq)
  bins <- quantile(dist, probs =c(0:6)/6)
  bins <- bins[!duplicated(bins)]
  pal <- colorBin("YlOrRd", domain = dist, bins = bins)
  
  res <- leaflet(departments_shp) %>% 
    addTiles() %>% 
    setView(lat = 48.5, lng = 2.5, zoom = 5) %>% 
    addPolygons(data= departments_shp, fillColor = ~pal(dist),   weight = 1, opacity = 0.5,   color = "white", dashArray = "3", fillOpacity = 0.7,
                highlight = highlightOptions(color = "red", bringToFront = TRUE), label=~code_insee)
  return(res)
}

niv<- read_excel("pred/C09-ISD_Niveau_De_Vie_Des_Menages.xls")
niveau <- niv[-c(1:5,102:110),]
niveau<- niveau %>% select(c(1,3)) %>% rename(niveau = X__2, dep =1 ) %>% mutate(niveau=as.numeric(niveau)) %>% arrange(dep)

pauv<- read_excel("pred/C07-ISD_Pauvrete_Monetaire.xls")
pauvrete <- pauv[-c(1:5,102:110),]
pauvrete<- pauvrete %>% select(c(1,3)) %>% rename(pauvrete = X__2, dep =1 ) %>% mutate(pauvrete=as.numeric(pauvrete)) %>% arrange(dep)

new <- cbind(tot,pauvrete %>% select(pauvrete),niveau %>% select(niveau))
