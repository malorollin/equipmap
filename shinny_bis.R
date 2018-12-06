

data <- data.frame(category = c("Healthcare","Transport","Transport","Transport","Transport"),n =c("maternities","E101","E102","E103","E106"))

#maternities<- maternities[-seq(nrow(maternities),nrow(maternities)-3),]
#maternities <- maternities %>% rename("mat_nb_2012" = nb_2012, "mat_nb_2017" = nb_2017)

departments_shp <- readOGR( dsn=getwd(), layer = "departements-20180101")
departments_shp <- departments_shp[!departments_shp$code_insee %in%c("971", "972", "973", "974", "975", "976", "69M"),]
departments_shp <- departments_shp[order(departments_shp$code_insee),]

departments_shp@data


spread_transport_equip_dep

get_map <- function(data, Year, variable){
  var_year<- paste(as.character(variable),"_nb_",as.character(Year),sep = "")
  dist <- data %>% dplyr::select(contains(var_year)) %>% pull()
  bins <- quantile(dist, probs =c(0:6)/6)
  pal <- colorBin("YlOrRd", domain = dist, bins = bins)
  
  res <- leaflet(data = data) %>% 
    addTiles() %>% 
    setView(lat = 48.5, lng = 2.5, zoom = 5) %>% 
    addPolygons(data= departments_shp, fillColor = ~pal(dist),   weight = 1, opacity = 0.5,   color = "white", dashArray = "3", fillOpacity = 0.7,
                highlight = highlightOptions(color = "red", bringToFront = TRUE), label=~code_insee)
  
  return(res)
}


map_base <- leaflet(data = departments_shp) %>% 
  addTiles() %>% 
  setView(lat = 48.5, lng = 2.5, zoom = 5) %>% 
  addPolygons(data= departments_shp, fillColor = "white",   weight = 1, opacity = 0.5,   color = "white", dashArray = "3", fillOpacity = 0.7,
              highlight = highlightOptions(color = "red", bringToFront = TRUE), label=~code_insee)