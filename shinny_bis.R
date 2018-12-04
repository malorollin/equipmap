data <- data.frame(category = c("Healthcare","Healthcare","Healthcare","Transport","Education"),n =c("aze","dede","ognh","geb","hube"))


departments_shp <- readOGR( dsn=getwd(),
                            layer = "departements-20180101")

#head(departments_shp)
#summary(departments_shp)
View(departments_shp)

departments_shp@data


bins <- c(0, 1, 2, 3, 4, 5, 7, 8, Inf)*10^-5
pal <- colorBin("YlOrRd", domain = maternities$nb_2012, bins = bins)

leaflet(data = departments_shp) %>% 
  addTiles() %>% 
  setView(lat = 48.5, lng = 2.5, zoom = 5) %>% 
  addPolygons(fillColor = ~pal(maternities$nb_2012),   weight = 1, opacity = 0.5,   color = "white", dashArray = "3", fillOpacity = 0.7,
              highlight = highlightOptions(color = "red", bringToFront = TRUE), label=~code_insee)