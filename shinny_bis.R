#lancer ce fichier avant le shinny_local.R pour charger toutes les données nécessaires 
#(il faut auparavant lancer chacun de vos 3 Rmd pour charger les dataframe que je renome ici)


#ici on charge les départements : pour ccela il faut avoir à la racinde du projet les fichiers nécessaire (demander à gabiche)
departments_shp <- readOGR( dsn=getwd(), layer = "departements-20140306-100m")
departments_shp <- departments_shp[!departments_shp$code_insee %in%c("971", "972", "973", "974", "975", "976", "69M"),]
departments_shp <- departments_shp[order(departments_shp$code_insee),]

#je renomme proprement les 3 df
data_ed <- PS_per_capita #cf owen Equipment_Public_secor.rmd
data_health <- health_per_inhabitant_and_year %>% rename(per_inhabitant = per_person)#cf jo Filtre_sante
data_tp <- transport_per_inhabitant_and_year #cf vic Victoire.rmd


#comme j'avais pas les catégories associées à chaque équipement je fabrique un df temporaire avec les code et catégories que j'utilise dans le shiny
#je ferai la modif rapide très vite et ça changera pas grand chose
equi_ed <- equipment_name[c(1:8),]
equi_ed <- equi_ed %>% mutate (category = "Education")
equi_tp <- data.frame(typequ = transport_per_inhabitant_and_year %>% distinct(typequ), name="name",category="Transport")
equi_health <- data.frame(typequ = health_per_inhabitant_and_year %>% distinct(typequ), name="name",category="Healthcare")

equi_total <- rbind(equi_ed,equi_health,equi_tp)

#charger la fonction en dehors du shiny (peut etre pas utile)
get_map <- function(data, Year, eq){
  dist <- data %>% dplyr::filter(year==Year,typequ == eq) %>% pull(per_inhabitant)
  bins <- quantile(dist, probs =c(0:6)/6)
  bins <- bins[!duplicated(bins)]
  pal <- colorBin("YlOrRd", domain = dist, bins = bins)
  
  res <- leaflet(data = departments_shp) %>% 
    addTiles() %>% 
    setView(lat = 48.5, lng = 2.5, zoom = 5) %>% 
    addPolygons(data= departments_shp, fillColor = ~pal(dist),   weight = 1, opacity = 0.5,   color = "white", dashArray = "3", fillOpacity = 0.7,
                highlight = highlightOptions(color = "red", bringToFront = TRUE), label=~code_insee)
  return(res)
}

