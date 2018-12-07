
library(shiny)
library(leaflet)
library(maps)
library(dplyr)


#l'application affiche un message d'erreur tant qu'on n'a pas appuyé sur le bouton d'affichage : pas très grave car ça disparait (mais il faudra quand même le régler)

ui <- fluidPage(
  
  titlePanel("Public equipment in France"),

  
  sidebarLayout(
    sidebarPanel(
      radioButtons("Year",label = "Year", choices = c("2012", "2017")),
      #par défaut, liste de tous les équipements de toutes les catégories
      selectInput("Equipement", label = "Equipement :" , choices=equi_total %>% dplyr::pull(typequ),selected = NULL), 
      actionButton( "Print", "Go")
       ),
    mainPanel(
      tabsetPanel(id="tabs", 
        #chaque onglet à sa map (car chaque onglet charge un df différent -> si on unifie toutes les données en 1 df, on pourrait utiliser une même map)
        tabPanel("Healthcare", leafletOutput("mymap_health")),
        tabPanel("Transport", leafletOutput("mymap_tp")),
        tabPanel("Education", leafletOutput("mymap_ed"))
      )
    )
  )
)


server <- function(input, output, session) {
  library(tidyverse)
  
  #fonction poour obtenir la map d'un équupement donnée, pour une année
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
  
  #mise à jour des propositions d'équipements à afficher suivant l'onglet dans lequel on se trouve
  observeEvent(input$tabs,{
    updateSelectInput(session = session, inputId = "Equipement",choices = equi_total %>% filter(category == input$tabs) %>% pull(typequ),selected = NULL)
  })
  
  #chargement de la map que si on appuie sur le bouton
  map_tp <- reactive({
      input$Print
      isolate({
        get_map(data = data_tp, Year= input$Year, eq=input$Equipement)
      })
    })
  map_health <- reactive({
    input$Print
    isolate({
      get_map(data = data_health, Year= input$Year, eq=input$Equipement)
    })
  })
  map_ed <- reactive({
    input$Print
    isolate({
      get_map(data = data_ed, Year= input$Year, eq=input$Equipement)
    })
  })
  
  #les output correspondant aux 3 maps réactives
  output$mymap_tp <- renderLeaflet({
    map_tp()
  })
  
  output$mymap_health <- renderLeaflet({
    map_health()
  })
  
  output$mymap_ed <- renderLeaflet({
    map_ed()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

