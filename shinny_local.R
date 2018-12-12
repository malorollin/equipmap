

library(shiny)
library(leaflet)
library(maps)
library(dplyr)


#l'application affiche un message d'erreur tant qu'on n'a pas appuyé sur le bouton d'affichage : pas très grave car ça disparait (mais il faudra quand même le régler)

ui <-navbarPage("Menu",
           tabPanel("Affichage",
                    sidebarLayout(
                      sidebarPanel(
                        #radioButtons("Year",label = "Year", choices = c("2012", "2017")),
                        #par défaut, liste de tous les équipements de toutes les catégories
                        selectInput("category",label = "category :" ,choices = c("Healthcare", "Transport", "Divers"),selected = NULL),
                        selectInput( "Equipement_name",label = "Equipement :" ,choices = names(tot)[-c(1, 2)],selected = NULL),
                        actionButton("Print", "Go")
                      ),
                      mainPanel(
                        leafletOutput("mymap")
                      )
                    )
           ),
           tabPanel("Prediction",
                    verbatimTextOutput("summary")
           ),
           navbarMenu("More",
                      tabPanel("Table"),
                      tabPanel("About")
           )
)


server <- function(input, output, session) {
  library(tidyverse)
  
  #fonction poour obtenir la map d'un équupement donnée, pour une année
  get_map <- function(eq, data = tot) {
    dist <- data %>% pull(eq)
    bins <- quantile(dist, probs = c(0:6) / 6)
    bins <- bins[!duplicated(bins)]
    pal <- colorBin("YlOrRd", domain = dist, bins = bins)
    
    res <- leaflet(data = departments_shp) %>%
      addTiles() %>%
      setView(lat = 48.5,
              lng = 2.5,
              zoom = 5) %>%
      addPolygons(
        data = departments_shp,
        fillColor = ~ pal(dist),
        weight = 1,
        opacity = 0.5,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(color = "red", bringToFront = TRUE),
        label =  ~ code_insee
      )
    return(res)
  }
  
  #mise à jour des propositions d'équipements à afficher suivant l'onglet dans lequel on se trouve
  observeEvent(input$category, {
    updateSelectInput(
      session = session,
      inputId = "Equipement_name",
      choices = eq_per_category %>% dplyr::filter(cat == as.character(input$category)) %>% dplyr::select(eq),
      selected = NULL
    )
  })
  
  
  #chargement de la map que si on appuie sur le bouton
  map <- reactive({
    input$Print
    isolate({
      get_map(eq = input$Equipement_name)
    })
  })
  
  #les output correspondant aux 3 maps réactives
  output$mymap <- renderLeaflet({
    map()
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
