
library(shiny)
library(leaflet)
library(maps)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Public equipment in France"),

  
  sidebarLayout(
    sidebarPanel(
      radioButtons("Year",label = "Year", choices = c("2012", "2017")),
      radioButtons("Type",label = "type: ", choices= c("par habitant","nombre total") ),
      radioButtons("Niveau",label = "zoom: ", choices=  c("Urban unit","Department","Region"), selected = "Department" ),
      #radioButtons("Equipement", label = "Equipement :" , choices=data %>% distinct(n) %>% pull(n)),
      #radioButtons("Type", label = "type d'equipement :" , choices=c("Healthcare","Transport","Education")),
      selectInput("Equipement", label = "Equipement :" , choices=c("mat","E101","E102","E103","E106"),selected = NULL), 
      actionButton( "Print", "Go")
       ),
    mainPanel(
      tabsetPanel(id="tabs", 
        tabPanel("Healthcare", leafletOutput("mymap2")),
        tabPanel("Transport", leafletOutput("mymap")),
        tabPanel("Education")
      )
    )
  )
)


server <- function(input, output, session) {
  library(tidyverse)
  
  get_map <- function(data, Year, eq){
    var_year<- paste(as.character(eq),"_nb_",as.character(Year),sep = "")
    dist <- data %>% dplyr::select(contains(var_year)) %>% pull()
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
  
  color_map <- function(data, Year, eq){
    var_year<- paste(as.character(eq),"_nb_",as.character(Year),sep = "")
    dist <- data %>% dplyr::select(contains(var_year)) %>% pull()
    bins <- quantile(dist, probs =c(0:6)/6)
    bins <- bins[!duplicated(bins)]
    pal <- colorBin("YlOrRd", domain = dist, bins = bins)
    
    res <- map_base %>% 
      addPolygons(fillColor = ~pal(dist))
    return(res)
  }
  
  data_eq <- data.frame(category = c("Healthcare","Transport","Transport","Transport","Transport"),n =c("mat","E101","E102","E103","E106"))
  
  observeEvent(input$tabs,{
    updateSelectInput(session = session, inputId = "Equipement",choices = data_eq %>% filter(category == input$tabs) %>% distinct(n) %>% pull(n),selected = NULL)
  })
    
  map <- reactive({
      input$Print
      isolate({
        get_map(data = spread_transport_equip_dep, Year= input$Year, eq=input$Equipement)
      })
    })
  
  output$mymap <- renderLeaflet({
    map()
  })
  
  output$mymap2 <- renderLeaflet({
    color_map(maternities, input$Year, input$Equipement)
  })


}

# Run the application 
shinyApp(ui = ui, server = server)

