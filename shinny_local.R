
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
      radioButtons("Equipement", label = "equipement :" , choices=data %>% distinct(n) %>% pull(n)),
      textOutput("t")
       ),
    mainPanel(
      tabsetPanel(id="tabs", 
        tabPanel("Healthcare", leafletOutput("mymap")),
        tabPanel("Transport"),
        tabPanel("Education")
      )
    )
  )
)


server <- function(input, output, session) {
  library(prenoms)
  library(tidyverse)
  data(prenoms)

  data = data.frame(category = c("Healthcare","Healthcare","Healthcare","Transport","Education"),n =c("aze","dede","ognh","geb","hube"))
  
  observeEvent(input$tabs,{
    updateRadioButtons(session = session, inputId = "Equipement",choices = data %>% filter(category == input$tabs) %>% distinct(n) %>% pull(n))
  })
  
  output$mymap <- renderLeaflet({
    m %>% addProviderTiles(providers$MtbMap) %>%
      addProviderTiles(providers$Stamen.TonerLines,options = providerTileOptions(opacity = 1)) %>%
      addProviderTiles(providers$Stamen.TonerLabels)
  })
  


}

# Run the application 
shinyApp(ui = ui, server = server)

