#library(lattice)
#library(DAAG)
library(shiny)
library(leaflet)
#library(maps)
library(dplyr)


#l'application affiche un message d'erreur tant qu'on n'a pas appuy?? sur le bouton d'affichage : pas tr??s grave car ??a disparait (mais il faudra quand m??me le r??gler)

ui <-fluidPage(navbarPage("Menu",
           tabPanel("Affichage",
                    sidebarLayout(
                      sidebarPanel(
                        #radioButtons("Year",label = "Year", choices = c("2012", "2017")),
                        #par d??faut, liste de tous les ??quipements de toutes les cat??gories
                        selectInput("category",label = "category :" ,choices = eq_per_category$cat,selected = NULL),
                        selectInput( "Equipement_name",label = "Equipement :" ,choices = names(data_dpt)[-c(1, 2)],selected = NULL),
                        actionButton("Print", "Go")
                      ),
                      mainPanel(
                        leafletOutput("mymap")
                      )
                    )
           ),
           tabPanel("Advise",
                    sidebarLayout(
                      sidebarPanel(
                        #radioButtons("Year",label = "Year", choices = c("2012", "2017")),
                        #par d??faut, liste de tous les ??quipements de toutes les cat??gories
                        selectInput("prediction_advise",label = "prediction :" ,choices = c("pauvrete", "Niveau_de_vie","crime_rate"),selected = NULL),
                        #checkboxGroupInput( "predictors",label = "predictors :" ,choices = names(tot)[-c(1, 2)],selected = names(tot)[-c(1, 2)]),
                        actionButton("Print3", "Go")
                      ),
                      mainPanel(
                        dataTableOutput("myadvise"),
                        leafletOutput("map_indic")
                      )
                    )
           ),
           tabPanel("Prediction",
                    sidebarLayout(
                      sidebarPanel(
                        #radioButtons("Year",label = "Year", choices = c("2012", "2017")),
                        #par d??faut, liste de tous les ??quipements de toutes les cat??gories
                        selectInput("prediction",label = "prediction :" ,choices = c("pauvrete", "Niveau_de_vie","crime_rate"),selected = NULL),
                        checkboxGroupInput( "predictors",label = "predictors :" ,choices = names(data_dpt)[-c(1, 2)],selected = names(data_dpt)[-c(1, 2)]),
                        actionButton("Print2", "Go")
                      ),
                      mainPanel(
                        leafletOutput("mypred")
                      )
                    )
           ),
           navbarMenu("More",
                      tabPanel("Table"),
                      tabPanel("About")
           )
)
)


server <- function(input, output, session) {
  library(tidyverse)

  #fonction poour obtenir la map d'un ??quupement donn??e, pour une ann??e
  get_map <- function(eq, data = data_dpt) {
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
  
  predict <- function(variable,pred){
    pred <- fitted(lm(as.formula(paste(variable," ~ ",paste(pred,collapse="+"))),cbind(data_dpt[,-c(1,2)],indicateurs)),x=TRUE,y=TRUE)
    pred <- as.data.frame(pred) %>% pull(pred)
    dif <- abs(pred-indicateurs %>% pull(variable))/mean(indicateurs %>% pull(variable))
    
    bins <-c(0,0.0125,0.025,0.0375,0.05,0.066,0.15,0.5,1)
    #bins <- as.vector(c(0:5,10000)*500)
    bins <- bins[!duplicated(bins)]
    pal <- colorBin("YlOrRd", domain = dif, bins = bins)
    res <- leaflet(data = departments_shp) %>%
      addTiles() %>%
      setView(lat = 48.5,
              lng = 2.5,
              zoom = 5) %>%
      addPolygons(
        data = departments_shp,
        fillColor = ~ pal(dif),
        weight = 1,
        opacity = 0.5,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(color = "red", bringToFront = TRUE),
        label =  ~ code_insee)%>% 
        addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                   position = "bottomright")
    return(res)
    }
  
  equipment_stepwise <- function(index, eq = data_dpt, ind = indicateurs){
    index <- enquo(index)
    data_index <-  eq %>% 
      dplyr::select(-c(1,2))
    col <- ind %>% dplyr::select(column_name = !!index)
    data_index <- add_column(data_index, indicator = col$column_name)
    pred_index <- lm(indicator~., data = data_index)
    invisible(capture.output(pred_index_step <- step(pred_index, direction="both")))
    pred_index_opt <- lm(formula(pred_index_step), data = data_index)
    eq_significant <- pred_index_opt$coefficients
    eq_significant <- eq_significant[-1]
    A <- as.data.frame(eq_significant)
    res <- cbind(as.data.frame(row.names(A)),A)
    res <- res %>% rename(equipment="row.names(A)",coef = "eq_significant")
    return(res)
  }
  
  #mise ?? jour des propositions d'??quipements ?? afficher suivant l'onglet dans lequel on se trouve
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
  
  map_pred <- reactive({
    input$Print2
    isolate({
      predict(input$prediction,input$predictors)
    })
  })
  
  predictor_relevant <- reactive({
    input$Print3
    isolate({
      equipment_stepwise(input$prediction_advise)
      })
  })
  
  map_indic <- reactive({
    input$Print3
    isolate({
      #equipment_stepwise(input$prediction_advise)
      get_map(eq=input$prediction_advise, data=indicateurs)
    })
  })
  
  #les output correspondant aux 3 maps r??actives
  output$mymap <- renderLeaflet({
    map()
  })
  output$mypred <- renderLeaflet({
    map_pred()
  })
  output$myadvise <- renderDataTable({
    predictor_relevant()
  })
  output$map_indic <- renderLeaflet({
    map_indic()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

