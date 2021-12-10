rm(list=ls())
setwd("/Users/William/Desktop/groepsWerk/Final")
load('cngStations_BE.Rdata')
load('lngStations_BE.Rdata')
load('petrolStations_BE.Rdata')
load('Roads.RData')
load("final.RData")
load("Environment.RData")
source("Profitz.R")


for (pack in c('readxl','leaflet', 'shinythemes', 'DT', 'sp','shiny','shinycssloaders')){if(!require(pack, character.only = TRUE)){
  install.packages(pack)}
  require(pack, character.only = TRUE, quietly = TRUE)
}


BELGIUM$car_daily_avg <- BELGIUM$total_daily_avg - BELGIUM$truck_daily_avg
#Initialise icons

stationIcon <- makeIcon(
  iconUrl = "https://i.ibb.co/TMY5N1d/g.png",
  iconWidth = 32, iconHeight = 40,
)




ui <- fluidPage(
  theme = shinytheme("sandstone"),
  
  # App title ----
  titlePanel("dirkOIL"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          title = "Advanced",
          value = 2,
          selectInput(inputId = "traffic_choice2",
                      label = "Select traffic type: ",
                      c("Total","Car","Truck")),
          sliderInput(inputId = "nrstation", label = "Number of LNG stations",0,10,
                      value = 10, step = 1),
          sliderInput("distance","Min. distance between LNG Stations", 5, 12,
                      value = 10, step = 1
          ),
          sliderInput("range","Range", 50, 400,
                      value = 200, step = 25
          ),
          sliderInput(inputId="capConstraint",  label="Maximum capacity",200000,500000,value = 300000),
          
          selectInput(inputId = "location",
                      label = "Select region ",
                      c("Belgium","Flanders","Wallonia")),
          selectInput(inputId = "partner",
                      label = "Select partner : ",
                      c("No partner","ASoil","Avia","CNG","Dats24","DCB","DKV","Esso","Gabriels","Gulf","GV","Independent_benzine","LPG_Autogas","Maes","Missil","Octa","Pollet","Power","Q8","shell","Texaco","Total_Petrol","Devagro","Drive","Eke","Electrabel","Elva","Enora","GOW!","GPS","IMOG","Zellik","Romac")),
          
          actionButton(inputId = "calc", label = "Calculate")
          
          
          
          
          
        ),
        
        tabPanel(
          title = "Statistics",
        value = 1,
        h3(textOutput("profits")),
        h4(textOutput("revenue")),
        
        h4(textOutput("costs")),
        
        
        downloadButton(outputId = 'downloadFile', 'Download')
        
      
    
      
      
      
      ),
  
      id = "tabselected"
      
      
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      conditionalPanel(condition="input.tabselected==1",
      DT::dataTableOutput(outputId = 'table')
   
      ),
      conditionalPanel(condition="input.tabselected==2",
                       tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                       
                       leafletOutput(outputId = "map"),
                       absolutePanel(
                         right = 20, width = 200, top = 1,
                         draggable = TRUE,
                         wellPanel(
                           
                           checkboxInput("traffic2", "Show traffic", FALSE),
                           checkboxInput("extra","Show extra",FALSE),
                           
                           conditionalPanel("input.extra == true",
                                            checkboxInput("Terminal2", "Show terminal", FALSE),
                                            conditionalPanel("input.Terminal2 == true",
                                            checkboxInput("Connector2", "Show connectors", TRUE)
                                            )
                                            
                           )
                           
                           
                         ),
                         style = "opacity: 0.90"
                       )
      )
      
      
                     
                       )
                       
      

                    
                                
                   
                    
      )
      
      
    )
  

  

server <- function(input, output,session) {
  customMap <- eventReactive(input$calc, 
               { 
                bool = TRUE
               progress <- shiny::Progress$new()
               on.exit(progress$close())
               progress$set(message = "Computing data", value = 0)
               
               updateProgress <- function(message=NULL,value = NULL, detail = NULL) {
                 detail2 = paste0(round(value*10),"%")
                 
                 progress$set(message=message ,value = value, detail = detail2)
               }
               
               solveProblem(Price_Per_km = 20,updateProgress,inputstations = input$partner, 
                            Capacity_constraint = input$capConstraint, 
                            inputnumberstations=input$nrstation,
                            Traffic_choice= input$traffic_choice2 , 
                            Dist_between_stations = input$distance,
                            range=input$range, 
                            fueltype='All fuel types', 
                            Location=input$location)
               
               
               },ignoreInit = FALSE
                      )
  
  

  
  ################## Custom solution

  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data=customMap(),~lon, ~lat,icon  = stationIcon,label=~paste0("Station ",Station))
  })
  
  ########## Add traffic
  observe({
    proxy <- leafletProxy("map", data = BELGIUM)
    
    if(input$traffic2){
      proxy %>% clearShapes() %>% clearControls()
      pal <- colorNumeric(c("blue"),429)
      
      if(input$traffic_choice2=="Total"){
        proxy %>% addProviderTiles(providers$CartoDB.Positron) %>%
          addCircles(lng = ~lon, lat = ~lat, weight = 0, 
                     radius = ~sqrt(total_daily_avg)*15, 
                     layerId = LETTERS[1:nrow(BELGIUM)])
        
      }
      else if(input$traffic_choice2=="Car"){
        proxy %>% addProviderTiles(providers$CartoDB.Positron) %>%
          addCircles(lng = ~lon, lat = ~lat, weight = 0, 
                     radius = ~sqrt(car_daily_avg)*15, 
                     layerId = LETTERS[1:nrow(BELGIUM)])
      }
      
      
      else if(input$traffic_choice2=="Truck"){
        proxy %>% addProviderTiles(providers$CartoDB.Positron) %>%
          addCircles(lng = ~lon, lat = ~lat, weight = 0, 
                     radius = ~sqrt(truck_daily_avg)*15, 
                     layerId = LETTERS[1:nrow(BELGIUM)])
      }
      
    }
    else{
      proxy %>% clearShapes() %>% clearControls()
    }
  })
  
  
########## Add location terminal
  
  observe({
    proxy <- leafletProxy("map", data = BELGIUM)
    
    if(input$extra){
      
    if (input$Terminal2) {
      proxy <- leafletProxy("map",data=customMap())
      
      proxy <- proxy %>% 
        addMarkers(~terminal_lon, ~terminal_lat, popup= ~name_terminal,label = ~name_terminal,group="Terminal")
      
      if(input$Connector2){
        
        for(i in 1:input$nrstation){
          proxy <- proxy %>% addPolylines(
            lat = as.numeric(customMap()[i, 
                                         c('terminal_lat',
                                           'lat')]), 
            lng = as.numeric(customMap()[i, 
                                         c('terminal_lon', 
                                           'lon')]),
            group="Connector",
            label = ~paste0("Station ",Station, ", terminal :",name_terminal , sep=" "),
            
            
          )
          
          
        }
        proxy  
      }
      else {
        clearGroup(proxy, "Connector")
        
      }
    }
    else{
      clearGroup(proxy, "Terminal")
      clearGroup(proxy, "Connector")
      

    }
  }
  else{
    proxy %>% clearShapes() %>% clearControls()
    
  }})
  
  
  
  ################## Standard

  
output$table <- DT::renderDataTable(
  datatable(data = as.data.frame(customMap()[c("Station","region","Total","Truck","Car","Cost","name_terminal")], 
                                 rownames = FALSE))
)

output$downloadFile <- downloadHandler(
  filename = function() {
    "output.csv"
  },
  content = function(file) {
    write.csv(customMap(), file, row.names = FALSE)
  },
  contentType = "text/csv"
)

output$profits <- renderText({ 
  paste0("Total profits = €",round(sum(customMap()['Total'])))
  
})


output$revenue <- renderText({ 
  paste0("Total revenue = €",(round(sum(customMap()['Total']))-round(sum(customMap()['Cost']))))
  
})



output$costs <- renderText({ 
  paste0("Total cost = €",(round(sum(customMap()['Cost']))))
  
})

}

shinyApp(ui, server)


