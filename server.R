library(shiny)
library(dplyr)
library(leaflet)
library(geojsonio)
library(rgeos)
library(sp)
start <- proc.time()
cardio = read.csv("cardioExplorer/Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv")
cardio$Stratification1 = as.character(cardio$Stratification1)
cardio$Stratification2 = as.character(cardio$Stratification2)
cardio$LocationID = as.character(lapply(cardio$LocationID, function(x){
  if (as.numeric(x) < 60){
    if (as.numeric(x) < 10){
      x = paste("0",as.character(x),sep="")
    }
    return(paste("0400000US", toString(x), sep=""))
  }
  return(paste("0500000US", ifelse(nchar(toString(x)) < 5, paste("0",toString(x), sep=""), toString(x)), sep =""))
}))
cardio = cardio[order(cardio$LocationID),]

print(proc.time() - start)

counties = geojson_read("cardioExplorer/counties.json", what = "sp")
counties = counties[order(counties$GEO_ID),]
counties$GEO_ID = as.character(counties$GEO_ID)

states = geojson_read("cardioExplorer/states.json", what="sp")
states$GEO_ID = as.character(states$GEO_ID)
states = states[order(states$GEO_ID),]

print(proc.time() - start)

shinyServer(function(input, output, session) {
   observeEvent(input$docButton, {
     showModal(modalDialog(
       title = "Documentation",
       HTML("<h1>Cardiovascular Disease Explorer - US Data from 2014</h1><br><h3>Tejas Guha - July 29, 2018</h3>
              <br><h2>How to Navigate the Map</h2><br><ul><li>To pan around just click a point and drag your mouse
            whichever direction you would like to move</li><li>To zoom in, click the plus and minuses to the left,
            or just use your scroll on your mouse</li><li>Hover over the counties/states in the US to see specific
            information</li></ul><h2>Using the Filters</h2><br><ul><li>Click on the filters and select the given options</li>
            <li>Remember to give the app 10 seconds between each change</li><li>The color scheme changes the colors of the states/counties</li></ul>"),
       easyClose = TRUE,
       footer = NULL
     ))
    })
  
   mapData = reactive({
     
     cardioData = filter(cardio, Stratification1==input$gender, Stratification2==input$race, GeographicLevel==input$geoLevel)
     print(proc.time() - start)
     
     if (input$geoLevel == "County"){
       polygons = counties[counties$GEO_ID %in% cardioData$LocationID == TRUE,]
       cardioData = cardioData[cardioData$LocationID %in% polygons$GEO_ID == TRUE,]
       
     }else{
       polygons = states
     }
     
     withProgress(message = 'Filtering and Setting up Data', value = 0, {
    
       polygons$cardio = as.numeric(lapply(cardioData$Data_Value, function(x){
         return(ifelse(is.na(x) || length(x) == 0, 0, x))
       }))
       
       print(proc.time() - start)

       polygons$locationName = apply(cardioData, 1, function(x) {
         location = paste0(x[3], ", ", x[2])
         return(sprintf("<strong>%s</strong><br/>%s deaths per 100,000", location, x[8]))
       })
       
       print(proc.time() - start)
     
     })
     
     return(polygons)
   })
   
   pal = reactive({
     return(colorNumeric(input$color, domain = mapData()$cardio))
   })
   

   output$map = renderLeaflet({
     leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% setView(lat=39, lng=-98, zoom=4)
   })
   
   observe({
     p = pal()
     polygons = mapData()
     print(proc.time() - start)
     
     leafletProxy("map", data = polygons) %>% clearShapes() %>% clearControls() %>% addPolygons(weight=1, color="#FFFFFF", 
       fillColor=~p(polygons$cardio), fillOpacity=1, label=lapply(polygons$locationName, HTML), highlight = highlightOptions(weight = 5,color = "#000",fillOpacity = 1,bringToFront = TRUE)) %>% 
       addLegend(pal=p, values=~polygons$cardio, position="bottomright", na.label = "", title="Mortality Rates", opacity=0.8)
     
     print(proc.time() - start)
  })
   
})
