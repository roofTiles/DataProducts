#Tejas Guha
#7/28/2018

library(shiny)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
cardioData = read.csv("cardioExplorer/Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv")
races = levels(unique(cardioData$Stratification2))

shinyUI(bootstrapPage(theme = shinytheme("darkly"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$title("Cardiovascular Disease Explorer"),
  tags$head(tags$script(src = "message-handler.js")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
      selectInput("gender", "Gender", c("Male","Female","Overall"), selected="Overall"),
      selectInput("race", "Ethnicity", races, selected="Overall"),
      selectInput("geoLevel", "Geographic Level", c("County", "State"), selected="County"),
      selectInput("color","Color Scheme", rownames(brewer.pal.info)[18:35], selected="YlOrRd"),
      actionButton("docButton", "How to Use")
  )
))
