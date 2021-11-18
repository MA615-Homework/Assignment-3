#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(shinydashboard)
library(RColorBrewer)
library(maps)
library(tigris)
library(leaflet)
library(stringr)
options(tigris_use_cache = TRUE)
library(magrittr)
library(rvest)
library(reshape2)
#library(ggiraph)
library(RColorBrewer)
library(geojsonio)
library(shinyWidgets)
library(shinythemes)
#library(systemfonts)
library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(leaflet)
library(htmltools)

#add data
addRepo("geanders")
data("hurr_tracks")
data("rain")

##########
#data manipulation
bouys_name<-c('42001','42002','42035','42019','42007','42040',
              '42020','42039','42036')
bouys_lat<-c(25.942,26.055,29.232,27.910,30.090,29.207,
             26.968,28.787,28.501)
bouys_long<-c(89.657,93.646,94.413,95.345,88.769,88.237,
              96.693,86.007,84.508)

buoys <-data.frame(Name=bouys_name,Lat=bouys_lat,Long=bouys_long*-1)





ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Hurricane Exposure</a>'), id="nav",
             windowTitle = "Hurricane Ike 2008",
             
             tabPanel("Hurricane Exposure",
                      sidebarLayout(   
                      sidebarPanel(
                        #first conditional panel for rainfall
                        conditionalPanel(condition = "input.tabselected == 1",
                                         #choose slider range 
                                         sliderInput("lag", "Days included:", 
                                                     min= -5, max= 3, value= c(-5,3) ),
                                         #numeric input of rainfall, value = initial value
                                         numericInput("rain_limit", label = h3("Rain limit"), min = 0, value = 0),
                                         numericInput("dist_limit", label = h3("Distance limit"), min = 0, value = 0)), 
                        
                        conditionalPanel(condition = "input.tabselected == 2",
                                         #numeric input of wind, value = initial value
                                         numericInput("wind_limit", label = h3("Wind limit"), min = 0, value = 0)), 
                        
                        conditionalPanel(condition = "input.tabselected == 3",
                                         #pikcer input of event, value = initial value
                                         pickerInput("event_select", "Event:",   
                                                     choices = c("flood", "tornado", "wind","tropical_storm"), 
                                                     selected = c("flood"),
                                                     multiple = FALSE))      
                        ),
                        #show different types of plot
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Rainfall", value = 1, plotlyOutput("rainfall_plot")),
                            tabPanel("Wind", value = 2, plotlyOutput("wind_plot")),
                            tabPanel("Event", value = 3, plotlyOutput("event_plot")), 
                            id = "tabselected"
                          )
                        )
                        
             )),
             
             tabPanel("Buoys mapper",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("buoy_plot", width="100%", height="100%"))
                      
                      
                      
                      ),
             
             tabPanel("Variogram"),
             
             tabPanel("Data"),
             
             tabPanel("About this site")
             
  )          
)


# Define server logic required to draw a histogram
server <- function(input, output) {
 output$rainfall_plot <- renderPlotly({
   if(input$rain_limit + input$dist_limit > 0){
     map_rain_exposure(storm = "Ike-2008", days_included = input$lag, rain_limit = input$rain_limit, dist_limit = input$dist_limit)+ 
       ggtitle("Ike-2008") + 
       theme(plot.title = element_text(hjust = 0.5))
   }else{
   map_counties(storm = "Ike-2008", metric = "rainfall", days_included = input$lag) +
     ggtitle("Ike-2008") +
     theme(plot.title = element_text(hjust = 0.5))}
 })
 output$wind_plot <- renderPlotly({
   if(input$wind_limit > 0){
     map_wind_exposure(storm = "Ike-2008", wind_limit = input$wind_limit)
   }else{
   map_counties(storm = "Ike-2008", metric = "wind") +
     ggtitle("Ike-2008") +
     theme(plot.title = element_text(hjust = 0.5))}
 })
 output$event_plot <-  renderPlotly({
   map_event_exposure(storm = "Ike-2008", event_type = input$event_select)
 })
 
 output$buoy_plot <-  renderLeaflet({
   
   leaflet(buoys) %>% addTiles() %>%
     addMarkers(~Long, ~Lat, popup = ~htmlEscape(Name))
   
 })
 
 
}

# Run the application 
shinyApp(ui = ui, server = server)
