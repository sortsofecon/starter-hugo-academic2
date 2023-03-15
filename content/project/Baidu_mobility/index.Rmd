---
title: "map"
output: html_document
date: "2023-03-15"
---


```{r setup, include=FALSE}
library(shiny)
library(maps)
library(mapproj)
library(shinythemes)
library(sf)
library(raster)
library(dplyr)
#library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

states <- geojsonio::geojson_read("merge__china-city__new.geojson", what = "sp")
class(states)

## check objects in the data
names(states)

m <- leaflet(states) %>%
  setView(lng = 113.75, lat = 34.77, zoom = 5) %>%
  addTiles()

# add uniform polygons with default styling
m %>% addPolygons(weight = 2, opacity = 1)


# color the states according to certain variable
#bins <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Inf)
library(paletteer) 
states$club_intra_new <- as.numeric(states$club_intra_new)

color <- paletteer_c("grDevices::Green-Orange", n=11)
pal <- colorFactor(c(color), domain = states$club_intra_new)

m %>% addPolygons(
  fillColor = ~pal(club_intra_new),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8)

# add interaction, make the polygons highlight as the mouse passes over them
m %>% addPolygons(
  fillColor = ~pal(club_intra_new),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.8,
    bringToFront = TRUE))


# generate the labels by handcrafting some HTML
# and passing it to lapply(htmltools::HTML)

#labels <- sprintf(
#  "<strong>%s</strong><br/>%g</sup>",
#  states$City_EN, states$club
#) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(club_intra_new),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.8,
    bringToFront = TRUE),
  label = ~paste0(states$City_EN, ": ", formatC(club_intra_new, big.mark = ",")),
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

# redefine the legend
labels <- c("1", "2", "3", "4", "5", "6")

# here should use as.numeric(club21), otherwise the legend will be misplaced
m %>% addLegend(pal = pal, values = ~as.numeric(club_intra_new), opacity = 0.8, title = "Clusters",
                position = "bottomright", na.label = "No data", 
                labFormat = function(type, cuts, p) {  # Here's the trick
                  paste0(labels)
                })


# a basic map with just the outline of the cities
m19 <- leaflet(states) %>%
  setView(lng = 113.75, lat = 34.77, zoom = 5) %>%
  addTiles()

# add uniform polygons with default styling
m19 %>% addPolygons(weight = 2, opacity = 1)


# color the states according to certain variable
#bins <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Inf)
states$work_new <- as.numeric(states$work_new)

pal19 <- colorFactor(c(color), domain = states$work_new)

m19 %>% addPolygons(
  fillColor = ~pal19(work_new),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8)

# add interaction, make the polygons highlight as the mouse passes over them
m19 %>% addPolygons(
  fillColor = ~pal19(work_new),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.8,
    bringToFront = TRUE))

# generate the labels by handcrafting some HTML
# and passing it to lapply(htmltools::HTML)

#labels <- sprintf(
#  "<strong>%s</strong><br/>%g</sup>",
#  states$City_EN, states$club
#) %>% lapply(htmltools::HTML)

m19 <- m19 %>% addPolygons(
  fillColor = ~pal19(work_new),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.8,
    bringToFront = TRUE),
  label = ~paste0(states$City_EN, ": ", formatC(work_new, big.mark = ",")),
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

# redefine the legend
labels19 <- c("0","1", "2", "3", "4", "5", "6", "7", "8", "9")

m19 %>% addLegend(pal = pal19, values = ~as.numeric(work_new), opacity = 0.8, title = "Clusters",
                  position = "bottomright", na.label = "No data", 
                  labFormat = function(type, cuts, p) {  # Here's the trick
                    paste0(labels19)
                  })


m20 <- leaflet(states) %>%
  setView(lng = 113.75, lat = 34.77, zoom = 5) %>%
  addTiles()

# add uniform polygons with default styling
m20 %>% addPolygons(weight = 2, opacity = 1)


# color the states according to certain variable
#bins <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Inf)
states$leisure_new <- as.numeric(states$leisure_new)

pal20 <- colorFactor(c(color), domain = states$leisure_new)

m20 %>% addPolygons(
  fillColor = ~pal20(as.numeric(leisure_new)),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8)

# add interaction, make the polygons highlight as the mouse passes over them
m20 %>% addPolygons(
  fillColor = ~pal20(as.numeric(leisure_new)),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.8,
    bringToFront = TRUE))

# generate the labels by handcrafting some HTML
# and passing it to lapply(htmltools::HTML)

#labels <- sprintf(
#  "<strong>%s</strong><br/>%g</sup>",
#  states$City_EN, states$club
#) %>% lapply(htmltools::HTML)

m20 <- m20 %>% addPolygons(
  fillColor = ~pal20(as.numeric(leisure_new)),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.8,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.8,
    bringToFront = TRUE),
  label = ~paste0(states$City_EN, ": ", formatC(leisure_new, big.mark = ",")),
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))


# redefine the legend
labels20 <- c("0","1", "2", "3", "4", "5", "6")

m20 %>% addLegend(pal = pal20, values = ~as.numeric(leisure_new), opacity = 0.8, title = "Clusters",
                  position = "bottomright", na.label = "No data", 
                  labFormat = function(type, cuts, p) {  # Here's the trick
                    paste0(labels20)
                  })



# Define UI for app that draws a histogram ----
ui <- bootstrapPage(theme = shinytheme("sandstone"), 
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
  leafletOutput("m", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10, tags$head(
    tags$style(HTML(
      "label { font-size:150%}"
    ))),
           selectInput("select", label="Select data here", 
                       choices = list("Intra-city travel strength", "Home-workplaces commute",
                                      "Dining, leisure and recreational travel"), 
                       selected = "Choice 1"),
    absolutePanel(style = "background: crimson; opacity: 0.8; color: white", 
    HTML("Sample periods: <br/> Intra-city travel strength: Jan 17, 2022—Mar 12, 2023 <br/> HW and DLR travel: May 17, 2021—June 26, 2022", sep="<br/>"),
    absolutePanel(style = "background: navy; opacity: 0.7; color: white", 
                  HTML("Notes: <br/> Clustering results are incomplete with 340 Chinese cities visualized here.", sep="<br/>")
                  ))
    #  uses textOutput to add a reactive line of text to the main panel 
))


  
    # Main panel for displaying outputs ----
    #mainPanel(
      
      # Output: Histogram ----
      #plotOutput(outputId = "distPlot")
      


server <- function(input, output, session) {
  
  # render reactive text
#  output$selected_var <- renderText({ 
#    paste("You have selected", input$radio)
  output$m <- renderLeaflet({
    if(input$select == "Intra-city travel strength"){
      labels <- c("1", "2", "3", "4", "5", "6")
      
      # here should use as.numeric(club21), otherwise the legend will be misplaced
      m %>% addLegend(pal = pal, values = ~as.numeric(club_intra_new), opacity = 0.8, title = "Clusters",
                      position = "bottomright", na.label = "No data", 
                      labFormat = function(type, cuts, p) {  # Here's the trick
                        paste0(labels)
                      })
      
    }else{
    if(input$select == "Home-workplaces commute"){
      labels19 <- c("0","1", "2", "3", "4", "5", "6", "7", "8", "9")
      
      m19 %>% addLegend(pal = pal19, values = ~as.numeric(work_new), opacity = 0.8, title = "Clusters",
                        position = "bottomright", na.label = "No data", 
                        labFormat = function(type, cuts, p) {  # Here's the trick
                          paste0(labels19)
                        })
    }else{
    if(input$select == "Dining, leisure and recreational travel"){
      labels20 <- c("0","1", "2", "3", "4", "5", "6")
      
      m20 %>% addLegend(pal = pal20, values = ~as.numeric(leisure_new), opacity = 0.8, title = "Clusters",
                        position = "bottomright", na.label = "No data", 
                        labFormat = function(type, cuts, p) {  # Here's the trick
                          paste0(labels20)
                        })
    }}}})
}


shinyApp(ui, server)
```
