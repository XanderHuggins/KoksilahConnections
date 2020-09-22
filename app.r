# load necessary packages
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(raster)) install.packages("raster", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(wesanderson)) install.packages("wesanderson", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(googlesheets4)) install.packages("googlesheets4", repos = "http://cran.us.r-project.org")
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")

## Comment out line below before publishing to shinyapps
setwd("C:/Users/xande/Desktop/Scripts/KoksilahConnections")

# Import stream network, watershed mask, aquifers, and well location shapefiles
Streams <- sf::read_sf("./StreamNetwork_4326.shp")
Streams.plot <- st_zm(Streams, drop = TRUE, what = "ZM") # need to drop zm dimensions to work with shiny apps

WshedMask <- sf::read_sf("./WatershedMask_4326.shp")
Aquifers <- sf::read_sf("./Aquifers.shp")

# Sort aquifers by type and size
Aquifers_bdrck <- Aquifers %>% dplyr::filter(SUBTYPE == 'Fractured bedrock') %>% arrange(desc(SHAPE_Area))
Aquifers_sg_unc <- Aquifers %>% dplyr::filter(SUBTYPE == 'Unconfined sand & gravel') %>% arrange(desc(SHAPE_Area))
Aquifers_sg_c <- Aquifers %>% dplyr::filter(SUBTYPE == 'Confined sand & gravel') %>% arrange(desc(SHAPE_Area))

Wells <- sf::read_sf("./Wells_4326.shp")
Wells <- st_zm(Wells, drop = TRUE, what = "ZM") # need to drop zm dimensions to work with shiny apps

###########################################
## Section below imports Stream Tracker  ##
## observations, converts to shapefile   ##
###########################################

# Import data from google sheets (imported from Stream Tracker, with added observations manually moved from comment section to unique field)
# necessary to comment-out lines 41-55 before publishing to shinyapps.io

PointData <- read_sheet('https://docs.google.com/spreadsheets/d/1AIu49hWSto0mHcvA9h77l1H0yOmkuzlgDhhOlhoZYfo/edit?usp=sharing')
PointData.small <- PointData[,c(1,3,4,16,24,25)]
# 
PointData.small$LON %<>% as.numeric()
PointData.small$LAT %<>% as.numeric()
# 
PointData.small$StreamPres %<>% as.factor()
PointData.small$FlowRate %<>% as.numeric()
PointData.small$Temp %<>% as.numeric()
PointData.small$id %<>% as.character()

PointData.shp <- st_as_sf(PointData.small, coords = c("LON", "LAT"), crs = 4326)

sf::write_sf(PointData.shp, here::here(), "Observations_updated",
             driver = "ESRI Shapefile", overwrite = T)

PointData.shp <- sf::read_sf("./Observations_updated.shp")

###########################################
## Section below builds the shinpy app   ##
###########################################

# Create custom legend function for variable stream widths to display
addLegendCustom <- function(map, position, title, colors, labels, sizes, group.in, opacity = 1){
    colorAdd <- paste0(colors, "; width: 12 px; height:", sizes, "px; margin-top: 6px;")
    labelAdd <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 0px;line-height: ", sizes, "px;'>", labels, "</div>")
    
    return(addLegend(map, position = position, title = title,
                     colors = colorAdd,
                     labels = labelAdd,
                     opacity = opacity,
                     group = group.in))
}

# Create blue palette for streams based on order
pal_blues <- colorFactor(
    palette = "Blues",
    domain = Streams$STREAM_ORD
)

# create opposite of 'in' function
'%!in%' <- function(x,y)!('%in%'(x,y))

# Create user interface
ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("mymap", width = "100%", height = "100%"),
    absolutePanel(top = 2, right = 2,
                  draggable = FALSE,
                  class = "panel panel-header",
                  style = "opacity: 1.00;",
                  selectInput("ObsType", "What observation data do you want to see?",
                              c("Observation locations",
                                "Flow Presence",
                                "Conductivity",
                                "Temperature")),
                  selectInput("AqTyp", "Aquifer types to show:",
                              c("None" = "NO",
                                "Fractured bedrock" = "BDR",
                                "Unconfined sand and gravel" = "UNC",
                                "Confined sand and gravel" = "CON")),
                  selectInput("WellYN", "Show wells?",
                              c("No" = "NO",
                                "Yes" = "YES"))
                  
    )
)

# Create server
server <- function(input, output) {
    # Create initial map
    output$mymap <- renderLeaflet({
        leaflet(PointData.shp) %>%
            addMapPane("Basemap", zIndex = 410) %>%
            addMapPane("Aquifers", zIndex = 420) %>%
            addMapPane("Streams", zIndex = 430) %>%
            addMapPane("Wells", zIndex = 440) %>%
            addMapPane("Observations", zIndex = 450) %>%
            addTiles(group = "OSM",
                     options = pathOptions(pane = "Basemap")) %>%
            setView(-123.7, 48.73, zoom = 12) %>%
            addPolygons(data = WshedMask,
                        col = 'black',
                        stroke = FALSE,
                        fillOpacity = 0.5,
                        smoothFactor = 0.5,
                        options = pathOptions(pane = "Basemap")) %>%
            addPolylines(data=Streams.plot,
                         layerId = Streams$OBJECTID_1,
                         color = "blue", #,~pal_blues(Streams$STREAM_ORD),
                         label = paste("Stream Order: ", Streams$STREAM_ORD, sep = ""),
                         weight = (Streams$STREAM_ORD)/1.5,
                         opacity = 1,
                         options = pathOptions(pane = "Streams"))
    })
    
    ##################### observe functions for the WELL DISPLAY OPTION
    observe({
        if ('YES' %in% input$WellYN) {
            leafletProxy("mymap") %>%
                addCircles(data = Wells,
                           layerId = Wells$OBJECTID_1,
                           fillColor = "black",
                           radius = 2,
                           fillOpacity = 0.8,
                           stroke = FALSE,
                           group = "wellgroup",
                           options = pathOptions(pane = "Wells"))
        }
    })
    observe({
        if ('NO' %in% input$WellYN) {
            leafletProxy("mymap") %>%
                clearGroup(group = "wellgroup")
        }
    })
    
    ##################### observe functions for AQUIFER SELECTIONS
    ### bedrock aquifers
    observe({
        if ('NO' %in% input$AqTyp) {
            leafletProxy("mymap")
        }
    })
    
    observe({
        if ('BDR' %in% input$AqTyp) {
            leafletProxy("mymap") %>%
                addPolygons(data = Aquifers_bdrck,
                            layerId = Aquifers_bdrck$AQUIFER_ID,
                            label = paste0(Aquifers_bdrck$NAME, " Aquifer", sep = ""),
                            fill = TRUE,
                            fillColor = wesanderson::wes_palette("Darjeeling1",
                                                                 n = length(unique(Aquifers_bdrck$AQUIFER_ID)),
                                                                 type = "discrete"),
                            stroke = TRUE,
                            weight = 1,
                            color = "black",
                            fillOpacity = 0.6,
                            highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
                            group = "bedrock",
                            options = pathOptions(pane = "Aquifers"))
        }
    }
    )
    
    observe({
        if ('BDR' %!in% input$AqTyp) {
            leafletProxy("mymap") %>%
                clearGroup("bedrock")
        }
    }
    )
    
    ### unconfined aquifers
    observe({
        if ('UNC' %in% input$AqTyp) {
            leafletProxy("mymap") %>%
                addPolygons(data = Aquifers_sg_unc,
                            layerId = Aquifers_sg_unc$AQUIFER_ID,
                            label = paste0(Aquifers_sg_unc$NAME, " Aquifer", sep = ""),
                            fill = TRUE,
                            fillColor = wesanderson::wes_palette("Darjeeling1",
                                                                 n = length(unique(Aquifers_sg_unc$AQUIFER_ID)),
                                                                 type = "discrete"),
                            stroke = TRUE,
                            weight = 1,
                            color = "black",
                            fillOpacity = 0.6,
                            highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
                            group = "unconfined",
                            options = pathOptions(pane = "Aquifers"))
        }
    }
    )
    
    observe({
        if ('UNC' %!in% input$AqTyp) {
            leafletProxy("mymap") %>%
                clearGroup("unconfined")
        }
    }
    )
    
    ### confined aquifers
    observe({
        if ('CON' %in% input$AqTyp) {
            leafletProxy("mymap") %>%
                addPolygons(data = Aquifers_sg_c,
                            layerId = Aquifers_sg_c$AQUIFER_ID,
                            label = paste0(Aquifers_sg_c$NAME, " Aquifer", sep = ""),
                            fill = TRUE,
                            fillColor = wesanderson::wes_palette("Darjeeling1",
                                                                 n = length(unique(Aquifers_sg_c$AQUIFER_ID)),
                                                                 type = "discrete"),
                            stroke = TRUE,
                            weight = 1,
                            color = "black",
                            fillOpacity = 0.6,
                            highlightOptions = highlightOptions(color = "black", weight = 5, bringToFront = TRUE),
                            group = "confined",
                            options = pathOptions(pane = "Aquifers"))
        }
    }
    )
    
    observe({
        if ('CON' %!in% input$AqTyp) {
            leafletProxy("mymap") %>%
                clearGroup("confined")
        }
    }
    )
    
    
    ##################### observe functions for STREAM OBSERVATIONS
    observe({
        if (input$ObsType == 'Flow Presence') {
            pal_ygb <- colorFactor(
                palette = wes_palette("Zissou1", 5)[c(1, 5, 3)],
                domain = PointData.shp$StreamPres
            )
            
            leafletProxy("mymap") %>%
                clearMarkers() %>% clearControls() %>%
                addCircleMarkers(data = PointData.shp,
                                 layerId = PointData.shp$id,
                                 # label = lapply(labs.SP, htmltools::HTML),
                                 radius = 8,
                                 fillColor = ~pal_ygb(StreamPres),
                                 stroke = TRUE,
                                 color = "black",
                                 fillOpacity = 1,
                                 group = "SG",
                                 options = pathOptions(pane = "Observations")) %>%
                addLegendCustom(position = "bottomright",
                                title = "Stream Order",
                                colors = c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
                                labels = c(seq(1, 6, 1)),
                                sizes = c(seq(1, 6, 1)),
                                group = "SG") %>%
                addLegend("bottomright",
                          pal = pal_ygb,
                          values = PointData.shp$StreamPres,
                          title = "Flow presence?",
                          labFormat = labelFormat(suffix = ""),
                          opacity = 1,
                          group = "SG")
        }})
    
    observe({
        if (input$ObsType == 'Conductivity') {
            pal_ygb <- colorNumeric(
                palette = "YlGnBu",
                domain = PointData.shp$FlowRate
            )
            
            leafletProxy("mymap") %>%
                clearMarkers() %>% clearControls() %>%
                addCircleMarkers(data = PointData.shp,
                                 layerId = PointData.shp$id,
                                 # label = lapply(labs.FR, htmltools::HTML),
                                 radius = 8,
                                 fillColor = ~pal_ygb(FlowRate),
                                 stroke = TRUE,
                                 color = "black",
                                 fillOpacity = 1,
                                 group = "SG",
                                 options = pathOptions(pane = "Observations")) %>%
                addLegendCustom(position = "bottomright",
                                title = "Stream Order",
                                colors = c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
                                labels = c(seq(1, 6, 1)),
                                sizes = c(seq(1, 6, 1)),
                                group = "SG") %>%
                addLegend("bottomright",
                          pal = pal_ygb,
                          values = PointData.shp$FlowRate,
                          title = "Conductivity",
                          labFormat = labelFormat(suffix = "µS/cm"),
                          opacity = 1,
                          group = "SG")
        }})
    
    observe({
        if (input$ObsType == 'Temperature') {
            pal_ygb <- colorNumeric(
                palette = "PuRd",
                domain = PointData.shp$Temp
            )
            
            leafletProxy("mymap") %>%
                clearMarkers() %>% clearControls() %>%
                addCircleMarkers(data = PointData.shp,
                                 layerId = PointData.shp$id,
                                 # label = lapply(labs.T, htmltools::HTML),
                                 radius = 8,
                                 fillColor = ~pal_ygb(Temp),
                                 stroke = TRUE,
                                 color = "black",
                                 fillOpacity = 1,
                                 group = "SG",
                                 options = pathOptions(pane = "Observations")) %>%
                addLegendCustom(position = "bottomright",
                                title = "Stream Order",
                                colors = c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
                                labels = c(seq(1, 6, 1)),
                                sizes = c(seq(1, 6, 1)),
                                group = "SG") %>%
                addLegend("bottomright",
                          pal = pal_ygb,
                          values = PointData.shp$Temp,
                          title = "Temperature",
                          labFormat = labelFormat(suffix = "*C"),
                          opacity = 1,
                          group = "SG")
        }})
    
    observe({
        if (input$ObsType == 'Observation locations') {
            leafletProxy("mymap")%>%
                clearMarkers() %>% clearControls() %>%
                addCircleMarkers(data = PointData.shp,
                                 layerId = PointData.shp$id,
                                 # label = paste("OBS ID: ", PointData.shp$ID, sep = ""),
                                 radius = 8,
                                 fillColor = "#2eb82e",
                                 stroke = TRUE,
                                 color = "black",
                                 fillOpacity = 1,
                                 group = "SG",
                                 options = pathOptions(pane = "Observations")) %>%
                addLegendCustom(position = "bottomright",
                                title = "Stream Order",
                                colors = c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#084594"),
                                labels = c(seq(1, 6, 1)),
                                sizes = c(seq(1, 6, 1)),
                                group = "SG") %>%
                addLegend("bottomright", colors = "#2eb82e", labels = "Observations",
                          opacity = 1,
                          group = "SG")
        }})
}

# Run app
shinyApp(ui, server)