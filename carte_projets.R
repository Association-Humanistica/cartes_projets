##install.packages("leaflet")
##install.packages("sp")
##install.packages("rgdal")
##install.packages("RColorBrewer")
##install.packages("leaflet.extras")
##install.packages("leaflet.minicharts")
##install.packages("htmlwidgets")
##install.packages("raster")
##install.packages("mapview")
##install.packages("leafem")

## Call the libraries
library(leaflet)
library(sp)
library(rgdal)
library(RColorBrewer)
library(leaflet.extras)
library(leaflet.minicharts)
library(htmlwidgets)
library(raster)
library(mapview)
library(leafem)
library(leafpop)
library(sf)
library(htmltools)
library(fontawesome)


## PART 1 - IN THIS PART THE CODE READS THE FILES AND ATTRIBUTES COLORS AND ICONS TO ELEMENTS

## Read the shapefile
countries <- geojsonio::geojson_read("countries/countries.geojson", what = "sp")

## Create the palette of colors for the shapefiles
pal <- colorBin("YlOrRd", domain = countries$number)

## Read the csv
projets <- read.csv("data/projets.csv")

# Filter rows for Antiquité
antiquite <- subset(projets, period_type == "Antiquité")
icon_antiquite <- makeAwesomeIcon(icon = "t", 
                                  markerColor = "orange", 
                                  library = "fa", 
                                  iconColor = "black",
                                  fontFamily = "x-small")

# Filter rows for Moyen-Âge
moyen_age <- subset(projets, period_type == "Moyen-Âge")
icon_moyen_age <- makeAwesomeIcon(icon = "t", 
                                  markerColor = "red", 
                                  library = "fa", 
                                  iconColor = "black",
                                  fontFamily = "x-small")


# Filter rows for Âge moderne
moderne <- subset(projets, period_type == "Âge moderne")
icon_moderne <- makeAwesomeIcon(icon = "t", 
                                markerColor = "green", 
                                library = "fa", 
                                iconColor = "black",
                                fontFamily = "x-small")

# Filter rows for Epoque contemporaine
contemporaine <- subset(projets, period_type == "Epoque contemporaine")
icon_contemporaine <- makeAwesomeIcon(icon = "t", 
                                      markerColor = "purple", 
                                      library = "fa", 
                                      iconColor = "black",
                                      fontFamily = "x-small")

## Create a html popup
content <- paste(sep = "<br/>",
                 paste0("<div class='leaflet-popup-scrolled' style='max-width:200px;max-height:200px'>"),
                 paste0("Titre du projet :", "<b>", projets$title, "</b>"),
                 paste0("P.I. du projet :", "<b>", projets$researcher, "</b>"),
                 paste0("<br>"),
                 paste0("Description du projet :", "<b>", projets$description, "</b>"),
                 paste0("Lien du projets :", "<b>", projets$project_url, "</b>"),
                 paste0("Lien des données :", "<b>", projets$project_url, "</b>"),
                 ##paste0(membres$url),
                 paste0("</div>"))


## PART 2 - IN THIS PART THE CODE ADDS ELEMENT ON THE MAP LIKE POLYGONS, POINTS AND IMAGES.

m <- leaflet() %>%
  ## Basemap
  ##addTiles(tile) %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  
  ## Add a zoom reset button
  addResetMapButton() %>%
  ## Add a Minimap to better navigate the map
  addMiniMap() %>%
  ## Add a coordinates reader
  leafem::addMouseCoordinates() %>%
  ## define the view
  setView(lng = -40.1113227933356, 
          lat = 28.09815143150374, 
          zoom = 2 ) %>%
  
  ## Add Polygon layer from the Geojson
  addPolygons(data = countries,
              fillColor = ~pal(countries$number),
              weight = 0.1,
              color = "brown",
              dashArray = "3",
              opacity = 0.1,
              stroke = TRUE,
              fillOpacity = 0.5,
              smoothFactor = 0.5,
              group = "Countries",
              label = ~paste(name, ": ", number, " projets", sep = ""),
              highlightOptions = highlightOptions(
                weight = 0.6,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.5,
                bringToFront = TRUE))%>%
  
  ## Add a legend with the occurrences of the toponyms according to the macro areas
  addLegend("bottomleft", 
            pal = pal, 
            values = countries$number,
            title = "projets par pays :",
            labFormat = labelFormat(suffix = " projets"),
            opacity = 0.5,
            group = "Countries") %>%
  
  ## Add Markers with clustering options
  addAwesomeMarkers(data = projets, 
                    lng = ~lng,
                    lat = ~lat, 
                    popup = c(content), 
                    group = "Projets",
                    options = popupOptions(maxWidth = 100, maxHeight = 150), 
                    clusterOptions = markerClusterOptions())%>%
  
  ## Add Heatmap of the  dataset
  addHeatmap(data = projets,
             lng = ~lng,
             lat = ~lat, 
             group = "Heatmap",
             blur = 8, 
             max = 0.5, 
             radius = 10) %>%
  
  ## Âge Moderne
  addAwesomeMarkers(data = moderne, 
                    moderne$lng, 
                    moderne$lat,
                    group = "Âge Moderne",
                    icon = icon_moderne,
                    popup = ~paste("<h4>Information on this place:</h4>",
                                   sep = " ")) %>%
  
  ## Epoque contemporaine
  addAwesomeMarkers(data = contemporaine, 
                    contemporaine$lng, 
                    contemporaine$lat,
                    group = "Epoque contemporaine",
                    icon = icon_contemporaine,
                    popup = ~paste("<h4>Information on this place:</h4>",
                                   sep = " ")) %>%
  
  ## Antiquité
  addAwesomeMarkers(data = antiquite, 
                    antiquite$lng, 
                    antiquite$lat,
                    group = "Antiquité",
                    icon = icon_antiquite,
                    popup = ~paste("<h4>Information on this place:</h4>",
                                   sep = " ")) %>%
  
  ## Moyen age
  addAwesomeMarkers(data = moyen_age, 
                    moyen_age$lng, 
                    moyen_age$lat,
                    group = "Moyen-Âge",
                    icon = icon_moyen_age,
                    popup = ~paste("<h4>Information on this place:</h4>",
                                   sep = " ")) %>%
  
  ## Add a legend with the credits
  addLegend("topright", 
            
            colors = c("trasparent"),
            labels=c("Association Francophone des Humanités Numériques (https://www.humanisti.ca/)"),
            
            title="Carte des projets de Humanistica") %>%
  
  
  ## PART 3 - IN THIS PART THE CODE MANAGE THE LAYERS' SELECTOR
  
  ## Add the layer selector which allows you to navigate the possibilities offered by this map
  
  addLayersControl(baseGroups = c("Projets",
                                  "Empty layer"),
                   
                   overlayGroups = c("Countries",
                                     "Heatmap",
                                     "Antiquité",
                                     "Moyen-Âge",
                                     "Âge Moderne",
                                     "Epoque contemporaine"),
                   
                   options = layersControlOptions(collapsed = TRUE)) %>%
  
  ## Hide the layers that the users can choose as they like
  hideGroup(c("Empty",
              "Heatmap",
              "Antiquité",
              "Moyen-Âge",
              "Âge Moderne",
              "Epoque contemporaine"))

## Show the map  
m

