# library(devtools)
# install_github("r-spatial/sf")
# devtools::install_github("r-spatial/mapview@develop")
# devtools::install_github("bhaskarvk/leaflet.extras")
# devtools::install_github("r-spatial/mapedit")
library(dplyr)
library(stringr)
library(sf)
library(leaflet)
library(ggplot2)

locnCoord <-
  read.csv('Locations.csv') %>%
  filter(str_detect(location, 'project25')) %>%
  mutate(location = str_replace(location, "digiscape_gbr.digiscapegbrproject25","")) %>%
  mutate(locationPopUpLabel = case_when(
    location == 'dixonroad' ~ 'Dixon Rd',
    location == 'behanacreek' ~ 'Behana Ck',
    location == 'miriwinni' ~ 'Mirriwinni',
    TRUE ~ '???'
  )) %>%
  mutate(depth = runif(3))

locnSF <- st_as_sf(locnCoord, coords = c('long','lat'), crs="+proj=longlat +datum=WGS84 +no_defs") #WGS84: EPSG=4326; proj4string: +proj=longlat +datum=WGS84 +no_defs

# m <- mapview()
#
# # inspect data on base map
# mapview(locnSF)
#
# selected = selectFeatures(locnSF, mode = "click")
#
# # check the selection (selected will be diplayed in blue)
# mapview(locnSF, col.regions = "red") + selected
#
# summary(selected)
# mean(selected$depth)
# sd(selected$depth)
#
# #### ----
#
# selected <- selectFeatures(locnSF)
#
# mapview(selected)
#
# #### ----
#
# selectFeatures(locnSF) %>%
#   st_union() %>%
#   mapview()
#
# selectFeatures(locnSF) %>%
#   ggplot(aes(x=location, y=depth))+
#   geom_col()
#
# #### ----
#
# leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data = locnSF) %>%
#   selectFeatures(map=.)
#
# #### ----
#
# leaflet() %>%
#   addTiles() %>%
#   addFeatures(data = locnSF) %>%
#   selectFeatures(locnSF, map=.) %>%
#   print()
#
# leaflet() %>%
#   addTiles() %>%
#   addFeatures(data = locnSF) %>%
#   selectFeatures(locnSF, map=.) %>%
#   ggplot(aes(x=location, y=depth))+
#   geom_col()
#
#
# selected <-
#   leaflet() %>%
#   addTiles() %>%
#   addFeatures(data = locnSF) %>%
#   selectFeatures(locnSF, map=.)
#
# summary(selected$depth)



#### ----


library(mapedit)
library(mapview)
library(shiny)

ui <- fluidPage(
  fluidRow(
    # edit module ui
    column(6,
           selectModUI("selectmap")
           ),
    column(
      6,
      h3("Point of Depth"),
      plotOutput("selectstat")
    )
  )
)
server <- function(input, output, session) {

  g_sel <- callModule(selectMod,
                      "selectmap",
                      leaflet() %>%
                        addTiles() %>%
                        addFeatures(data = locnSF, layerId = ~location)
                      )

  rv <- reactiveValues(selected=NULL)

  observe({
    gs <- g_sel()

    if(length(gs$id) > 0) {
      rv$selected <- locnSF %>% filter(location %in% gs$id)
    } else {
      rv$selected <- NULL
    }
  })

  output$selectstat <- renderPlot({
    ggplot()
    # selected <- locnSF %>% filter(location %in% g_sel()$id)
    if(!is.null(rv$selected) && nrow(rv$selected) > 0) {
      ggplot(data=rv$selected, aes(location, depth))+
        geom_point(color='red')
    } else {
      ggplot()
    }
  })
}
shinyApp(ui, server)
