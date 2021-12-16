#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(tidyverse)
library(ggmap)
library(leaflet)
library(stringr)
#library(shinydashboard)
library(shinyTree)
library(dplyr)


#Abandoned Properties data - Brianna 
prop <- st_read("Abandoned_Property_Parcels.shp")
prop <- prop %>% select(Structures, Outcome_St, Zip_Code, Council_Di)
#taking out demolished and deconstructured 
prop <- prop %>% filter(Outcome_St != 'Demolished' & Outcome_St != 'Deconstructed')
prop <- prop %>% filter(Structures != 'NA') #removing null values from property structure type

prop_center <- st_centroid(prop)
prop_spatial <- prop_center %>%
    st_as_sf(coords  = c("Lon","Lat"))
prop_spatial$popup <- paste("Property Type: ",prop_spatial$Structures,"</b><br>",
                            "Property Status: ",prop_spatial$Outcome_St,"<br>", 
                            "District: ", prop_spatial$Council_Di
)

#Census Data - Fernando & John
census <- st_read("2010_CensusData.shp", stringsAsFactors = FALSE)

#adding in Fernando's Population Density
census <- census %>% select(., TRACTCE:Geo_QName, SE_T002_02,SE_T068_00:SE_T069_02, SE_T054_00:SE_T054_07) %>%
    rename(., Pop_Den = SE_T002_02, Units = SE_T068_00, Occ_Units = SE_T069_00, Own_Occ_Units = SE_T069_01, Ren_Occ_Units = SE_T069_02) %>%
    mutate(., Pct_Occ = Occ_Units/Units, Pct_Own_Occ = Own_Occ_Units/Occ_Units, Pct_Ren_Occ = Ren_Occ_Units/Occ_Units) %>%
    mutate(., White = SE_T054_01, Non_White = SE_T054_02 + SE_T054_03 + SE_T054_04 + SE_T054_05 + SE_T054_06 + SE_T054_07) %>%
    rename(., Total_Pop = SE_T054_00) %>%
    mutate(., Pct_White = White/Total_Pop, Pct_Non_White = Non_White/Total_Pop)
census_center <- st_centroid(census)
census_spatial <- census_center %>% st_as_sf(coords  = c("Lon","Lat"))
ggplot() + geom_sf(data = census)
pal <- colorNumeric("viridis", domain = NULL)

#adding popup to use in conjunction with properties data 
#census_spatial$popup <- paste("Total Population in Region: ", census$Total_Pop)


#Public Facilities Data - Brenna
pf <- read_csv("Public_Facilities.csv")

# create a palette for the leaflet
pal2 <- colorFactor(palette = c("orange", "green", "blue"), domain = pf$POPL_TYPE)
pf.spatial <- pf %>%
    st_as_sf(coords = c("Lon", "Lat")) %>%
    st_set_crs(value = 4326)
pf.spatial$popup <- paste("<b>", pf.spatial$POPL_TYPE, "</b><br>",
                          "Name: ", pf.spatial$POPL_NAME, "<br>",
                          "Address: ", pf.spatial$POPL_ADDR1) 

# icon set up - Brenna
logos <- awesomeIconList(
    'FIRE STATION' = makeAwesomeIcon(
        icon = 'fire',
        iconColor = 'black',
        markerColor = 'orange',
        library = 'fa'
    ),
    'LIBRARY' = makeAwesomeIcon(
        icon = 'book',
        iconColor = 'black',
        markerColor = 'green',
        library = 'fa'
    ),
    
    'POLICE STATION' = makeAwesomeIcon(
        icon = 'shield',
        iconColor = 'black',
        markerColor = 'blue',
        library = 'fa'
    )
)


###################
#### App Code #####
###################


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Data Visualization - Final Project "),
    h4('Select Filters To Learn More About Your South Bend Community'),
    
    # Application title
    #titlePanel("Select Filters Below To Learn More About Your South Bend Community'"),
    
    # Sidebar with select input type of occupancy and facility type - Fernando & John
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "variable",
                        label = "Percentage:",
                        choices = c("Population density" = "Pop_Den",
                                    "Occupied housing" = "Pct_Occ",
                                    "Owner occupied housing" = "Pct_Own_Occ",
                                    "White population" = "Pct_White",
                                    "Non White population" = "Pct_Non_White")),
            radioButtons(inputId = "facility",
                         label = "Type of facility",
                         choices = c("Fire Station" = "FIRE STATION",
                                     "Library" = "LIBRARY",
                                     "Police Station" = "POLICE STATION"),
                         selected = "FIRE STATION"),
            
            #Properties of interest - Brianna 
            radioButtons(inputId = "property",
                         label = "Status",
                         choices = c("Repaired",
                                     "Repaired & Occupied",
                                     "Occupied & Not Repaired")),
            
        ), #sidebarPanel
        
        #added main panel with main tab - Brianna
        mainPanel( 
            tabsetPanel(type = "tabs", 
                        tabPanel("South Bend Map", leafletOutput("map2", height = 520)))
        )#mainpanel
        
        
    ),
    
)#fluid page


#########################
## Server 
#########################

#testing checkbox
server <- shinyServer(function(input, output, session) {  
    
    
    #listening for user input 
    prop_condition_filter <- reactive({
        
        #in names shows us what the user selected from our tree 
        if('Repaired & Occupied' %in% names(
            as.data.frame(
                get_selected(
                    input$tree, format = "slices")))){
            
            prop_condition_filter = 'Repaired & Occupied'
            
        } #if 
        
        
        else if('Occupied & Not Repaired' %in% names(
            as.data.frame(
                get_selected(
                    input$tree, format = "slices")))){
            
            prop_condition_filter = 'Occupied & Not Repaired'
            
        } #if
        
        else {
            prop_condition_filter = 'Repaired'
        }
        
        prop_condition_filter
        
    }) #reactive 
    
    
    #changing data based on user input heard
    filtered <- reactive({
        
        prop_condition_filter1 = ""
        prop_condition_filter2 = ""
        prop_condition_filter3 = ""
        
        if('Repaired...Occupied' %in% names(
            as.data.frame(
                get_selected(
                    input$tree, format = "slices")))){
            
            prop_condition_filter1 = 'Repaired & Occupied'
            
        } #if 
        
        if('Occupied...Not.Repaired' %in% names(
            as.data.frame(
                get_selected(
                    input$tree, format = "slices")))){
            
            prop_condition_filter2 = 'Occupied & Not Repaired'
            
        } #if
        
        if (names(
            as.data.frame(
                get_selected(
                    input$tree, format = "slices"))) == 'Repaired') {
            prop_condition_filter3 = 'Repaired'
        }
        
        rows <- (prop$Outcome_St == prop_condition_filter1 | prop$Outcome_St == prop_condition_filter2 | prop$Outcome_St == prop_condition_filter3)
        
        #getting only the data we need atm
        prop[rows,,drop = FALSE] #we can change this to our master data or sumn
        
    }) #filtered   
    
    
    #####################################
    ## Formatting Output Tables / Maps ##
    #####################################
    
    
    #this shows us what box the user checked (or boxes) 
    # output$Table <- renderPrint({
    #     
    #     names(as.data.frame(get_selected(input$tree, format = "slices")))
    #     
    # })#render Print
    
    
    #testing how to get grandfather node 
    # get_checked<- function(tree, format=c("names", "slices", "classid")){
    #     format <- match.arg(format, c("names", "slices", "classid"), FALSE)
    #     switch(format,
    #            "names"=get_checked_names(tree),
    #            "slices"=get_checked_slices(tree),
    #            "classid"=get_checked_classid(tree)
    #     )  
    # }
    
    # namesT <- reactive({
    #     get_checked_names(input$tree)
    # })
    # 
    # output$Table3 <- renderPrint({
    #     
    #     namesT()
    #     
    # })#render Print
    
    #THIS PIECE ENABLES US TO SEE HOW REACTIVE IS WORKING WITH ABANDONED BUILDINGS DATA 
    #trying to show all new data 
    # observe({
    # output$Table2 <- renderDataTable({ #was renderTable or renderPrint
    #    filtered()
    # }) #render table - showing us what value is shown 
    # })#observe
    
    
    #shows map below it 
    # output$map <- renderLeaflet({
    #     #get_selected(input$tree, format = "slices") #get selected
    # 
    #     #not sure how to connect these 2
    #     leaflet(filtered()) %>% #was prop
    #                 addTiles() %>%
    #                 addPolygons()
    # 
    # }) #leafLet
    
    #commented out 5:50 p.m.
    # output$map2 <- renderLeaflet({
    #     leaflet(pf.spatial) %>% 
    #         addTiles() %>% 
    #         addAwesomeMarkers(icon = ~logos[POPL_TYPE], 
    #                           label = ~POPL_ADDR1,
    #                           popup = ~popup) %>%
    #         addPolygons(data = housing, color = "#444444", weight = 1, smoothFactor = 0.5, fillColor = ~pal(Pct_Occ), fillOpacity = 0.5,
    #                     label = ~paste0(round(Pct_Own_Occ, 2)*100, '%'), 
    #                     highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE))
    # })
    # 
    
    
    #bringing property markers to front where possible 
    marker_options <- markerOptions( 
        zIndexOffset = as.integer(as.factor(prop_spatial$Outcome_St)) * 1000 
    )
    
    #Fernando's map 2
    output$map2 <- renderLeaflet({
        census$active <- census %>% pull(input$variable)
        pal <- colorNumeric("viridis", domain = census$active)
        leaflet() %>% 
            addTiles() %>% 
            addPolygons(data = census,
                        color = "#444444", weight = 1, smoothFactor = 0.5, fillColor = ~pal(census$active), fillOpacity = 0.5,
                        #adding in pop density - ppl per square mile (Fernando addition)
                        label = ~ifelse(census$active == "Pop_Den", yes = census$active, no = round(census$active, 2)), 
                        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% 
            #adding in Brenna's cool marker icon thingy
            addAwesomeMarkers(data = pf.spatial %>% filter(., POPL_TYPE == input$facility), 
                              icon = ~logos[input$facility], 
                              label = ~POPL_NAME,
                              popup = ~popup) %>% 
            #adding popup with Structure info - Brianna's add
            addCircleMarkers(data = prop_spatial %>% filter(., Outcome_St == input$property), stroke = 0, fillOpacity = 1, radius = 8, popup=~popup,
                             options = marker_options, color = "red") #
        
        #testing colors rn , color = ~pal(Outcome_St))
        #color = ~pal(Outcome_St)
    }) #renderLeaflet
    
    
    
}) #shinyServer 


# Run the application 
shinyApp(ui = ui, server = server)
