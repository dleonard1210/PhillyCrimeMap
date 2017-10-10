#
# This is the server logic of a Shiny web application that provides interactive visual exploration of
# the Philadelphia Crime statistics database
#

library(shiny)
library(ggplot2)
library(leaflet)
library(lubridate)
library(dplyr)


# Define server logic 
shinyServer(function(input, output) {
    
    # Get a list of the different types of crime
    # crimelist <- read.csv(url("https://phl.carto.com/api/v2/sql?format=csv&q=SELECT distinct text_general_code 
    #                                                                          FROM incidents_part1_part2
    #                                                                          order by text_general_code"), 
    #                       stringsAsFactors = FALSE)$text_general_code
    
    parms <- reactive({
        # Create input list for testing - commented out for production
        # input <- list(start = "2017-01-01", end = Sys.Date(), CrimeType = "Homicide - Criminal")

        startDate <- input$dateRange[1]
        endDate <- input$dateRange[2]
        CrimeType <- input$CrimeType
        query <- paste0("SELECT dispatch_date,
                                point_x,  point_y,
                                text_general_code
                        FROM incidents_part1_part2
                        where text_general_code = '",CrimeType,"' and
                        dispatch_date between '",as.character(startDate),"' and '",as.character(endDate),"'")
        crimedata <- read.csv(url(paste0("https://phl.carto.com/api/v2/sql?format=csv&q=",query)),
                              stringsAsFactors = FALSE)
        crimedata$dispatch_date <- as.Date(crimedata$dispatch_date)
        crimedata$year <- year(crimedata$dispatch_date)
        crimedata$month <- month(crimedata$dispatch_date)
        crimedata$day <- day(crimedata$dispatch_date)
        crimedata$lat <- crimedata$point_y
        crimedata$lng <- crimedata$point_x
        crimedata$text_general_code <- gsub(" $", "", crimedata$text_general_code, perl=TRUE) # Remove trailing blanks
        #str(crimedata)
        homicides <- crimedata %>% filter(text_general_code == "Homicide - Criminal")
        homicidesum <- homicides %>% filter(year < 2017) %>% count(year) %>% arrange(year)
        
        list(crimedata = crimedata,
             startDate = startDate,
             endDate = endDate,
             crimecount = length(crimedata$dispatch_date),
             CrimeType = CrimeType,
             LastUpdate = max(crimedata$dispatch_date))
        
    })
    
  output$leafletMap <- renderLeaflet({
          if(parms()$crimecount > 0)
              parms()$crimedata %>% 
              leaflet() %>%
              addTiles() %>%
              addCircleMarkers(radius = 6,
                               color = "black",
                               fillOpacity = 1,
                               popup = ~as.character(dispatch_date),
                               lng = ~lng,
                               lat = ~lat,
                               clusterOptions = markerClusterOptions()
                               )
    
  })
  output$MapTitle <- renderUI({
      headingString <- paste0("<b>Cluster Map for Crime Type '",parms()$CrimeType,"' for the period from ",
                              as.character(parms()$startDate)," to ",as.character(parms()$LastUpdate),"</b>")
      if(parms()$crimecount > 0) 
          heading <- HTML(paste0('<br><p style="text-align:center">',headingString,'</p>'))
      else 
          heading <- HTML('<br><br><p style="text-align:center"><b>Sorry - No Data Available For Selected Type and Time Period.</b></p>')
      heading
  })
  # output$CostPlots <- renderPlot({
  #     #str(parms())
  #       })
  # 
  # output$help <- renderUI({
  #     helptext <- HTML('<h1>Inventory Allocation Optimizer</h1>
  #                       <h3>This tool will tell you the most profitable level of inventory to allocate to a store. "Most profitable" is
  #                           defined as the level that minimizes the sum of two costs: lost margin due to stockouts, and obsolescence
  #                           cost at the end of the season due to unsold units.<br><br>
  #                           Use the sliders to adjust various parameters used to determine the optimal answer:</h3>
  #                       <ul style="font-size:20px">
  #                          <li>Retail Margin %: What portion of the retail price of the item represents gross profit?</li>
  #                          <li>Average Daily Demand: the average number of units sold per day at this location</li>
  #                          <li>Variability of Demand: the degree to which demand varies from day-to-day (as a % of demand)</li>
  #                          <li>Time Remaining in Season: the number of weeks left before this item will be taken off the shelf</li>
  #                          <li>Shipping Frequency: the number of days between arrival of shipments to this location</li>
  #                          <li>Shipping Time: number of days it takes for ordered goods to arrive at this location from the distribution center</li>
  #                       </ul>
  #                       <h3>Assumptions:</h3>
  #                       <ul style="font-size:20px">
  #                          <li>Average demand rate is constant over the remainder of the season</li>
  #                          <li>At the end of the season, any remaining units are discarded, so the obsolescence cost is 100% of the unit cost</li>
  #                       </ul>
  #                       <h3>Notes:</h3>
  #                       <ul style="font-size:20px">
  #                          <li>There is an interesting interplay between margin % and variability of demand.
  #                              If you set a very high margin %, then as you increase variability you will see 
  #                              the recommended stocking level increase. On the other hand, if you set the margin % low,
  #                              the recommended stocking level will go down as variability goes up. Why do you think that is?</li>
  #                          <li>The recommended stocking level is not an "order" - what you would order is the difference
  #                              between the recommended level and the number of units currently held at the store.</li>
  #                       </ul>
  #                      
  #                     ')
  # })
})
