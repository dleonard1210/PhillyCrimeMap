#
# This is the server logic of a Shiny web application that provides interactive visual exploration of
# the Philadelphia Crime statistics database
#

library(shiny)
library(shinyjs)
library(ggplot2)
library(leaflet)
library(lubridate)
library(dplyr)
library(rgdal)
library(sp)
library(geojsonio)


# Define server logic 
shinyServer(function(input, output, session) {
    
    # Get a list of the different types of crime
    urlstring <- paste0("http://phl.carto.com/api/v2/sql?format=csv&q=",
                        "SELECT distinct rtrim(text_general_code) as text_general_code ",
                        "FROM incidents_part1_part2 order by text_general_code")
    urlstring <- URLencode(urlstring)
    crimelist <- read.csv(url(urlstring),
                          stringsAsFactors = FALSE)$text_general_code

    updateSelectInput(session,
                      "CrimeType",
                      choices = crimelist[nchar(crimelist)>0],
                      selected = "Homicide - Criminal")
                      
    parms <- reactive({
        # Create input list for testing - commented out for production
        # input <- list(start = "2017-01-01", end = Sys.Date(), CrimeType = "Homicide - Criminal")
        
        displayMonths <- c("January","February","March","April","May","June","July","August","September","October","November","December")

        CrimeType <- input$CrimeType
        urlstring <- paste0("http://phl.carto.com/api/v2/sql?format=csv&q=",
                        "SELECT dispatch_date, dispatch_date_time, location_block, dc_dist, point_x, point_y, text_general_code ",
                        "FROM incidents_part1_part2",
                        " where text_general_code like '")
        urlstring <- paste0(URLencode(urlstring),gsub(" ","_",CrimeType),"'")
        crimedata <- read.csv(url(urlstring),
                              stringsAsFactors = FALSE)
        crimedata$dispatch_date <- as.Date(crimedata$dispatch_date)
        crimedata$year <- year(crimedata$dispatch_date)
        crimedata$month <- month(crimedata$dispatch_date)
        crimedata$dispmonth <- displayMonths[crimedata$month]
        crimedata$day <- day(crimedata$dispatch_date)
        crimedata$lat <- crimedata$point_y
        crimedata$lng <- crimedata$point_x
        crimedata$searchterms <- gsub(" ","+",paste("Philadelphia",
                                                    CrimeType,
                                                    paste0("'Police%20District%20",crimedata$dc_dist,"'"),
                                                    crimedata$dispmonth,
                                                    crimedata$day,
                                                    crimedata$year,
                                                    crimedata$dispatch_date,
                                                    crimedata$location_block)
                                      )

        list(crimedata = crimedata,
             crimecount = length(crimedata$dispatch_date),
             CrimeType = CrimeType,
             FirstDispatch = min(crimedata$dispatch_date),
             LastDispatch = max(crimedata$dispatch_date))
        
    })
    
    dates <- reactive({
        startDate <- switch(input$radio,
                            month = Sys.Date()-31,
                            sixmonths = Sys.Date()-182,
                            year = Sys.Date()-365,
                            custom = min(input$dateRange[1],input$dateRange[2])
        )
        endDate <- switch(input$radio,
                          custom = max(input$dateRange[1],input$dateRange[2]),
                          month = Sys.Date(),
                          sixmonths = Sys.Date(),
                          year = Sys.Date()
        )
        if (input$radio == "custom") enable("dateRange") else disable("dateRange")
            
        list(startDate = startDate, endDate = endDate)
    })
    
  output$leafletMap <- renderLeaflet({
      startDate <- dates()$startDate
      endDate <- dates()$endDate
      
      crimedata <- parms()$crimedata %>%
          filter(dispatch_date >= startDate, dispatch_date <= endDate)
      
      crimecount <- length(crimedata$dispatch_date)
      
      gjfile <- "phillyhoods.geojson"

      phillyhoods <- geojson_read(gjfile, what = "sp")
      
      if( crimecount > 0)
          crimedata %>% 
          filter(dispatch_date >= startDate, dispatch_date <= endDate) %>%
          leaflet() %>%
          addTiles() %>%
          addPolygons(fillOpacity = 0, label = ~mapname, weight = 2, data = phillyhoods) %>%
          addCircleMarkers(radius = 6,
                           color = "black",
                           fillOpacity = 1,
                           popup = ~paste0(as.character(dispatch_date_time),
                                           '; ',
                                           location_block,
                                           '; Police District ',
                                           dc_dist, '; ',
                                           ' <a href = "https://www.google.com/search?q=',
                                           searchterms,
                                           '" target="_blank"> Search for More Information </a>'),
                           label = ~as.character(dispatch_date_time),
                           lng = ~lng,
                           lat = ~lat,
                           clusterOptions = markerClusterOptions()
                           )

  })
  output$NumberOfObservations <- renderUI({
      startDate <- dates()$startDate
      endDate <- dates()$endDate
      startend <- parms()$crimedata %>%
          filter(dispatch_date >= startDate, dispatch_date <= endDate) %>%
          summarize(firstdispatch = min(dispatch_date), lastdispatch = max(dispatch_date), incidents = n())
      
      headingString <- paste0("There were <b>",
                              formatC(startend$incidents[1], big.mark=","),
                              "</b> incidents of <b>",parms()$CrimeType,"</b> in Philadelphia during the period from ",
                              as.character(startend$firstdispatch[1])," to ",as.character(startend$lastdispatch[1]))
      if(startend$incidents[1] > 0) 
          heading <- HTML(paste0('<p style="text-align:left">',headingString,'</p>'))
      else 
          heading <- HTML('<p style="text-align:center"><b><font color="red">Sorry - No Data Available For Selected Type and Time Period.</font></b></p>')
      heading
  })

  output$facetWrap <- renderPlot({
 
      urlstring <- URLencode(paste0("http://phl.carto.com/api/v2/sql?format=csv&q=",
                                    "SELECT date_part('year', Dispatch_Date_Time) as Year, rtrim(text_general_code) as CrimeType, count(*) as Incidents ",
                                    "FROM incidents_part1_part2 ",
                                    "GROUP BY CrimeType, Year ",
                                    "ORDER BY CrimeType, Year")
      )
      crimedata <- read.csv(url(urlstring))
      
      crimedata$year <- as.factor(crimedata$year)
      
      crimedata <- crimedata[crimedata$crimetype != "",] #remove empty category
      
      ggplot(crimedata, aes(year, incidents), group = 1) + 
          xlab("Year") +
          ylab("Number of Incidents") +
          #scale_fill_manual(values = crimedata$crimetype) +
          geom_point(size = 2) +
          geom_line(size = 1, color="navy blue", aes(group=1)) +
          geom_smooth(method = "lm", linetype = "dotted", se = FALSE, color = "red", aes(group=1)) +
          expand_limits(y = 0) +
          #expand_limits(x = max(as.numeric(crimedata$year))+1) +
          facet_wrap(~crimetype, scales = "free_y") +
          theme(#plot.background = element_rect(fill = 'gray'),
                strip.text = element_text(size=10, face="bold"),
                axis.text.x = element_text(angle = 90, hjust = 0.5),
                axis.title=element_text(size=14,face="bold"),
                plot.title = element_text(size = 18, face="bold", hjust = 0.5)) +
          ggtitle("Incidence of Crimes by Year in Philadelphia")

  }, height = 600)

#   observe({
#       print(dates()$startDate)
#       print(dates()$endDate)
#       startDate <- dates()$startDate
#       endDate <- dates()$endDate
#       updateDateRangeInput(session, "daterange", start = startDate, end = endDate)
# })
})
