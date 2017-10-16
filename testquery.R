urlstring <- URLencode(paste0("http://phl.carto.com/api/v2/sql?format=csv&q=",
                    "SELECT date_part('year', Dispatch_Date_Time) as Year, text_general_code as CrimeType, count(*) as Incidents ",
                    "FROM incidents_part1_part2 ",
                    "GROUP BY Year, text_general_code ",
                    "ORDER BY text_general_code, Year")
)
crimedata <- read.csv(url(urlstring))
str(crimedata)

t <- ggplot(crimedata, aes(year, incidents)) + geom_line()
t+facet_wrap(~crimetype, scales = "free")

install.packages("geojsonio")
library(geojsonio)

install.packages("rgdal")
library(rgdal)

#pngj <- "https://raw.githubusercontent.com/azavea/geo-data/master/Neighborhoods_Philadelphia/Neighborhoods_Philadelphia.geojson"

#phillyneighborhoods <- readLines(url(pngj))

gjfile <- "phillyhoods.geojson"
#jsoncon <- file(gjfile, "w")
#writeLines(phillyneighborhoods,jsoncon)
#close(jsoncon)

phillyhoods <- geojson_read(gjfile, what = "sp")

leaflet(phillyhoods) %>% addTiles() %>% 
    addPolygons(fillOpacity = 0, label = ~mapname, weight = 2)
