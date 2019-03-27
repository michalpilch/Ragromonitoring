
library(RCurl)
library(httr)
library(jsonlite)

# # Current satellite image data by polygon
# Desctiption:
#   On this initial step we search all available satellite imageries for your polygon and return you URLs to them. URLs are organized in 4 groups - 'image', 'tile', 'stats' and 'data'. Use URLs from these 4 groups in the Step 2 to get images or metadata that you can embed to your application. Now you can get an image of your polygon in PNG, a tile in PNG format, metadata of NDVI & EVI indices of polygon and the image in Tiff format.
#
# API call:
#   http://api.agromonitoring.com/agro/1.0/image/search?start={start date}&end={end date}&polyid={ID of polygon}&appid={API key}

# Parameters of request:
#   Necessary parameters
## appid Personal API key
## polygon_id ID of a polygon
## start Start date of the data search (unix time, UTC), e.g. start=1483218000
## end End date of the data search (unix time, UTC), e.g. end=1504213200

# Optional parameters
# resolution_min px/meters
# resolution_max px/meters
# type Name of datasourse Landsat-8 (l8), Sentinel-2 (s2)
# coverage_max %
# coverage_min %
# clouds_max %
# clouds_min %
#
# Create a polygon
# use dasboard


## dev
#polyid='5c7c3d76792cfd000714e011'
#archutowo

polyid='5c7bfb1f792cfd000714e00d'
appid='537f4839a8903f08c56382d8d8184c17'

# now - 30week
start <- Sys.time()-(30*1036800)
# format to UNIX time
format.POSIXct(start, '%s') -> start
end <- Sys.time()
format.POSIXct(end, '%s') -> end



get_current_img=function(start, end,
                         #resolution_min, resolution_max, type, coverage_max, coverage_min,
                          #clouds_max, clouds_min,
                          polyid, appid)
  #no optional parameters - dev
  # API call:
  #   http://api.agromonitoring.com/agro/1.0/image/search?start={start date}&end={end date}&polyid={ID of polygon}&appid={API key}
{
  url="http://api.agromonitoring.com/agro/1.0/image/search?"

  url=paste(url, "start=", start, "&end=", end, "&polyid=", polyid, "&appid=", appid,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}

get_current_img(start = start, end = end, polyid = polyid, appid = appid) -> image




#
# Get image of your polygon in PNG format with "image"
# Description:
#   This API call is a part of the response of the Satellite Imagery Search API. You can get image for your polygon in the following presets - True color, False color, NDVI and EVI.
#
# In the Step 1 specify the ID of your polygon, the period of searching and your API key. In the API response, you will receive URLs to all availble images. Use these URLs for calling for images in PNG in True color, False color, NDVI and EVI.
#
# Example of API request:
#   Get NDVI satellite image in PNG format
#
# http://api.agromonitoring.com/image/1.0/02059768a00/5ac22f004b1ae4000b5b97cf?appid=bb0664ed43c153aa072c760594d775a7
#
# To get an image of your polygon in other custom palettes, just add an additional parameter to the got URL:
#   paletteid Palette ID (values correspond to the list above - 1, 2, 3, 4)
# Example of API request:
#   http://api.agromonitoring.com/image/1.0/02059768a00/5ac22f004b1ae4000b5b97cf?appid=bb0664ed43c153aa072c760594d775a7&paletteid=1

obraz <- image$image$ndvi[1]


get_current_image=function(obraz, paletteid)

{
  url=obraz

  url=paste(url, "&paletteid=", paletteid, sep="")
  # image_png <- getURL(url)
  # # d=fromJSON(d)
  # d
}

get_current_image(obraz = obraz, 3) -> png_obraz
library(magick)
magick::image_read(png_obraz)

# Get tiles with your polygon in PNG format with "tile"
# leaflet map
library(leaflet)

tile <- image$tile$ndvi[1]
map <- leaflet() %>% setView(20.105913, 52.504884, zoom = 14) %>%
  addTiles() %>%   addTiles(urlTemplate = tile)

image$dt <- as.POSIXct(image$dt , origin="1970-01-01")

##########history
#http://api.agromonitoring.com/agro/1.0/ndvi/history?start={start date}&end={end date}&polyid={ID of polygon}&appid={API key}

# now - week
start <- as.POSIXct("2016-01-01 00:00:00", tz="pl")
# format to UNIX time
format.POSIXct(start, '%s') -> start
end <- Sys.time()
format.POSIXct(end, '%s') -> end



get_historical_NDVI=function(start, end,  polyid, appid)
{
  url="http://api.agromonitoring.com/agro/1.0/image/search?"

  url=paste(url, "start=", start, "&end=", end, "&polyid=", polyid, "&appid=", appid,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}

####
data.frame(historical_NDVI$stats$ndvi) -> NDVI
historical_NDVI$date <- as.POSIXct(historical_NDVI$dt , origin="1970-01-01")



get_history_NDVI=function(x)
{

  d= lapply(NDVI$historical_NDVI.stats.ndvi, FUN = function(x){historical =getURL(x)})
  d=lapply(d, FUN = function(x){historical =fromJSON(x)})
  require(data.table)
  historia <- rbindlist(d, fill=TRUE)
  historia <- cbind(historia, historical_NDVI$date)

}
historia <- cbind(historia, historical_NDVI$date)

ggplot2::ggplot(historia, aes(x=V2, y=median)) +
  geom_point()
################


# get_current_tile=function(image, appid, x, y, z)
#
# {
#   url="http://api.agromonitoring.com/tile/1.0/"
#
#   url=paste(url, z, "/", x, "/", y,"/",  image, "?appid=", appid, sep="")
#   d=getURL(url)
# I$dt))
# }




# Current soil data by polygon
get_current_soil=function(appid, polyid)
{
  url="http://api.agromonitoring.com/agro/1.0/soil?"

  url=paste(url, "polyid=", polyid, "&appid=", appid,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}

# Current UVI data by polygon
# http://api.agromonitoring.com/agro/1.0/uvi?polyid=5aaa8052cbbbb5000b73ff66&appid=bb0664ed43c153aa072c760594d775a7

get_current_UVI=function(appid, polyid)
{
  url="http://api.agromonitoring.com/agro/1.0/uvi?"

  url=paste(url, "polyid=", polyid, "&appid=", appid,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}


# API call:
#   http://api.agromonitoring.com/agro/1.0/weather
#
# Parameters:
#   appid personal api key
#
# polyid ID of polygon
#
# Examples of API calls:
  # http://api.agromonitoring.com/agro/1.0/weather?polyid=5aaa8052cbbbb5000b73ff66&appid=bb0664ed43c153aa072c760594d775a7

get_current_weather=function(appid, polyid)
{
  url="http://api.agromonitoring.com/agro/1.0/weather?"

  url=paste(url, "polyid=", polyid, "&appid=", appid,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}

