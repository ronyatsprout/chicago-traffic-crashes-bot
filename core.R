install.packages("rgeos", repos="http://R-Forge.R-project.org", type="source")
install.packages("rgdal", repos="http://R-Forge.R-project.org", type="source")
library(devtools)
install_github("r-spatial/sf", configure.args = "--with-proj-lib=/usr/local/lib/")
library(RSocrata)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(sf)
library(leaflet)
library(webshot2)
library(viridis)
library(rgeos)
library(leaflet)
library(colorspace)
library(leaflegend)
library(rtweet)
install_github("hunzikp/MapColoring")
library(MapColoring)
library(data.table)
library(rmarkdown)


dateQueryString <- paste0("crash_date between '",ymd(today(tz = "America/Chicago")-1), "' and '", ymd(today(tz = "America/Chicago")-0),"'")

##Authenticate Twitter
auth <- rtweet_bot(
  api_key       = Sys.getenv("TWITTER_API_KEY"),
  api_secret    = Sys.getenv("TWITTER_API_KEY_SECRET"),
  access_token  = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

##Check if Data is ready
sleep <- function(x){
  Sys.sleep(x)
}

numberOfChecks <- 0

checkIfDataRefreshed <- function(dateQueryString){
  chicagoCrashPeople <- read.socrata(paste0("https://data.cityofchicago.org/resource/u6pd-qa9d.json?$where=",dateQueryString))
  chicagoCrashCrashes <- read.socrata(paste0("https://data.cityofchicago.org/resource/85ca-t3if.json?$where=",dateQueryString))
  if(nrow(chicagoCrashCrashes)<10 | nrow(chicagoCrashPeople)<10){
    dataReadyFlag <- FALSE
    if(numberOfChecks == 0){
      auth_as(auth)
      ## post reply
      delayTweet<-post_tweet("Beep Boop Bop, I'm still waiting for CPD to upload data. Please hold.")
      numberOfChecks <- numberOfChecks +1 
    }else{
      numberOfChecks <- numberOfChecks +1 
    }
    message(paste0("DATA IS NOT READY YET! As of ",now(tz='America/Chicago')))
    sleep(900)
    checkIfDataRefreshed(dateQueryString)
  }else{
    if(numberOfChecks > 0){
      ##DELETE data check tweet
      auth_as(auth)
      my_timeline <- get_my_timeline()
      ## ID for destruction
      destroy_id <- my_timeline$id_str[1]
      post_destroy(destroy_id)
    }
    dataReadyFlag <- TRUE
    return(dataReadyFlag)
  }
}

checkIfDataRefreshed(dateQueryString)

if(dataReadyFlag == TRUE){
  chicagoCrashPeople <- read.socrata(paste0("https://data.cityofchicago.org/resource/u6pd-qa9d.json?$where=",dateQueryString))
  chicagoCrashCrashes <- read.socrata(paste0("https://data.cityofchicago.org/resource/85ca-t3if.json?$where=",dateQueryString))
}else{
  checkIfDataRefreshed(dateQueryString)
}


wardMap <- read_sf("https://data.cityofchicago.org/resource/k9yb-bpqx.geojson")
alderList <- read.socrata("https://data.cityofchicago.org/resource/htai-wnw4.json")

chicagoCrash <- chicagoCrashPeople %>%
  left_join(chicagoCrashCrashes)

crashes <- chicagoCrash 
#%>%
#  filter( longitude != 0)
#filter(injury_classification == "NONINCAPACITATING INJURY" | injury_classification ==  "REPORTED, NOT EVIDENT"
#       | injury_classification == "INCAPACITATING INJURY"  | injury_classification == "FATAL" ,
#       crash_date >= '2017-01-01', longitude != 0)

crashesLongLats <- crashes %>%
  select(longitude, latitude, crash_record_id, person_id)

# create a points collection
pnts_sf <- do.call("st_sfc",c(lapply(1:nrow(crashesLongLats), 
                                     function(i) {st_point(as.numeric(crashesLongLats[i, 1:2]))}), list("crs" = 4326))) 

pnts_trans <- st_transform(pnts_sf, 2163) # apply transformation to pnts sf
tt1_trans <- st_transform(wardMap, 2163)      # apply transformation to polygons sf

# intersect and extract state name
crashesLongLats$ward <- as.integer(paste(apply(st_intersects(tt1_trans, pnts_trans, sparse = FALSE), 2, 
                                               function(col) { 
                                                 tt1_trans[which(col), ]$ward
                                               })))

crashesWard <- crashes %>%
  left_join(crashesLongLats %>% filter(!is.na(ward))) %>%
  mutate(ward = as.factor(ward)) %>%
  mutate(injury_super_class = if_else(injury_classification == 'NO INDICATION OF INJURY' | is.na(injury_classification), 'No Injury', 
                                      if_else(injury_classification == 'NONINCAPACITATING INJURY' | injury_classification =='REPORTED, NOT EVIDENT', 'Injury',
                                              if_else(injury_classification =='INCAPACITATING INJURY', 'Severe Injury', 'Fatal')))) %>%
  mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))


crashesPersonInjury <- crashesWard %>%
  filter(injury_super_class != 'No Injury' & injury_super_class != 'Fatal') %>%
  count(person_type, injury_super_class) %>%
  rename(Injuries = n)

numberOfPeopleInjured <- sum(crashesPersonInjury$Injuries)

crashesPersonFatal <- crashesWard %>%
  filter(injury_super_class == 'Fatal') %>%
  count(person_type, injury_super_class) %>%
  rename(Fatalities = n)

numberOfPeopleKilled <- sum(crashesPersonFatal$Fatalities)

crashesNoInjury <- crashesWard %>%
  filter(injury_super_class == 'No Injury') %>%
  count(person_type, injury_super_class) %>%
  rename(Crashes = n)

numberOfPeopleNotInjured <- sum(crashesNoInjury$Crashes)

##How many wards had a crash, how many didn't
crashesWardInjury <- crashesWard %>%
  filter(injury_super_class != 'No Injury') %>%
  count(ward) %>%
  rename(Injuries = n) %>%
  filter(!is.na(ward))

numberOfWardsWithInjuries <- nrow(crashesWardInjury)


crashesWardCrash <- crashesWard %>%
  count(ward) %>%
  rename(Crashes = n) %>%
  filter(!is.na(ward))

numberOfWardsWithCrashes <- nrow(crashesWardCrash)


c(1:50)[!(c(1:50) %in% crashesWardCrash$ward)]



## Define globals
LEAFLET_TILES <- "CartoDB.DarkMatter"

##==============================================================================
## DOWNLOAD DATA
##==============================================================================

## Contained code to download Chicago's wards
shpCityWards <- local({
  cur <- getwd()
  on.exit(setwd(cur))
  
  tmp <- tempfile(fileext = ".zip")
  setwd(dirname(tmp))
  # url <- "https://data.cityofchicago.org/api/geospatial/sp34-6z76?method=export&format=Shapefile"
  url <- "https://data.cityofchicago.org/api/geospatial/sp34-6z76?method=export&format=GeoJSON"
  download.file(url, destfile = tmp)
  shp <- rgdal::readOGR(basename(tmp), stringsAsFactors = FALSE)
  shp
})

## Generate city outline
shpCityOutline <- rgeos::gUnaryUnion(as(shpCityWards, "SpatialPolygons"))



##==============================================================================
## LABEL WITH DEFAULT LABEL LOCATIONS
##==============================================================================

## Extract labels by pulling "labpt"
## This is how you *should* be able to do it
# labs <- sapply(shpCityWards@polygons, `@`, "labpt")

## Extract labels by pulling "labpt"
labs <- vector("list", length(shpCityWards@polygons))
for(i in 1:length(shpCityWards@polygons)){
  labs[[i]] <- c(shpCityWards@polygons[[i]]@labpt,
                 as.numeric(shpCityWards@polygons[[i]]@ID))
}
labs <- data.table(do.call(rbind, labs))
setnames(labs, c("x","y", "ward"))


##==============================================================================
## LABEL WITH gCentroid LOCATIONS
##==============================================================================

## That wasn't too great. 
## Try with creating labs with gCentroid

## Create labels
labs <- as.data.frame(gCentroid(shpCityWards, byid = TRUE))
labs$ward <- shpCityWards$ward


##==============================================================================
## LABEL WITH centroid FUNCTION
##==============================================================================
## Source for functions:
## https://gis.stackexchange.com/a/265475/78424

#' find the center of mass / furthest away from any boundary
#' 
#' Takes as input a spatial polygon
#' @param pol One or more polygons as input
#' @param ultimate optional Boolean, TRUE = find polygon furthest away from centroid. False = ordinary centroid

require(rgeos)
require(sp)

centroid <- function(pol,ultimate=TRUE,iterations=5,initial_width_step=10){
  if (ultimate){
    new_pol <- pol
    # For every polygon do this:
    for (i in 1:length(pol)){
      width <- -initial_width_step
      area <- gArea(pol[i,])
      centr <- pol[i,]
      wasNull <- FALSE
      for (j in 1:iterations){
        if (!wasNull){ # stop when buffer polygon was alread too small
          centr_new <- gBuffer(centr,width=width)
          # if the buffer has a negative size:
          substract_width <- width/20
          while (is.null(centr_new)){ #gradually decrease the buffer size until it has positive area
            width <- width-substract_width
            centr_new <- gBuffer(centr,width=width)
            wasNull <- TRUE
          }
          # if (!(is.null(centr_new))){
          #   plot(centr_new,add=T)
          # }
          new_area <- gArea(centr_new)
          #linear regression:
          slope <- (new_area-area)/width
          #aiming at quarter of the area for the new polygon
          width <- (area/4-area)/slope
          #preparing for next step:
          area <- new_area
          centr<- centr_new
        }
      }
      #take the biggest polygon in case of multiple polygons:
      d <- disaggregate(centr)
      if (length(d)>1){
        biggest_area <- gArea(d[1,])
        which_pol <- 1                             
        for (k in 2:length(d)){
          if (gArea(d[k,]) > biggest_area){
            biggest_area <- gArea(d[k,])
            which_pol <- k
          }
        }
        centr <- d[which_pol,]
      }
      #add to class polygons:
      new_pol@polygons[[i]] <- remove.holes(new_pol@polygons[[i]])
      new_pol@polygons[[i]]@Polygons[[1]]@coords <- centr@polygons[[1]]@Polygons[[1]]@coords
    }
    centroids <- gCentroid(new_pol,byid=TRUE)
  }else{
    centroids <- gCentroid(pol,byid=TRUE)  
  }  
  return(centroids)
}

#Given an object of class Polygons, returns
#a similar object with no holes

remove.holes <- function(Poly){
  # remove holes
  is.hole <- lapply(Poly@Polygons,function(P)P@hole)
  is.hole <- unlist(is.hole)
  polys <- Poly@Polygons[!is.hole]
  Poly <- Polygons(polys,ID=Poly@ID)
  # remove 'islands'
  max_area <- largest_area(Poly)
  is.sub <- lapply(Poly@Polygons,function(P)P@area<max_area)  
  is.sub <- unlist(is.sub)
  polys <- Poly@Polygons[!is.sub]
  Poly <- Polygons(polys,ID=Poly@ID)
  Poly
}
largest_area <- function(Poly){
  total_polygons <- length(Poly@Polygons)
  max_area <- 0
  for (i in 1:total_polygons){
    max_area <- max(max_area,Poly@Polygons[[i]]@area)
  }
  max_area
}


labs <- centroid(pol = shpCityWards, 
                 ultimate = TRUE,
                 iterations = 150,
                 initial_width_step = .01)
labs$ward <- shpCityWards$ward


shpCityWards <- merge(shpCityWards, crashesWardCrash, by="ward") %>%
  merge(crashesWardInjury, by="ward")

#map crash concentration
pal <- colorNumeric(
  palette = "RdPu",
  domain = shpCityWards$Crashes)


concentrationOfCrashesMap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(LEAFLET_TILES) %>%
  addPolygons(data = shpCityOutline, fill = FALSE, color = "black", weight = 5) %>%
  addPolygons(data = shpCityWards, color = ~pal(Crashes), fillOpacity = 0.3, 
              weight = 0.5, label = ~paste("Ward:", ward) ) %>%
  addLabelOnlyMarkers(data = labs, ~labs$x, ~labs$y, label = ~as.character(ward),
                      labelOptions = labelOptions(noHide = TRUE,
                                                  direction = "center",
                                                  offset = c(0, 0), opacity = 1, 
                                                  textsize = "12px", textOnly = TRUE, 
                                                  style = list("font-style" = "bold",
                                                               color = "white"))) %>%
  addLegend(
    "topright",
    pal <- colorNumeric(
      palette = "RdPu",
      domain = shpCityWards$Crashes),
    values = shpCityWards$Crashes, 
    opacity = 0.3
  )

#map injury concentration
pal <- colorNumeric(
  palette = "RdPu",
  domain = shpCityWards$Injuries)


concentrationOfInjuriesMap <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(LEAFLET_TILES) %>%
  addPolygons(data = shpCityOutline, fill = FALSE, color = "black", weight = 5) %>%
  addPolygons(data = shpCityWards, color = ~pal(Injuries), fillOpacity = 0.3, 
              weight = 0.5, label = ~paste("Ward:", ward) ) %>%
  addLabelOnlyMarkers(data = labs, ~labs$x, ~labs$y, label = ~as.character(ward),
                      labelOptions = labelOptions(noHide = TRUE,
                                                  direction = "center",
                                                  offset = c(0, 0), opacity = 1, 
                                                  textsize = "12px", textOnly = TRUE, 
                                                  style = list("font-style" = "bold",
                                                               color = "white"))) %>%
  addLegend(
    "topright",
    pal <- colorNumeric(
      palette = "RdPu",
      domain = shpCityWards$Injuries),
    values = shpCityWards$Injuries, 
    opacity = 0.3
  )



getSize <- function(crashesWard) {
  sapply(crashesWard$injury_super_class, function(injury_super_class) {
    if(injury_super_class == 'No Injury') {
      .2
    } else if(injury_super_class == 'Fatal') {
      1
    } else if(injury_super_class == 'Severe Injury') {
      .6
    } else {
      .4
    } })
}


getColor <- function(crashesWard) {
  sapply(crashesWard$injury_super_class, function(injury_super_class) {
    if(injury_super_class == 'No Injury') {
      "yellow"
    } else if(injury_super_class == 'Fatal') {
      "aquamarine"
    } else {
      "red"
    } })
}


orderedInjuries <- c('No Injury', 'Injury', 'Severe Injury', 'Fatal')
factorPal <- colorFactor('BrBG', domain = orderedInjuries, ordered = TRUE)


crashCoordsMap <- leaflet(crashesWard, options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles(LEAFLET_TILES) %>%
  addPolygons(data = shpCityOutline, fill = FALSE, color = "white", weight = 1) %>%
  addPolygons(data = shpCityWards, color = '#EAEAE0', fillOpacity = 0.1, 
              weight = 0.5, label = ~paste("Ward:", ward) ) %>%
  addSymbolsSize(values = ~getSize(crashesWard),
                 lat = ~latitude,
                 lng = ~longitude,
                 shape = c('circle'),
                 color = ~factorPal(injury_super_class),
                 opacity=1,
                 baseSize=3) %>%
  addLegendFactor(pal = factorPal, shape = "circle",values =  ~factor(injury_super_class, levels = orderedInjuries), position = 'topright')


numberOfPeople <- nrow(chicagoCrashPeople)
numberOfCrashes <- nrow(chicagoCrashCrashes)
##ALDERS SOCIALS
alderSocialsData <- read.csv('data/alder_social_media_list.csv')

alderSocials <- alderSocialsData %>%
  mutate(twitter = trimws(if_else(twitter =='', alder, twitter)))

pedSafetyCommitteeSocials <- alderSocials %>%
  filter(committee_on_ped_safety != '') %>%
  select(twitter)

##FATALITY ALERT TWEET
if(numberOfPeopleKilled>0){
  fatalityAlertText <- "FATALITY ALERT! "
  
  fatalityTweetFlag <- TRUE
  
  fatalitiesByPersonType <- crashesPersonFatal %>%
    group_by(person_type) %>% 
    summarise(fatalities = sum(Fatalities))
  
  fatalityPersonTypes<-data.frame()
  
  if('BICYCLE' %in% fatalitiesByPersonType$person_type){
    bikeFatalities <- fatalitiesByPersonType$fatalities[fatalitiesByPersonType$person_type=='BICYCLE']
    fatalityPersonTypes<-rbind(fatalityPersonTypes,"cyclist")
  }else{
    bikeFatalities <- 0
  }
  if('PEDESTRIAN' %in% fatalitiesByPersonType$person_type){
    pedFatalities <- fatalitiesByPersonType$fatalities[fatalitiesByPersonType$person_type=='PEDESTRIAN']
    fatalityPersonTypes<-rbind(fatalityPersonTypes, "pedestrian")
  }else{
    pedFatalities <- 0
  }
  if('DRIVER' %in% fatalitiesByPersonType$person_type){
    driverFatalities <- fatalitiesByPersonType$fatalities[fatalitiesByPersonType$person_type=='DRIVER']
    fatalityPersonTypes<-rbind(fatalityPersonTypes, "driver")
  }else{
    driverFatalities <- 0
  }
  if('PASSENGER' %in% fatalitiesByPersonType$person_type){
    passengerfatalities <- fatalitiesByPersonType$fatalities[fatalitiesByPersonType$person_type=='PASSENGER']
    fatalityPersonTypes<-rbind(fatalityPersonTypes, "passenger")
  }else{
    passengerFatalities <- 0
  }
  
  fatalityPersonTypesString <- paste(fatalityPersonTypes, collapse = ", ")
  
  #Ward(s) where people were killed
  wardsWithFatalities <- crashesWard %>%
    filter(injury_super_class == 'Fatal') %>%
    select(ward) %>%
    unique() %>%
    mutate(ward = as.integer(ward))
  
  alderSocialsWithFatalies <- wardsWithFatalities %>%
    left_join(alderSocials)
  
  wardsWithFatalitiesString <- paste(wardsWithFatalities$ward, collapse = ", ")
  
  fatalityAlertTweetText <- paste0('FATALITY ALERT: there was ',numberOfPeopleKilled, 
                                   ' person(s) killed by traffic violence in Chicago on ', ymd(today(tz = "America/Chicago")-1),'.\n\n',
                                   'People (including ',fatalityPersonTypesString ,') died due to vehicular violence in these wards: ', wardsWithFatalities,'.\n\n',
                                   paste(alderSocialsWithFatalies$twitter,collapse = ", "), ' people are being killed in your ward. #FatalityAlert')
}else{
  fatalityAlertText<-''
  fatalityTweetFlag <- FALSE
}

firstTweetText <- paste0(ymd(today(tz = "America/Chicago")-1), ": ", numberOfCrashes, ' Traffic Crashes w/ ', 
                           numberOfPeople, ' People Involved. ',fatalityAlertText,'\n\n@ChicagoDOT make it stop. #ChicagoCrashMap')

htmlwidgets::saveWidget(widget = crashCoordsMap, file = "maps/temp/crashCoordsMap/map.html", selfcontained = FALSE)
webshot2::webshot(url = "maps/temp/crashCoordsMap/map.html", file = paste0("maps/","crashCoordsMap", "-", ymd(today(tz = "America/Chicago")-1),".png"), 
                  delay = 1,
                  zoom = 3)
firstTweetImg <- paste0(getwd(),"/maps/","crashCoordsMap", "-", ymd(today(tz = "America/Chicago")-1),".png")


###SECOND TWEET
head(crashesWardCrash %>% arrange(desc(Crashes)), n =5)[1,1]
secondTweetText <- paste0('Concentration of Traffic Crashes by Ward.', '\n\nWorst 5 Wards:\n', 
                         '#ward', head(crashesWardCrash %>% arrange(desc(Crashes)), n =5)[1,1], '\n',
                         '#ward', head(crashesWardCrash %>% arrange(desc(Crashes)), n =5)[2,1], '\n',
                         '#ward', head(crashesWardCrash %>% arrange(desc(Crashes)), n =5)[3,1], '\n',
                         '#ward', head(crashesWardCrash %>% arrange(desc(Crashes)), n =5)[4,1], '\n',
                         '#ward', head(crashesWardCrash %>% arrange(desc(Crashes)), n =5)[5,1], '\n',
                         '\n#ConcentrationOfCrashes')
cat(secondTweetText)

htmlwidgets::saveWidget(widget = concentrationOfCrashesMap, file = "maps/temp/concentrationOfCrashesMap/map.html", selfcontained = FALSE)
webshot2::webshot(url = "maps/temp/concentrationOfCrashesMap/map.html", file = paste0("maps/","concentrationOfCrashesMap", "-", ymd(today(tz = "America/Chicago")-1),".png"), 
                  delay = 1,
                  zoom = 3)
secondTweetImg <- paste0(getwd(),"/maps/","concentrationOfCrashesMap", "-", ymd(today(tz = "America/Chicago")-1),".png")

###THIRD TWEET
head(crashesWardInjury %>% arrange(desc(Injuries)), n =5)[1,1]
thirdTweetText <- paste0('Concentration of Traffic Injuries by Ward.', '\n\nWorst 5 Wards:\n', 
                          '#ward', head(crashesWardInjury %>% arrange(desc(Injuries)), n =5)[1,1], '\n',
                          '#ward', head(crashesWardInjury %>% arrange(desc(Injuries)), n =5)[2,1], '\n',
                          '#ward', head(crashesWardInjury %>% arrange(desc(Injuries)), n =5)[3,1], '\n',
                          '#ward', head(crashesWardInjury %>% arrange(desc(Injuries)), n =5)[4,1], '\n',
                          '#ward', head(crashesWardInjury %>% arrange(desc(Injuries)), n =5)[5,1], '\n',
                          '\n#ConcentrationOfInjuries')

htmlwidgets::saveWidget(widget = concentrationOfInjuriesMap, file = "maps/temp/concentrationOfInjuriesMap/map.html", selfcontained = FALSE)
webshot2::webshot(url = "maps/temp/concentrationOfInjuriesMap/map.html", file = paste0("maps/","concentrationOfInjuriesMap", "-", ymd(today(tz = "America/Chicago")-1),".png"), 
                  delay = 1,
                  zoom = 3)
thirdTweetImg <- paste0(getwd(),"/maps/","concentrationOfInjuriesMap", "-", ymd(today(tz = "America/Chicago")-1),".png")


###FOURTH TWEET

injuriesByPersonType <- crashesPersonInjury %>%
  group_by(person_type) %>% 
  summarise(injuries = sum(Injuries))

if(injuriesByPersonType$injuries[injuriesByPersonType$person_type=='BICYCLE']>0){
  bikeInjuries <- injuriesByPersonType$injuries[injuriesByPersonType$person_type=='BICYCLE']
}else{
  bikeInjuries <- 0
}
if(injuriesByPersonType$injuries[injuriesByPersonType$person_type=='PEDESTRIAN']>0){
  pedInjuries <- injuriesByPersonType$injuries[injuriesByPersonType$person_type=='PEDESTRIAN']
}else{
  pedInjuries <- 0
}
if(injuriesByPersonType$injuries[injuriesByPersonType$person_type=='DRIVER']>0){
  driverInjuries <- injuriesByPersonType$injuries[injuriesByPersonType$person_type=='DRIVER']
}else{
  driverInjuries <- 0
}
if(injuriesByPersonType$injuries[injuriesByPersonType$person_type=='PASSENGER']>0){
  passengerInjuries <- injuriesByPersonType$injuries[injuriesByPersonType$person_type=='PASSENGER']
}else{
  passengerInjuries <- 0
}


fourthTweetText <- paste0('In summary, there were ',numberOfCrashes,' crashes with ',numberOfPeople,' people involved in those crashes.\n', 

       numberOfPeopleInjured,' were injured. ',numberOfPeopleNotInjured,' were not injured.\n',

'Of the injured: ',pedInjuries,' ped(s), ',bikeInjuries,' cyclist(s), ',passengerInjuries,' passengers, ',driverInjuries,' drivers.\n',

'There were injuries in ',numberOfWardsWithInjuries,' wards and crashes in ',numberOfWardsWithCrashes,'. #Summary')


auth_as(auth)

#first post
post_tweet(firstTweetText, media = firstTweetImg,
           media_alt_text = "Coordinates of all individuals involved in a reported traffic crash.")

##fatality tweet
if(fatalityTweetFlag == TRUE){
  ## lookup status_id
  my_timeline <- get_my_timeline()
  ## ID for reply
  reply_id <- my_timeline$id_str[1]
  ## post reply
  post_tweet(fatalityAlertTweetText, in_reply_to_status_id = reply_id)
}

## lookup status_id
my_timeline <- get_my_timeline()
## ID for reply
reply_id <- my_timeline$id_str[1]
## post reply
post_tweet(secondTweetText, media = secondTweetImg,
           media_alt_text = "Concentration of reported traffic crashes by ward.",
           in_reply_to_status_id = reply_id)

## lookup status_id
my_timeline <- get_my_timeline()
## ID for reply
reply_id <- my_timeline$id_str[1]
## post reply
post_tweet(thirdTweetText, media = thirdTweetImg,
           media_alt_text = "Concentration of reported traffic injuries by ward.",
           in_reply_to_status_id = reply_id)

## lookup status_id
my_timeline <- get_my_timeline()
## ID for reply
reply_id <- my_timeline$id_str[1]
## post reply
post_tweet(fourthTweetText,
           in_reply_to_status_id = reply_id)


lastTweet <- paste0('Comm. on Ped Safety - ', paste(pedSafetyCommitteeSocials$twitter,collapse = ", "))

## lookup status_id
my_timeline <- get_my_timeline()
## ID for reply
reply_id <- my_timeline$id_str[1]
## post reply
post_tweet(lastTweet, in_reply_to_status_id = reply_id)
