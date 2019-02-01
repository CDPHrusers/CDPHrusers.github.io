

# install.packages(c("sf","tidyverse","date.table"))
library(sf)
library(tidyverse)
library(data.table)




stateFIPs <- "06"
TRurl <-
  paste0(
    "https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR",
    stateFIPs,
    ".txt"
  )
TRcenters <- fread(TRurl)


TRcenters
class(TRcenters$STATEFP)

TRcentersAlt <- fread(
  TRurl,
  colClasses = c(
    "character",
    "character",
    "character",
    "integer",
    "double",
    "double"
  )
) %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP, TRACTCE))



TRcentersAlt
class(TRcentersAlt$STATEFP)




TRcentersSP <- st_as_sf(TRcenters,
                        coords = c("LONGITUDE", "LATITUDE"),
                        crs = 4326)

plot(TRcentersSP["POPULATION"])







HMSday <-
  20180801 # define the day in the format it appears in the

year <- substr(HMSday, 1, 4)
month <- substr(HMSday, 5, 6)
day <- substr(HMSday, 7, 8)

directory <-
  tempdir() # create a temporary directory to save the files as they're downloaded.

setwd(directory) # set the working directory to the temp directory we just created

shpFiles <-
  list(
    paste0(
      "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
      HMSday,
      ".dbf"
    ),
    paste0(
      "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
      HMSday,
      ".shp"
    ),
    paste0(
      "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
      HMSday,
      ".shx"
    )
  )

shpNames <- list("HMSGIS.dbf", "HMSGIS.shp", "HMSGIS.shx")

GISfiles <- mapply(download.file,
                   # the function to execute
                   shpFiles,
                   # a list of the urls to download
                   shpNames,
                   # a list of the names of the destinations for the downloaded files
                   MoreArgs = list(method = "auto", mode = "wb")) # additional arguments for download.file function

Smk <-
  st_read(dsn = "HMSGIS.shp") %>%  st_set_crs(4326) # read in the shapefile and

Smk



Smk <- Smk %>%
  mutate(
    year = substr(HMSday, 1, 4),
    month = substr(HMSday, 5, 6),
    day = substr(HMSday, 7, 8),
    date = paste0(year, month, day),
    smoke = factor(
      ifelse(
        Density %in% c(5, "5.000"),
        "light",
        ifelse(
          Density %in% c(27, "27.000") ,
          "heavy",
          ifelse(Density %in% c(16, "16.000"), "medium", "missing")
        )
      ),
      levels = c("light", "medium", "heavy", "missing")
    )
  )

plot(Smk["smoke"],
     axes = TRUE, # include axes labels in the plot
     key.pos = 1) # place legend at the bottom





# install.packages("lwgeom")
library(lwgeom)

SmkTractDayInt <- st_intersection(st_make_valid(Smk),
                                  TRcentersSP) %>%
  select(date,
         year,
         month,
         day,
         smoke,
         STATEFP,
         COUNTYFP,
         TRACTCE,
         POPULATION)

SmkTractDayInt




st_geometry(SmkTractDayInt) <-
  NULL # remove the spatial information for the moment and if we want to work with it we can comment this line out

singleDay <- SmkTractDayInt  %>%
  mutate(yn = 1) %>% # since every observataion in our result is an instance of smoke with people, we'll give them all a value of 1
  group_by(date,
           year,
           month,
           day,
           smoke,
           STATEFP,
           COUNTYFP,
           TRACTCE,
           POPULATION) %>% # then we'll group by these variables
  summarise(yn = mean(yn)) %>% # and our yes/no marker will be averaged for instances of common smoke categories
  spread(key = smoke, value = yn)  # lastly, let's restructure the data to a wider format, where each smoke category has its own column
# this is not exactly tidy-compliant, but we'll be using this soon

unlink(paste0(tempdir(), '/*'))     # clear temp folder




sum(filter(singleDay, heavy == 1)$POPULATION)  # here is that quick calculation using the tidyr notation

as.data.table(singleDay)[heavy == 1, sum(POPULATION)] # here is that quick calculation using the data.table notation





filter(singleDay, heavy == 1) %>% arrange(POPULATION)



singleDayFull <- left_join(TRcenters, singleDay) %>%
  replace_na(
    list(
      date = HMSday,
      year = substr(HMSday, 1, 4),
      month = substr(HMSday, 5, 6),
      day = substr(HMSday, 7, 8),
      light = 0,
      medium = 0,
      heavy = 0
    )
  )



processHMSday <- function(HMSday) {
  year <- substr(HMSday, 1, 4)
  month <- substr(HMSday, 5, 6)
  day <- substr(HMSday, 7, 8)
  
  directory <-
    tempdir() # create a temporary directory to save the files as they're downloaded.
  
  setwd(directory) # set the working directory to the temp directory we just created
  
  shpFiles <-
    list(
      paste0(
        "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
        HMSday,
        ".dbf"
      ),
      paste0(
        "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
        HMSday,
        ".shp"
      ),
      paste0(
        "http://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/GIS/ARCHIVE/hms_smoke",
        HMSday,
        ".shx"
      )
    )
  
  shpNames <- list("HMSGIS.dbf", "HMSGIS.shp", "HMSGIS.shx")
  
  GISfiles <- mapply(download.file,
                     # the function to execute
                     shpFiles,
                     # a list of the urls to download
                     shpNames,
                     # a list of the names of the destinations for the downloaded files
                     MoreArgs = list(method = "auto", mode = "wb")) # additional arguments for download.file function
  
  Smk <- st_read(dsn = "HMSGIS.shp") %>%
    st_set_crs(4326) %>%
    mutate(
      year = substr(HMSday, 1, 4),
      month = substr(HMSday, 5, 6),
      day = substr(HMSday, 7, 8),
      date = paste0(year, month, day),
      smoke = factor(
        ifelse(
          Density %in% c(5, "5.000"),
          "light",
          ifelse(
            Density %in% c(27, "27.000") ,
            "heavy",
            ifelse(Density %in% c(16, "16.000"), "medium", "missing")
          )
        ),
        levels = c("light", "medium", "heavy", "missing")
      )
    )
  
  SmkTractDayInt <- st_intersection(st_make_valid(Smk),
                                    TRcentersSP) %>%
    select(date,
           year,
           month,
           day,
           smoke,
           STATEFP,
           COUNTYFP,
           TRACTCE,
           POPULATION) %>%
    mutate(yn = 1) %>%
    group_by(date,
             year,
             month,
             day,
             smoke,
             STATEFP,
             COUNTYFP,
             TRACTCE,
             POPULATION) %>%
    summarise(yn = mean(yn)) %>%
    spread(key = smoke, value = yn)
  
  st_geometry(SmkTractDayInt) <- NULL
  
  singleFull <- left_join(TRcenters, SmkTractDayInt) %>%
    replace_na(
      list(
        date = HMSday,
        year = substr(HMSday, 1, 4),
        month = substr(HMSday, 5, 6),
        day = substr(HMSday, 7, 8),
        light = 0,
        medium = 0,
        heavy = 0
      )
    )
  
  unlink(paste0(tempdir(), '/*'))
  
  return(singleFull)
  
}





dateList <- 20181108:20181116

campFire <-
  rbindlist(lapply(dateList, FUN = processHMSday), fill = TRUE)


# install.packages("jsonlite")
library(jsonlite)

variable <-
  "B09020_001E" # this is a placeholde for the ACS varibale we'll read in, t in this case population over 65
# "B17021_016E" # in poverty and living alone


# first read in the data from the ACS and store as a data frome
#examining the data will show that the first row is the names of the data
elderly <-
  as.data.frame(fromJSON(
    paste0(
      "https://api.census.gov/data/2016/acs/acs5?get=NAME,",
      variable,
      "&for=tract:*&in=state:",
      stateFIPs,
      "%20county:*"
    ),
    flatten = T
  ))

# elderlyName <- fromJSON(paste0("https://api.census.gov/data/2016/acs/acs5/variables/",variable,".json"), flatten = T)$label

# here we can elimiate the uneccessary first row and column AND...
# change the name and type of the variables to match our table of smoke exposures
elderly <-
  elderly[-1, -1] %>% mutate(
    estimate = as.integer(as.character(V2)),
    STATEFP = as.integer(as.character(V3)),
    COUNTYFP = as.integer(as.character(V4)),
    TRACTCE = as.integer(as.character(V5))
  ) %>%
  select(STATEFP, COUNTYFP, TRACTCE, estimate)

elderly




elderlySmoke <- merge(campFire, elderly)




elderlySmoke %>%
  group_by(day) %>%
  summarise(
    lightPDs = sum(light * POPULATION, na.rm = T),
    mediumPDs = sum(medium * POPULATION, na.rm = T),
    heavyPDs = sum(heavy * POPULATION, na.rm = T)
  ) %>%
  gather(lightPDs,
         mediumPDs,
         heavyPDs,
         key = smoke,
         value = PersonDays) %>%
  mutate(smoke = factor(
    x = smoke,
    levels = c("lightPDs", "mediumPDs", "heavyPDs")
  )) %>%
  ggplot(aes(
    x = day,
    y = PersonDays / 1000000,
    fill = smoke
  )) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("yellow", "orange", "red")) +
  ylab("Millions of Persons Exposed") +
  xlab("Nov day")




elderlySmoke %>%
  group_by(day) %>%
  summarise(
    heavyUnder65 = sum(heavy * (POPULATION - estimate), na.rm = T),
    heavyOver65 = sum(heavy * estimate, na.rm = T)
  ) %>%
  gather(heavyUnder65, heavyOver65, key = smoke, value = PersonDays) %>%
  mutate(smoke = factor(
    x = smoke,
    levels = c("heavyUnder65", "heavyOver65")
  )) %>%
  ggplot(aes(
    x = day,
    y = PersonDays / 1000000,
    fill = smoke
  )) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("pink", "red")) +
  ylab("Millions of Persons Exposed") +
  xlab("Nov day")




# install.packages("leaflet")
library(leaflet)

# first sumarize the data we want by location, aggregating person days of exposure by tract

mapData <- elderlySmoke %>%
  group_by(STATEFP, COUNTYFP, TRACTCE, LATITUDE, LONGITUDE) %>%
  summarise(
    ExposedPOP = sum(heavy * POPULATION, na.rm = T),
    ExposedPOP65 = sum(heavy * estimate, na.rm = T)
  )

# define the palette for the results showing full population
palPOP <- colorQuantile(palette = "Blues",
                        domain = mapData$ExposedPOP,
                        n = 3)

# define the palette to show the elderly segment of the population
pal65 <- colorQuantile(palette = "Reds",
                       domain = mapData$ExposedPOP65,
                       n = 3)


# here we create the Leaft map with two layers, one for all ages and one for elderly
mapData %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% # these are the tiles for the background of the map - find more here http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  addCircleMarkers(
    # here we add the first layer, since our data has lat and lon, we can add the tabular data direcetly to a map
    radius = 3,
    color = ~ palPOP(ExposedPOP),
    stroke = FALSE,
    fillOpacity = .3,
    label = ~ ExposedPOP,
    # here is text that will appear on mouseover
    popup = ~ ExposedPOP,
    # this is text to appear on a click
    group = "All Ages"
  ) %>%
  addCircleMarkers(
    # here we add the second layer, since our data has lat and lon, we can add the tabular data direcetly to a map
    radius =  ~ ExposedPOP65 / max(mapData$ExposedPOP65) * 10,
    color = ~ pal65(ExposedPOP65),
    stroke = FALSE,
    fillOpacity = .3,
    label = ~ paste0(
      format(ExposedPOP65, big.mark = ","),
      " over 65 person-days of exposure in the Week the Camp Fire began"
    ),
    popup = ~ ExposedPOP65,
    group = "Over 65"
  ) %>%
  addLayersControl(baseGroups =  c("All Ages", "Over 65"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  setView(lat = 37.085206,
          lng = -119.540085,
          zoom = 6) %>% #defaults the view of the original map to show the entire state
  addEasyButton(easyButton(
    icon = "fa-globe",
    title = "Zoom to State",
    onClick = JS(
      "function(btn, map){ map.setView([37.085206, -119.540085],6); }"
    )
  )) # adds a button to return to the whole staet view
