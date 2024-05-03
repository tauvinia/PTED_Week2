#Import Data
library("readr")
library("sf")

wildschwein_BE <- read_delim("wildschwein_BE_2056.csv", ",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056)

#Getting an overview

difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}

now <- wildschwein_BE$DatetimeUTC

later <- lead(now)

#wildschwein_BE$timelag <- difftime_secs(later, now)
wildschwein_BE

wildschwein_BE <- group_by(wildschwein_BE, TierID)

wildschwein_BE <- mutate(wildschwein_BE, 
       timelag = difftime_secs(lead(DatetimeUTC), DatetimeUTC))

# why we first use later as lead and then as lag? -> should be lead

# Distance

later <-  lead(wildschwein_BE$geometry)
now <- wildschwein_BE$geometry

st_distance(later, now, by_element = TRUE)  # by_element must be set to TRUE

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

steplength <- distance_by_element(later, now)


#Deriving distance & speed


steplength <- st_distance(later, now, by_element = TRUE)

wildschwein_BE <- mutate(wildschwein_BE, 
       steplength = distance_by_element(lead(geometry), geometry))

wildschwein_BE <- mutate(wildschwein_BE, 
       speed = steplength/timelag)

wildschwein_BE 

# Plausibility check

wildschwein_sample <- wildschwein_BE |>
  filter(TierName == "Sabi") |> 
  head(100)

library(tmap)
tmap_mode("view")

tm_shape(wildschwein_sample) + 
  tm_dots()

#tm_shape(wildschwein_BE) + 
 # tm_dots()

wildschwein_sample_line <- wildschwein_sample |> 
  # dissolve to a MULTIPOINT:
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemaps = "OpenStreetMap")

tm_shape(wildschwein_sample_line) +
  tm_lines() +
  tm_shape(wildschwein_sample) + 
  tm_dots()

