library("readr")
library("sf")
library("dplyr")

difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

caro <- read_delim("caro60.csv", ",") |>
  st_as_sf(coords = c("E","N"), crs = 2056) |> 
  select(DatetimeUTC)

#Task 1: Calculate speed at scale 1


caro <- mutate(caro, 
                         timelag = difftime_secs(lead(DatetimeUTC), lag(DatetimeUTC)))



caro <- mutate(caro, 
                         steplength = distance_by_element(lead(geometry), lag(geometry)))

caro <- mutate(caro, 
                         speed = steplength/timelag)

head(caro)


#Task 2: Calculate speed at scale 2


caro <- mutate(caro, 
               timelag2 = difftime_secs(lead(DatetimeUTC, 2), lag(DatetimeUTC, 2)))

caro <- mutate(caro, 
               steplength2 = distance_by_element(lead(geometry, 2), lag(geometry, 2)))

caro <- mutate(caro, 
               speed2 = steplength2/timelag2)

caro |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag2, steplength2, speed2) |> 
  head()



#Task 3: Calculate speed at scale 3


caro <- mutate(caro, 
               timelag3 = difftime_secs(lead(DatetimeUTC, 4), lag(DatetimeUTC, 4)))

caro <- mutate(caro, 
               steplength3 = distance_by_element(lead(geometry, 4), lag(geometry, 4)))

caro <- mutate(caro, 
               speed3 = steplength3/timelag3)

caro |> 
  st_drop_geometry() |> 
  select(timelag3, steplength3, speed3) |> 
  head()


#Task 4: Compare speed across scales

caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

library(ggplot2)

ggplot(caro, aes(y = speed)) + 
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)


library(tidyr)

# before pivoting, let's simplify our data.frame
caro2 <- caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

caro_long <- caro2 |> 
  pivot_longer(c(speed, speed2, speed3))

head(caro_long)

#why now tibble is much smaller?

ggplot(caro_long, aes(name, value)) +
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)

#A steady decrease in median speed as the temporal analysis scale increases;
#A decrease in the overall variance in speed as the temporal scale increases;
#Lower minimum values at the shortest temporal scales;

# we got the same results!

