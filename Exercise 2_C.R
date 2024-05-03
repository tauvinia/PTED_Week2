# In the semester project, you will analyse your own movement data, collected with an app 
#of your choice or the GPS tracker (see Preparation Project). Acquire this data and save 
#it to a subfolder of your current R Project named data. Follow the instructions provided 
#in the column Collect data in Table 2.1. 

#Now, import your data in the same way you imported the the wild boar data in task 1. 


library("readr")

data <- read_delim("combined_data.csv", ",")

#Next, start exploring your data, similarly as you did in task 2. At a minimum:
#Import your data as a data frame and convert it to an sf object, using the correct 
#CRS information

library("sf")

data_sf <- st_as_sf(data,
                           coords = c("longitude", "latitude"),
                           crs = 4326
)

#Convert your data to CH1903+ LV95

data_2056 <- data_sf |> st_transform(2056)


#Make a map of your data using ggplot2 or tmap.

library("ggplot2")
ggplot(data, aes(x = longitude, y = latitude)) +
  geom_point() +
  theme(legend.position = "none")


library("dplyr")

data_smry <- summarise(data_2056)

data_smry

mcp <- st_convex_hull(data_smry)
plot(mcp)

ggplot(mcp) +
  geom_sf(alpha = 0.4) 

ggplot(mcp) +
  geom_sf(alpha = 0.4) +
  coord_sf(datum = 2056) 


library("tmap")

#tm_shape (add data) +
# what want to do with it


library("terra")

pk100_BE <- terra::rast("pk100_BE.tif")

pk100_BE

plot(pk100_BE)

plotRGB(pk100_BE)
tm_shape(pk100_BE) +
  tm_rgb() +
  tm_shape (mcp) +
  tm_polygons(alpha = 0.4, border.col = "red")+
  tm_legend(bg.col = "white")

#Task 6
tmap_mode("view")

tm_shape(mcp) +
  tm_polygons(alpha = 0.4, border.col = "red") +
  tm_legend(bg.color = "white")













