# Demo 
#Difftime

difftime()
 now <- as.POSIXct("2024-04-26 10:20:00")
later <- as.POSIXct("2024-04-26 11:35:00") 

time_difference <- difftime(later,now, units = "secs")

time_difference
str(time_difference)

as.numeric(difftime(later,now, units = "secs"))

difftime_secs <- function(later, now) {
  as.numeric(difftime(later, now, units = "secs"))
}


#Lead and lag

numbers <- 1:10

numbers

library(dplyr)

lead(numbers)

#offseting by 1 value

lead(numbers,2)

lag(numbers)
#in the other direction

lag(numbers, 4, default = 9999)

#Mutate

wildschwein <- tibble(
  TierID = c(rep("Hans", 5), rep("Klara", 5)),
  DatetimeUTC = rep(as.POSIXct("2015-01-01 00:00:00", tz = "UTC") + 0:4 * 15 * 60, 2)
)

wildschwein

now <- wildschwein$DatetimeUTC

later <- lead(now)

wildschwein$timelag <- difftime_secs(later, now)
wildschwein

wildschwein <- group_by(wildschwein, TierID)

mutate(wildschwein, 
      timelag2 = difftime_secs(lead(DatetimeUTC), DatetimeUTC))

#ctr shift m

wildschwein <- wildschwein |> 
  group_by(TierID) |> 
  mutate(
  timelag2 = difftime_secs(lead(DatetimeUTC), DatetimeUTC)
  )


#Summary

wildschwein |> 
  group_by(TierID) |> 
  summarise(
    first_sample = min(DatetimeUTC)
  )

summarise(wildschwein, mean = mean(timelag, na.rm = TRUE))



install.packages("gitcreds")
library("gitcreds")
gitcreds_set()
gitcreds_get()
