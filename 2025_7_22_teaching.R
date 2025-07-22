library(tidyr)
library(dplyr)
library(viridis)
library(sugrrants)
pedestrian17 <- filter(hourly_peds, Year == "2017")
pedestrian17

centre <- pedestrian17 %>% 
  filter(Sensor_Name == "Melbourne Convention Exhibition Centre")
centre_calendar <- centre %>%
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, calendar = "monthly")
centre_calendar
View(centre_calendar)

p1 <- centre_calendar %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line()
p1
prettify(p1)


centre_calendar_free <- centre %>%
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, calendar = "monthly",
                 scale = "free", ncol = 4)
p2 <- ggplot(centre_calendar_free, 
             aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line()
prettify(p2)


centre_calendar_wday <- centre %>%
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, calendar = "monthly",
                 scale = "free_wday", ncol = 4)
p3 <- ggplot(centre_calendar_wday, 
             aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line()
prettify(p3)
