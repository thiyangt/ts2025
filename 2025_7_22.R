library(forcats)
library(lubridate)
library(tidyverse)
library(sugrrants)
library(showtext)
library(tsibble)
theme_set(theme_bw())

pedestrian_2016 <- read_rds("pedestrian-2016.rds")


hol16 <- holiday_aus(2016, state = "VIC") %>% 
  bind_rows(tibble(holiday = "AFL", date = ymd("20160930")))
workday <- fct_inorder(c("Work day", "Non-work day"))
# turning implicit missingness to explicit
pedestrian_2016 <- pedestrian_2016 %>% 
  as_tsibble(key = Sensor_Name, index = Date_Time) %>% 
  group_by(Sensor_Name) %>% 
  fill_gaps(.full = TRUE) %>% 
  ungroup() %>% 
  mutate(
    Year = year(Date_Time),
    Day = wday(Date_Time, label = TRUE, week_start = 1),
    Time = hour(Date_Time),
    Date = as_date(Date_Time),
    Workday = if_else(
      (Date %in% hol16$date) | Day %in% c("Sat", "Sun"),
      workday[2], workday[1])
  )

# selected sensors
sensors <- c("State Library", "Flagstaff Station", "Birrarung Marr")
sensor_cols <- c(
  "State Library" = "#5e3c99", 
  "Flagstaff Station" = "#b2abd2", 
  "Birrarung Marr" = "#e66101"
)

## ---- time-series-plot
# subsetting the data
subdat <- pedestrian_2016 %>% 
  filter(Sensor_Name %in% sensors) %>% 
  mutate(Sensor_Name = fct_reorder(Sensor_Name, -Latitude, na.rm = TRUE))
# conventional time series plot
subdat %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line(size = 0.3) +
  facet_grid(
    Sensor_Name ~ ., 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  scale_x_datetime(date_labels = "%d %b %Y", date_minor_breaks = "1 month") +
  theme(legend.position = "bottom") +
  xlab("Date Time") +
  ylab("Hourly Counts")

## ---- facet-time
# time series plot faceted by sensors and day of week
subdat %>% 
  ggplot(aes(x = Time, y = Hourly_Counts, group = Date, colour = Sensor_Name)) +
  geom_line(size = 0.3) +
  facet_grid(
    Sensor_Name ~ Day, 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_x_continuous(breaks = seq(0, 24, by = 6)) +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("Hourly Counts")


###
rdbu <- c("Work day" = "#d7191c", "Non-work day" = "#2c7bb6")
# calendar plot for Flagstaff station
fs <- subdat %>% 
  filter(Sensor_Name == "Flagstaff Station")

fs_cal <- fs %>%
  frame_calendar(x = Time, y = Hourly_Counts, date = Date)

p_fs <- fs_cal %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_line() +
  scale_color_manual(values = rdbu) +
  theme(legend.position = "bottom")
prettify(p_fs)

# calendar plot for fs street station using local scale
fs_cal_free <- fs %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, scale = "free")

p_fs_free <- fs_cal_free %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_line() +
  scale_color_manual(values = rdbu) +
  theme(legend.position = "bottom")
prettify(p_fs_free)

# calendar plot for fs street station in polar coordinates
fs_polar <- fs %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, polar = TRUE)

p_fs_polar <- fs_polar %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Workday)) +
  geom_path() +
  scale_color_manual(values = rdbu) +
  theme(legend.position = "bottom")
prettify(p_fs_polar)

# overlaying calendar plots 
subset_cal <- subdat %>% 
  frame_calendar(Time, Hourly_Counts, Date)

sensor_cols2 <- c(
  "#5e3c99" = "#5e3c99", 
  "#b2abd2" = "#b2abd2", 
  "#e66101" = "#e66101"
) # 4-class PuOr without #fdb863
p_three <- subset_cal %>% 
  ggplot() +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[1]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols2[1])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[2]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols2[2])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[3]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols2[3])
  ) +
  scale_colour_identity(
    name = "Sensor",
    breaks = names(sensor_cols2),
    labels = c(
      "State Library", 
      "Flagstaff Station",
      "Birrarung Marr"
    ),
    guide = "legend"
  ) +
  theme(legend.position = "bottom")
prettify(p_three)



## ---- facet
# calendar plots faceted by the sensors
facet_cal <- subdat %>% 
  group_by(Sensor_Name) %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, nrow = 2)

p_facet <- facet_cal %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line(aes(colour = Sensor_Name)) +
  facet_grid(
    Sensor_Name ~ ., 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_colour_manual(name = "Sensor", values = sensor_cols, guide = "legend") +
  theme(legend.position = "bottom")
prettify(p_facet, size = 3, label.padding = unit(0.1, "lines"))


#####################
# boxplot
pedestrian_dec <- pedestrian_2016 %>% 
  filter(Month == "December") %>% 
  frame_calendar(
    x = Time, y = Hourly_Counts, date = Date, width = 0.97, height = 0.97
  )
p_boxplot <- pedestrian_dec %>% 
  ggplot() +
  geom_boxplot(
    aes(x = .Time, y = .Hourly_Counts, group = Date_Time),
    outlier.size = 0.3, width = 0.004, position = "identity",
    colour = "grey50"
  ) +
  geom_smooth(
    aes(.Time, .Hourly_Counts, group = Date), 
    se = FALSE, method = "loess"
  )
p_boxplot
