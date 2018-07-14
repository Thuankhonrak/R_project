
install.packages("tidyverse")
library(tidyverse)


install.packages("nycflights13")
library(nycflights13)

1. 
flights2 <- flights %>% select(year:day, arr_delay, tailnum)

plane2 <- mutate(planes, age = 2013-year ) %>% select(tailnum, year, manufacturer, age, seats)

join_plane <- plane2 %>% left_join(flights2, by = "tailnum") %>% group_by(age) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))

ggplot(na.omit(join_plane)) + geom_point(mapping = aes(x = age, y = delay))

2. 
flights3 <- flights %>% select(year:day, arr_delay)

join_weather1 <- weather %>% left_join(flights3, by = c("year", "month", "day")) %>% group_by(temp) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))
join_weather2 <- weather %>% left_join(flights3, by = c("year", "month", "day")) %>% group_by(dewp) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))
join_weather3 <- weather %>% left_join(flights3, by = c("year", "month", "day")) %>% group_by(humid) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))
join_weather4 <- weather %>% left_join(flights3, by = c("year", "month", "day")) %>% group_by(wind_dir) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))
join_weather5 <- weather %>% left_join(flights3, by = c("year", "month", "day")) %>% group_by(wind_speed) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))
join_weather6 <- weather %>% left_join(flights3, by = c("year", "month", "day")) %>% group_by(wind_gust) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))
join_weather7 <- weather %>% left_join(flights3, by = c("year", "month", "day")) %>% group_by(precip) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))

ggplot(na.omit(join_weather1)) + geom_point(mapping = aes(x = temp, y = delay))
ggplot(na.omit(join_weather2)) + geom_point(mapping = aes(x = dewp, y = delay))
ggplot(na.omit(join_weather3)) + geom_point(mapping = aes(x = humid, y = delay))
ggplot(na.omit(join_weather4)) + geom_point(mapping = aes(x = wind_dir, y = delay))
ggplot(na.omit(join_weather5)) + geom_point(mapping = aes(x = wind_speed, y = delay))
ggplot(na.omit(join_weather6)) + geom_point(mapping = aes(x = wind_gust, y = delay))
ggplot(na.omit(join_weather7)) + geom_point(mapping = aes(x = precip, y = delay))


3.
anti_join(flights, airports, by = c("dest" = "faa"))
anti_join(airports, flights, by = c("faa" = "dest"))

#ggplot
visual_bar <- flights %>% filter(origin == "JFK", dest == "LAX") %>% select(year, month, day, origin, dest) %>% group_by(month) 
visual_bar_plot <- summarise(visual_bar, count = n())
visual_bar_plot <- data.frame(visual_bar_plot)
ggplot(visual_bar_plot, aes(x = month, y = count)) + geom_bar(stat = "identity")

#Boxplot
visual_box <- flights %>% filter(carrier == "UA") %>% select(year, month, day, arr_delay) %>% group_by(year, month, day) %>% summarise(delay = mean(arr_delay, na.rm = TRUE))


#corrdinate 
bar <- ggplot(visual_bar_plot) + geom_bar(mapping = aes(x = month, y = count), show.legend = FALSE, width = 1, stat = "identity") + theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)
bar + coord_polar()

