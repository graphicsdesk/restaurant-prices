install.packages("jsonlite")
library(jsonlite)
json_data <- fromJSON("r_sketch/yelpOutput1.json")

library(tidyverse)

# 1.bar graph(? cause I DON'T HAVE GOOGLE API TO GET MAPSSS)

p <- ggplot(json_data, aes(x = coordinates$longitude, y = coordinates$latitude))

p +
  geom_point(aes(color = price), alpha = 0.6) +
  scale_x_continuous(limits = c(-73.98, -73.93)) +
  scale_y_continuous(limits = c(40.78, 40.826)) +
  facet_wrap(~ price)

