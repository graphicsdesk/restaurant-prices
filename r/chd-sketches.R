library(tidyverse)

rawData <- read.csv("pct_data2.csv")

#years in business 
#p1 <- ggplot(data = rawData,
#             mapping = aes(x = Year, y = Count))

#p1 + geom_point(mapping = aes(color = Price)) + 
#  scale_x_discrete(limits=c("Pre-open","30 days - 1 yr","1 - 2 yr","2 - 5 yr","5+ yr"))

p1 <- ggplot(rawData, aes(Year,Pct_age_Price,fill = Price))

p1 + geom_col()

