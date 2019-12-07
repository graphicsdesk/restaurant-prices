library(tidyverse)

rawData <- read.csv("pct_data2.csv") %>% 
  mutate(
    Price = as.character(Price),
    Price = case_when(
      Price %in% c(" < 5", "5 - 7") ~ "< 7",
      Price %in% c("7 - 10", "10 - 15") ~ "7 - 15",
      Price %in% c("30 - 50", "Over $50") ~ "> 30",
      TRUE ~ Price
    )
  ) %>% 
  mutate(
    # Price = factor(Price, levels = c(" < 5", "5 - 7", "7 - 10", "10 - 15", "15 - 20", "20 - 30", "30 - 50", "Over $50", "Uncoded")),
    Price = factor(Price, levels = c("< 7", "7 - 15", "15 - 20", "20 - 30", "> 30", "Uncoded")),
    Year = factor(Year, levels = c("Pre-open", "30 days - 1 yr", "1 - 2 yr", "2 - 5 yr", "5+ yr") %>% rev())
  ) %>%  
  filter(Price != "Uncoded") %>% 
  group_by(Year, Price) %>% 
  summarize(Pct_age_Price = sum(Pct_age_Price))

data <- read.csv("chd-data.csv")
data %>% 
  mutate(
    Price = as.character(Price),
    Year = as.character(Year),
    Price = if_else(Price %in% c("< 5", " 5 - 7"), "< 7", Price),
    Price = if_else(Price %in% c("7 - 10", "10 - 15"), "7 - 15", Price),
    Price = if_else(Price %in% c("30 - 50", "Over $50"), "> 30", Price),
    Year = if_else(Year %in% c("Pre-open", "30 days - 1 yr", "1 - 2 yr"), "< 2 yr", Year),    
  ) %>% 
  mutate(
    Price = factor(Price, levels = c("< 7", "7 - 15", "15 - 20", "20 - 30", "> 30", "Uncoded")),
    Year = factor(Year, levels = c("< 2 yr", "2 - 5 yr", "5+ yr") %>% rev())
  ) %>%
  mutate(
    Price = plyr::revalue(Price, c("< 7" = "Less than $7", "7 - 15" = "$7 to $15", "15 - 20" = "$15 to $20", "20 - 30" = "$20 to $30", "> 30" = "More than $30", "Uncoded" = "Uncoded")),
    Year = plyr::revalue(Year, c("5+ yr" = "5+", "2 - 5 yr" = "2 to 5", "< 2 yr" = "< 2"))
  ) %>% 
  filter(Price != "Uncoded") %>% 
  mutate(Count = ifelse(is.na(Count), 0, Count)) %>% 
  group_by(Year, Price) %>% 
  summarize(Count = sum(Count)) %>% 
  group_by(Price) %>% 
  mutate(
    Total_In_Price_Range = sum(Count),
    Percent = Count / sum(Count)
  ) %>% write.csv(file = "CHD_Data_Export.csv")
  ggplot(aes(Year, Percent)) +
  geom_col(width = 1) +
  facet_wrap(~ Price, ncol = 5) +
  scale_y_continuous(labels = scales::percent_format(10), expand = c(0, 0), limits = c(0, 0.75)) +
  scale_x_discrete(expand = c(0, 0)) +
  xlab("Number of years in business") +
  labs(title = "Restaurant ages by average check amount") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_blank(),
    axis.title.y = element_blank()
  )

# Stacked bar chart
rawData %>% 
  ggplot(aes(Year, Pct_age_Price, fill = Price)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer() +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0), limits = c(0, 1)) +
  xlab("Age of restaurant\n<--- Older --- Newer --->") +
  ylab("Distribution of average check values")

