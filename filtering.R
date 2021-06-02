#rm(list=ls())

library(tidyverse)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(plotly)

data <- read_csv("hfi_cc_2020.csv")

data <- data %>%
  select(year,hf_rank,pf_religion_restrictions,ef_legal,region,ISO_code,countries,ef_money_growth) 

data %>%
  filter(year == 2018) %>%
  arrange(hf_rank)

religious_vs_hf <- data %>% 
  select(year,countries,pf_religion_restrictions,hf_rank) %>%
  filter(year == 2018)

ggplot(data = religious_vs_hf,aes(hf_rank,pf_religion_restrictions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Impact of religious restrictions on the human freedom rank of countries",
      x = "Human Freedom Rank",
      y = "Religious Restrictions")

##could highlight bottom and top 5 hf_rank_countries 

legal_restriction_yrs <- data %>% 
  select(ef_legal,year,countries,region,hf_rank) %>%
  filter(year == 2017) %>%
  filter(region == "South Asia")

ggplot(data = legal_restriction_yrs,aes(countries,ef_legal, fill = countries)) +
  geom_col() +
  geom_text(aes(label = hf_rank), vjust = -0.2) +
  labs(title = "Impact of legal restrictions on the human freedom rank of countries",
       x = "Countries",
       y = "Legal Restrictions") 

top_money_growth <- data %>% 
  select(year,countries,region,ef_money_growth,hf_rank) %>% 
  filter(region == "South Asia") %>%
  distinct(countries,.keep_all = TRUE) %>%
  distinct(hf_rank,.keep_all = TRUE) %>%
  arrange(hf_rank) %>%
  head(5) 
  
print(top_money_growth)

money_plot <-
  data %>%
  filter(countries %in% top_money_growth$countries, na.rm = TRUE)

print(money_plot)

ggplot(data = money_plot,aes(year,ef_money_growth, col = countries)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = hf_rank), vjust = -0.2) +
  labs(title = "Economic growth in countries",
       x = "Countries",
       y = "Money Growth") +
  xlim(2008,2018)
 
top_money_growth %>% data %>% 
  arrange(desc(ef_money_growth)) %>% 
  select(countries) %>% 
  head(5)

country_vs_hf <- data %>% 
  group_by(countries) %>% 
  select(hf_rank, countries)

map_data("world") %>%
  mutate(ISO_code = iso.alpha(region, n = 3)) -> map_data

hf_rank_data <- left_join(map_data,data,by = "ISO_code") 

hf_rank_map <- ggplot(hf_rank_data) +
  geom_polygon(mapping = aes(x = long,y = lat, group = group, fill = hf_rank)) +
  coord_quickmap()

print(hf_rank_map)

data_2018 <- data %>%
  select(year,hf_rank,pf_religion_restrictions,ef_legal,region,ISO_code,countries,ef_money_growth) %>%
  filter(year == "2018")

plot_ly(data_2018, type='choropleth', locations=data_2018$ISO_code, z=data_2018$hf_rank, text=data_2018$countries)

output$point_widget <- renderUI({
  sliderInput("points", label = "Chose human freedom ranks",
              min = 1,
              max = 10,
              selected = 10)
})

highlight_point <- reactive({
  data %>%
    filter(year %in% religious_data()$year) %>%
    filter(hf_rank <= input$points)
})

geom_point(highlight_point(),aes(hf_rank,pf_religion_restrictions),color = "red")
