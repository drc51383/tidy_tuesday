# Tidy Tuesday Project
# Plastic Pollution from the Break Free from Plastic Audit

# Created by David Christafore
# Created on 01/27/21

# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-01-26')
tuesdata <- tidytuesdayR::tt_load(2021, week = 5)

plastics <- tuesdata$plastics


# Prepare data ------------------------------------------------------------

# Tidy the plastic type columns and sum by country and type

plastics_tidy <- plastics %>% 
  filter(year == 2020, parent_company != "Grand Total") %>% 
  select(-grand_total, -num_events, -volunteers) %>% 
  pivot_longer(empty:pvc, names_to = "type", values_to = "count") %>% 
  group_by(country, type) %>% 
  summarize(count = sum(count))

# Find the top 6 countries in total pieces

total_top_6 <- plastics_tidy %>% 
  group_by(country) %>% 
  summarize(count = sum(count)) %>% 
  slice_max(count, n = 6)

# Keep just the top 6 countries and fix names

plastics_ready <- plastics_tidy %>% 
  filter(country %in% total_top_6$country) %>%
  mutate(
    country = str_replace(country, "United States of America", "USA"),
    type_new = case_when(
      type == "empty" | type == "o" ~ "other/unknown",
      type == "pet" ~ "Polyester",
      type == "pp" ~ "Polypropylene",
      type == "ps" ~ "Polystyrene",
      type == "pvc" ~ "PVC",
      type == "ldpe" ~ "Low density polyethylene",
      type == "hdpe" ~ "High density polyethylene"
    )
  )

# Create plot -------------------------------------------------------------

ggplot(plastics_ready, aes(country, count, fill = type_new)) +
  geom_col(position = "fill") +
  labs(
    y = "Proportion of Total Pieces", 
    x = "Country",
    title = "2020 Break Free from Plastic Audit Results",
    subtitle = "Proportion of pieces for countries with the most pieces",
    fill = "Type of Plastic",
    caption = "Data source: www.breakfreefromplastic.org \n Created by: David Christafore"
    ) + 
  coord_flip()
  
ggsave("plastic_tidy.png")
  
  
  

 
