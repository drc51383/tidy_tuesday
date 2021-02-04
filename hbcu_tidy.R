# Tidy Tuesday Project
# HBCU and College Attainment Data

# Created by David Christafore
# Created on 02/04/21

# Load libraries ----------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(janitor)

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2021-02-02')

hbcu_all <- tuesdata$hbcu_all %>% 
  clean_names()

# Explore type ------------------------------------------------------------

hbcu_type <- hbcu_all %>% 
  select(year, private = total_private, public = total_public) %>% 
  pivot_longer(-year, names_to = "type", values_to = "enrollment") 

ggplot(hbcu_type, aes(year, enrollment, color = type)) +
  geom_line() + 
  scale_y_continuous(labels = scales::comma) +
  labs(
    y = "Enrollment", 
    x = "Year",
    title = "HBCU Enrollment 1976-2015",
    subtitle = "Enrollment in Private vs Public Colleges",
    caption = "Data source: Data.World \n Created by: David Christafore"
  ) +
  theme_bw() + 
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )



