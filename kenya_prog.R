# Tidy Tuesday Project
# Internet Usage in Kenya

# Created by David Christafore
# Created on 01/22/21


# Load libraries ----------------------------------------------------------

library(rKenyaCensus)
library(tidyverse)
library(sf)
library(viridis)

# Load data ---------------------------------------------------------------

# County internet data

county_internet <- V4_T2.33%>% 
  filter(AdminArea == "County") %>% 
  mutate(County = str_to_lower(County)) %>% 
  select(county = County, UoI_Total_Perc)

# Shapefile of Kenya counties 

kenya_counties <- st_read("ke_county.shp")


# Join data ---------------------------------------------------------------

# Join internet usage data to the shapefile
# First clean the county names to match the names in the usage data

kenya_counties_j <- kenya_counties %>% 
  mutate(
    county = str_to_lower(county),
    county = str_replace(county, "elgeyo-marakwet", "elgeyo/marakwet"),
    county = str_replace(county, "nairobi", "nairobi city"),
    county = str_replace(county, "taita taveta", "taita/taveta"),
    county = str_replace(county, "tharaka-tithi", "tharaka-nithi")
    ) %>% 
  left_join(county_internet, by = "county")


# Create map --------------------------------------------------------------

ggplot(kenya_counties_j) +
  geom_sf(mapping = aes(fill = UoI_Total_Perc)) + 
  labs(
    title = "Internet Usage in Kenya", 
    caption = "Data source: rKenyaCensus \n Created by: David Christafore", 
    fill = "Internet Users (%)",
    subtitle = "Percentage of persons 3 years and above who used the internet"
    ) + 
  scale_fill_viridis_b() + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.border = element_blank()
  )



