# Public parks in Trafford #

# Source: Trafford Council
# URL: https://www.trafford.gov.uk/residents/leisure-and-lifestyle/parks-and-open-spaces/Parks-in-Trafford.aspx
# Licence: OGL v3.0 ; © OpenStreetMap contributors (ODbL)

# WARNING 2021-11-09 ---------
# There are currently problems with this script.
# The output created by the joining of the `parks` and `osm` objects only creates 19 observations of 3 variables.
# This is not the expected output when looking at the current parks.csv file (32 obs. of 19 variables), with observations based on facilities etc.
# I suspect that the final version of this pre-processing.R script that produced the output in the current format did not get committed to GitHub.
# For reproducibility this script needs amending to create the desired output, however at the time of writing some of the source information on the council website is also outdated.
# As a temporary measure amendments are being made directly to parks.csv

library(tidyverse) ; library(rvest) ; library(osmdata) ; library(sf)

# scrape park names from Trafford website
url_base <- "https://www.trafford.gov.uk/residents/leisure-and-lifestyle/parks-and-open-spaces/Parks-in-Trafford.aspx"
url_list <- read_html("https://www.trafford.gov.uk/residents/leisure-and-lifestyle/parks-and-open-spaces/Parks-in-Trafford.aspx") %>% 
  html_nodes(".parks-list__item a") %>% 
  html_attr("href")

parks <- map_df(url_list, function(i) {
  cat(".")
  page <- read_html(paste0(url_base, i))
  tibble(name = html_text(html_nodes(page, "h1")))
})

# retrieve parks from OpenStreetMap
osm <- opq(bbox = c(-2.478454,53.35742,-2.253022,53.48037)) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf() %>% 
  magrittr::extract2("osm_polygons") %>% 
  mutate(name = case_when(name == "Ashton Park" ~ "Ashton on Mersey Park", TRUE ~ name),
         lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_set_geometry(NULL) %>% 
  select(name, lon, lat)

# write results
parks %>% 
  left_join(osm, by = "name") %>% 
  drop_na() %>% 
  write_csv("parks.csv")
