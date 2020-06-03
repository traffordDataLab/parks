# Public parks in Trafford #

# Source: Trafford Council
# URL: https://www.trafford.gov.uk/residents/leisure-and-lifestyle/parks-and-open-spaces/parks-in-trafford/parks-in-trafford.aspx
# Licence: OGL v3.0 ; © OpenStreetMap contributors (ODbL)

library(tidyverse) ; library(rvest) ; library(osmdata) ; library(sf)

# scrape park names from Trafford website
url_base <- "https://www.trafford.gov.uk"
url_list <- read_html("https://www.trafford.gov.uk/residents/leisure-and-lifestyle/parks-and-open-spaces/parks-in-trafford/parks-in-trafford.aspx") %>% 
  html_nodes("#main-article li .sys_t0") %>% 
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
