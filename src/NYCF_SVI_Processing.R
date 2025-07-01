# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL PREPARE THE SOCIAL VULNERABILITY INDEX DATA PROVIDED BY THE CDC (2022, CENSUS TRACT LEVEL) FOR GENERATING A SUPPLEMENTARY FIGURE COMPARING THE SVI WITH OUR MAPPED SOCIAL VULNERABILITY.

source("src/NYCF_housekeeping_GIS_vars.R")

CD <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd) %>%
  filter(boro_cd %nin% c(164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

SVI_NYC <- sf::st_read(dsn = "data/1_raw/SVI2022_NEWYORK_tract.gdb/",
                       layer = "SVI2022_NEWYORK_tract") %>%
  filter(RPL_THEMES != -999) %>% 
  st_make_valid() %>%
  st_transform(UTM_18N_meter) %>% 
  dplyr::filter(COUNTY %in% c("Kings County", "Queens County", "Richmond County", "New York County", "Bronx County")) %>% 
  st_intersection(st_dissolve(CD, cast_to = "MULTIPOLYGON")) %>% 
  select(FIPS, SVI = RPL_THEMES)

CD <- st_interpolate_aw(x = select(SVI_NYC, SVI), to = CD, extensive = FALSE)

st_write(SVI_NYC, "data/2_intermediate/SVI2022_CensusTracts.geojson")
st_write(CD, "data/2_intermediate/CD_SVI2022_interpolated.geojson")
