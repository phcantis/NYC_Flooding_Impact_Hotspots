# source("src/NYCF_housekeeping_GIS_vars.R")

### buildings --> calculate min distance to each flooding scenario

buildings_flooding <- st_read("data/1_raw/Building_Footprints_20250507.geojson") %>%
  st_transform(UTM_18N_meter) %>% 
  st_make_valid()

CD_buildings_join <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd)

buildings_flooding <- buildings_flooding %>%
  st_join(CD_buildings_join, largest = TRUE) %>% 
  select(boro_cd) %>%
  st_transform(crs = UTM_18N_meter) %>%
  st_make_valid() %>%
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

# shortest distance from tax lots CENTROIDS to flooding

buildings_flooding <- write_closest_flooding(buildings_flooding,
                                               moderate_flooding,
                                               "m_d_f",
                                               centroids = FALSE)

buildings_flooding <- write_closest_flooding(buildings_flooding,
                                               extreme_flooding,
                                               "e_d_f",
                                               centroids = FALSE)

st_write(buildings_flooding, "data/2_intermediate/buildings_CD_flood.shp", delete_dsn = TRUE)

rm(buildings_flooding)
