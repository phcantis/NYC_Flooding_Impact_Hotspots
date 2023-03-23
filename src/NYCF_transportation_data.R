# source("src/NYCF_housekeeping_GIS_vars.R")

bus_stops <- st_read("data/1_raw/bus_stops_nyc_nov2020.shp") %>% 
  st_transform(UTM_18N_meter) %>% 
  st_join(CD, join = st_nn, maxdist = 100, left = FALSE)

bus_stops <- write_closest_flooding(bus_stops,
                                    moderate_flooding,
                                    "m_d_f",
                                    centroids = FALSE)

bus_stops <- write_closest_flooding(bus_stops,
                                    extreme_flooding,
                                    "e_d_f",
                                    centroids = FALSE)

st_write(bus_stops, "data/2_intermediate/bus_stops_flooding.shp", delete_dsn = TRUE)

subway_entrances <- st_read("data/1_raw/geo_export_a9ca2d05-28dc-4a51-9b65-7e17888f49ec.shp") %>% 
  st_transform(UTM_18N_meter) %>% 
  st_join(CD, join = st_nn, maxdist = 100, left = FALSE)

subway_entrances <- write_closest_flooding(subway_entrances,
                                           moderate_flooding,
                                           "m_d_f",
                                           centroids = FALSE)

subway_entrances <- write_closest_flooding(subway_entrances,
                                           extreme_flooding,
                                           "e_d_f",
                                           centroids = FALSE)

st_write(subway_entrances, "data/2_intermediate/subway_entrances_flooding.shp", delete_dsn = TRUE)

rm(bus_stops, subway_entrances)
