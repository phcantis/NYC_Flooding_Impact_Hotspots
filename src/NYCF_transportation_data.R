# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL MEASURE THE MINIMUM DISTANCE TO FLOODING OF NEW YORK CITY'S BUS STOPS AND SUBWAY ENTRANCES.

## this script is meant to be sourced directly from the script "NYCF_report_CD_impacts.R"
## due to datasets and functions being loaded in the parent script, running this file independently will require line-by-line checks for missing datasets

## we load the input data - NYC's building footprints

bus_stops <- st_read("data/1_raw/bus_stops_nyc_nov2020.shp") %>% 
  st_transform(UTM_18N_meter) %>% 
  st_join(CD, join = st_nn, maxdist = 100, left = FALSE) # spatial join to assign each bus stop a CD. We use a nearest neighbor approach because some bus stops have been observed to lie at the edge of CDs, mainly on the opposite side of the road - e.g. bus stops at the edge of Central Park that would also be used by residents of the opposite side's CD.

bus_stops <- write_closest_flooding(bus_stops,
                                    moderate_flooding,
                                    "m_d_f")

bus_stops <- write_closest_flooding(bus_stops,
                                    extreme_flooding,
                                    "e_d_f")

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
