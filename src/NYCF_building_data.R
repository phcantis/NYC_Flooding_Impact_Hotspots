# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL MEASURE THE MINIMUM DISTANCE TO FLOODING OF NEW YORK CITY'S BUILDING FOOTPRINTS.

## this script is meant to be sourced directly from the script "NYCF_report_CD_impacts.R"
## due to datasets and functions being loaded in the parent script, running this file independently will require line-by-line checks for missing datasets

## we load the input data - NYC's building footprints

buildings_flooding <- st_read("data/1_raw/Building_Footprints_20250507.geojson") %>%
  st_transform(UTM_18N_meter) %>% 
  st_make_valid()

## since it is already a very large dataset, we remove unecessary buildings by removing those located in the CDs assigned to parks, which are out of scope in this study
## this will also reduce the time taken to carry out the exposure assessment

buildings_flooding <- buildings_flooding %>%
  st_join(CD, largest = TRUE) %>% 
  select(boro_cd) %>%
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

## the lines below will write a new column in the dataset indicating the distance to the closest flooding 
## the new column called "m_d_f" refers to exposure under the moderate scenario, while "e_d_f" does for the extreme scenario

buildings_flooding <- write_closest_flooding(buildings_flooding,
                                               moderate_flooding,
                                               "m_d_f")

buildings_flooding <- write_closest_flooding(buildings_flooding,
                                               extreme_flooding,
                                               "e_d_f")

st_write(buildings_flooding, "data/2_intermediate/buildings_CD_flood.shp", delete_dsn = TRUE)

rm(buildings_flooding)
