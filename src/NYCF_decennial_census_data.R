# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL MEASURE THE MINIMUM DISTANCE TO FLOODING OF NEW YORK CITY'S 2010 CENSUS BLOCKS.

## this script is meant to be sourced directly from the script "NYCF_report_CD_impacts.R"
## due to datasets and functions being loaded in the parent script, running this file independently will require line-by-line checks for missing datasets

## we load the input data - 2010 decennial census demographic data at the census block level

NYC_flooding_blocks <- st_read("data/1_raw/NYC_2010_decennial_census.geojson") %>% 
  filter(ALAND10 > 0) %>% # we filter census blocks that are 100% water, since we will not care about their exposure to flooding
  st_transform(UTM_18N_meter)

## the lines below will write a new column in the dataset indicating the distance to the closest flooding 
## the new column called "m_d_f" refers to exposure under the moderate scenario, while "e_d_f" does for the extreme scenario

NYC_flooding_blocks <- write_closest_flooding(NYC_flooding_blocks,
                                              moderate_flooding,
                                              "m_d_f")

NYC_flooding_blocks <- write_closest_flooding(NYC_flooding_blocks,
                                              extreme_flooding,
                                              "e_d_f")


st_write(NYC_flooding_blocks, "data/2_intermediate/NYC_blocks_CD_demo_flood.shp", delete_dsn = TRUE)

NYC_flooding_blocks <- st_read("data/2_intermediate/NYC_blocks_CD_demo_flood.shp")

## in order to aggregate exposure statistics to the CD level, we need to spatially join census blocks to CDs

NYC_flooding_blocks.joined.CDs <- st_join(NYC_flooding_blocks, CD_with_parks, largest = TRUE) %>%
  filter(boro_cd %nin% c(164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595)) %>%
  drop_na(boro_cd)

st_write(NYC_flooding_blocks.joined.CDs, "data/2_intermediate/NYC_blocks_CD_demo_flood.shp", delete_dsn = TRUE)

rm(NYC_flooding_blocks.joined.CDs, NYC_flooding_blocks)
