# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL ASSESS THE EXPOSURE TO FLOODING FOR EACH DATASET USED IN THE STUDY. 
# EXPOSURE ASSESSMENTS ARE CARRIED OUT BY MEASURING THE MINIMUM DISTANCE TO FLOODING OF EACH SPATIAL ENTITY, SO THAT A FILTERING APPROACH CAN BE USED LATER.
# EXPOSURE METRICS ARE SUMMARIZED TO THE SCALE OF THE ANALYSIS - COMMUNITTY DISTRICTS IN NYC

## in this study, we flag small spatial entities as exposed when they lay within a 30m radius of flooding. 
## users interested in using a different radius may update the variable below (in meters)

## exposure_criteria_meters <- 30

## we begin loading the libraries, functions, and variables that we will recurrently use

source("src/NYCF_housekeeping_GIS_vars.R")

## we begin loading a Community Districts (CD) layer and filter out  the dataset's entities that represent parks (e.g. Central Park). 
## large park areas lack socioeconomic and hazard data, and hence are excluded from the risk analysis.

CD <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%                                                                 # we only need this field, depicting each CD's unique identifier
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd) %>%
  filter(boro_cd %nin% c(164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595)) # CD's assigned to parks

## some spatial operations will require using the full NYC area, so we load a version of the CD dataset that includes park areas

CD_with_parks <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd)

## now we load flood hazard data for each scenario

extreme_flooding <- sf::st_read(dsn = "data/1_raw/NYC_Stormwater_Flood_Map_-_Extreme_Flood.gdb/NYC Stormwater Flood Map - Extreme Flood.gdb",
                                layer = "ExtremeFlood_single_part") %>%
  st_cast("MULTIPOLYGON") %>%       # we make sure that all geometries are treated as multipolygon. Some geometries in the original dataset are provided as multisurface and may generate incosistent results
  st_make_valid() %>%               # sanity check after altering geometry types to make sure the new geometries are 100% valid
  st_transform(UTM_18N_meter) %>% 
  st_dissolve() %>%                 # we blend all flood hazard geometries to have a single flood hazard type that will be treated equally across the study. 
  st_intersection(CD)               # each flooding geometry is intersected an assigned to its overlapping CD

  
moderate_flooding <- sf::st_read(dsn = "data/1_raw/NYC_Stormwater_Flood_Map_-_Moderate_Flood.gdb/NYC Stormwater Flood Map - Moderate Flood.gdb",
                                 layer = "ModerateFlood_single_part") %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>%
  st_transform(UTM_18N_meter) %>% 
  st_dissolve() %>% 
  st_intersection(CD)

## now we will prepare all the data by measuring the distance to flooding of each spatial entity. 
## for sanity, each exposure assessment is made in a separate script.
## ATTENTION these scripts (esp for tax lots and buildings) take a REALLY long time to run and significant computation resources.
## outputs of each script are already provided in the "data/2_intermediate" folder to save time. 
## attempts to reproduce and run these scripts must expect long running times.

## Run script ONCE to assess the exposure of decennial census data
# source("src/NYCF_decennial_census_data.R")

## Run script ONCE to assess the exposure of decennial ACS data
# source("src/NYCF_ACS_data.R")

## Run script ONCE to assess the exposure of tax lot data
# source("src/NYCF_tax_lots_data.R")

## Run script ONCE to assess the exposure of buildings data
# source("src/NYCF_buildings_data.R")

## Run script ONCE to assess the exposure of facilities data
# source("src/NYCF_facilities_data.R")

## Run script ONCE to assess the exposure of transportation data
# source("src/NYCF_transportation_data.R")

## now that we have measured all the distances to closest flooding for the input data, we will progressively add columns to our CDs dataset, depicting different exposure indicators.
## additional exposure assessments relying on other metrics than distance (e.g. %area) will be performed along the way.

## we begin with some columns to initiate our CDs final table reporting flooding exposure indicators

columns <- c("Geography",              # final table uses the term "Geography" to refer to each CD identifier in order to avoid confusions in the code when calling "boro_cd" in the CD dataset
             "Total_area", 
             "Total_road_area",
             "M.Total_road_area",
             "M.PCT_road_area",
             "E.Total_road_area",
             "E.PCT_road_area")


## set up final table with the defined starting columns 
final_table <- data.frame(matrix(ncol = length(columns), nrow = nrow(CD)))
names(final_table) <- columns
final_table$Geography <- c(CD$boro_cd)    

## geographic total areas (based on CDs layer) - square meters

final_table["Total_area"] <- c(st_area(CD))

## assess impact of flooding in each CD - % area flooded in each CD

M.area_flooded <- moderate_flooding %>% 
  mutate(area = st_area(moderate_flooding)) %>% 
  st_drop_geometry() %>% 
  group_by(boro_cd) %>% 
  summarise(M.Total_area = sum(area)) # all indicators with an "M." prefix refer to the moderate scenario

E.area_flooded <- extreme_flooding %>% 
  mutate(area = st_area(extreme_flooding)) %>% 
  st_drop_geometry() %>% 
  group_by(boro_cd) %>% 
  summarise(E.Total_area = sum(area)) # all indicators with an "E." prefix refer to the extreme scenario

final_table <- final_table %>%
  left_join(M.area_flooded, by = c("Geography" = "boro_cd")) %>% 
  left_join(E.area_flooded, by = c("Geography" = "boro_cd")) %>% 
  mutate(M.PCT_area = 100  * M.Total_area / Total_area,
         E.PCT_area = 100  * E.Total_area / Total_area)

## population exposure, considering total population, as well as age and race (based on decennial census at census block level)
## we will first create summary statistics for each CD regardless of flooding
## and then we will generate the same metrics isolating the data of exposed / non-exposed blocks

NYC_flooding_blocks.joined.CDs <- st_read("data/2_intermediate/NYC_blocks_CD_demo_flood.shp") %>%
  st_drop_geometry()

## summary statistics for each CD's total population

demographic_summary <- (NYC_flooding_blocks.joined.CDs) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(Total_population = sum(Total, na.rm = TRUE),
                   Total_black_population = sum(AfAm, na.rm = TRUE),
                   Total_hispanic_population = sum(Latinx, na.rm = TRUE),
                   Total_white_population = sum(White, na.rm = TRUE),
                   PCT_black_population = (100 * sum(AfAm, na.rm = TRUE) / sum(Total, na.rm = TRUE)),
                   PCT_hispanic_population = 100 * sum(Latinx, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   PCT_white_population = 100 * sum(White, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   Total_below5_population = sum(b5, na.rm = TRUE),
                   Total_above65_population = sum(a65, na.rm = TRUE),
                   PCT_below5_population = 100* sum(b5, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   PCT_above65_population = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE)) %>%
  mutate(Total_bipoc_population = Total_population - Total_white_population,
         PCT_bipoc_population = 100 - PCT_white_population)

final_table <- inner_join(final_table, demographic_summary, by = c("Geography" = "boro_cd"))

## summary statistics for each CD's exposed population, under each scenario
## at the same time, we generate the same stats for each CD's population that is NOT exposed (indicators with prefixes "M.N." and "E.N.")

m.exposed_population <- filter(NYC_flooding_blocks.joined.CDs, m_d_f == 0)
m.unexposed_population <- filter(NYC_flooding_blocks.joined.CDs, m_d_f > 0)

e.exposed_population <- filter(NYC_flooding_blocks.joined.CDs, e_d_f == 0)
e.unexposed_population <- filter(NYC_flooding_blocks.joined.CDs, e_d_f > 0)

M.demographic_summary <- (m.exposed_population) %>%
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise("M.Total_population" = sum(Total, na.rm = TRUE),
                   "M.Total_black_population" = sum(AfAm, na.rm = TRUE),
                   "M.Total_hispanic_population" = sum(Latinx, na.rm = TRUE),
                   "M.Total_white_population" = sum(White, na.rm = TRUE),
                   "M.PCT_black_population" = (100 * sum(AfAm, na.rm = TRUE) / sum(Total, na.rm = TRUE)),
                   "M.PCT_hispanic_population" = 100 * sum(Latinx, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "M.PCT_white_population" = 100 * sum(White, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "M.Total_below5_population" = sum(b5, na.rm = TRUE),
                   "M.Total_above65_population" = sum(a65, na.rm = TRUE),
                   "M.PCT_below5_population" = 100* sum(b5, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "M.PCT_above65_population" = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE)) %>%
  mutate(M.Total_bipoc_population = M.Total_population - M.Total_white_population,
         M.PCT_bipoc_population = 100 - M.PCT_white_population)

M.N.demographic_summary <- (m.unexposed_population) %>%
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise("M.N.Total_population" = sum(Total, na.rm = TRUE),
                   "M.N.Total_black_population" = sum(AfAm, na.rm = TRUE),
                   "M.N.Total_hispanic_population" = sum(Latinx, na.rm = TRUE),
                   "M.N.Total_white_population" = sum(White, na.rm = TRUE),
                   "M.N.PCT_black_population" = (100 * sum(AfAm, na.rm = TRUE) / sum(Total, na.rm = TRUE)),
                   "M.N.PCT_hispanic_population" = 100 * sum(Latinx, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "M.N.PCT_white_population" = 100 * sum(White, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "M.N.Total_below5_population" = sum(b5, na.rm = TRUE),
                   "M.N.Total_above65_population" = sum(a65, na.rm = TRUE),
                   "M.N.PCT_below5_population" = 100* sum(b5, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "M.N.PCT_above65_population" = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE)) %>%
  mutate(M.N.Total_bipoc_population = M.N.Total_population - M.N.Total_white_population,
         M.N.PCT_bipoc_population = 100 - M.N.PCT_white_population)

E.demographic_summary <- (e.exposed_population) %>%
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise("E.Total_population" = sum(Total, na.rm = TRUE),
                   "E.Total_black_population" = sum(AfAm, na.rm = TRUE),
                   "E.Total_hispanic_population" = sum(Latinx, na.rm = TRUE),
                   "E.Total_white_population" = sum(White, na.rm = TRUE),
                   "E.PCT_black_population" = (100 * sum(AfAm, na.rm = TRUE) / sum(Total, na.rm = TRUE)),
                   "E.PCT_hispanic_population" = 100 * sum(Latinx, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "E.PCT_white_population" = 100 * sum(White, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "E.Total_below5_population" = sum(b5, na.rm = TRUE),
                   "E.Total_above65_population" = sum(a65, na.rm = TRUE),
                   "E.PCT_below5_population" = 100* sum(b5, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "E.PCT_above65_population" = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE)) %>%
  mutate(E.Total_bipoc_population = E.Total_population - E.Total_white_population,
         E.PCT_bipoc_population = 100 - E.PCT_white_population)

E.N.demographic_summary <- (e.unexposed_population) %>%
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise("E.N.Total_population" = sum(Total, na.rm = TRUE),
                   "E.N.Total_black_population" = sum(AfAm, na.rm = TRUE),
                   "E.N.Total_hispanic_population" = sum(Latinx, na.rm = TRUE),
                   "E.N.Total_white_population" = sum(White, na.rm = TRUE),
                   "E.N.PCT_black_population" = (100 * sum(AfAm, na.rm = TRUE) / sum(Total, na.rm = TRUE)),
                   "E.N.PCT_hispanic_population" = 100 * sum(Latinx, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "E.N.PCT_white_population" = 100 * sum(White, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "E.N.Total_below5_population" = sum(b5, na.rm = TRUE),
                   "E.N.Total_above65_population" = sum(a65, na.rm = TRUE),
                   "E.N.PCT_below5_population" = 100* sum(b5, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   "E.N.PCT_above65_population" = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE)) %>%
  mutate(E.N.Total_bipoc_population = E.N.Total_population - E.N.Total_white_population,
         E.N.PCT_bipoc_population = 100 - E.N.PCT_white_population)

Flood_summary_all <- left_join(M.demographic_summary, M.N.demographic_summary) %>%
  left_join(E.demographic_summary) %>%
  left_join(E.N.demographic_summary)

Flood_summary_all[is.na(Flood_summary_all)] <- 0

final_table <- left_join(final_table, Flood_summary_all, by = c("Geography" = "boro_cd")) %>%
  mutate(M.PCT_Total_population = 100 * (M.Total_population / Total_population),
         E.PCT_Total_population = 100 * (E.Total_population / Total_population))

## now we move to tax lot data to assess exposure per land use type

tax_lots_flooding <- st_read("data/2_intermediate/tax_lots_CD_flood.shp") %>%
  st_drop_geometry() %>%
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))
  
tax_lots_flooding_summary <- tax_lots_flooding %>%
  group_by(boro_cd) %>%
  summarize(Total_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"),
            Total_basement_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES == "Basement in residential lot"),
            Total_mixed_comres_lots = sum(LUseCat %in% "Mixed residential and commercial"),
            Total_commercial_lots = sum(LUseCat %in% "Commercial"),
            Total_industrial_lots = sum(LUseCat %in% "Industrial & manufacturing"),
            M.Total_residential_lots_0 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= 0),
            M.Total_residential_lots_5 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= 5),
            M.Total_residential_lots_15 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= 15),
            M.Total_residential_lots_30 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= 30),
            M.Total_residential_lots_50 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= 50),
            M.Total_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= exposure_criteria_meters),
            M.Total_basement_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES == "Basement in residential lot"& m_d_f <= exposure_criteria_meters),
            M.Total_mixed_comres_lots = sum(LUseCat %in% "Mixed residential and commercial"& m_d_f <= exposure_criteria_meters),
            M.Total_commercial_lots = sum(LUseCat %in% "Commercial"& m_d_f <= exposure_criteria_meters),
            M.Total_industrial_lots = sum(LUseCat %in% "Industrial & manufacturing"& m_d_f <= exposure_criteria_meters),
            E.Total_residential_lots_0 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= 0),
            E.Total_residential_lots_5 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= 5),
            E.Total_residential_lots_15 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= 15),
            E.Total_residential_lots_30 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= 30),
            E.Total_residential_lots_50 = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= 50),
            E.Total_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= exposure_criteria_meters),
            E.Total_basement_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES == "Basement in residential lot"& e_d_f <= exposure_criteria_meters),
            E.Total_mixed_comres_lots = sum(LUseCat %in% "Mixed residential and commercial"& e_d_f <= exposure_criteria_meters),
            E.Total_commercial_lots = sum(LUseCat %in% "Commercial"& e_d_f <= exposure_criteria_meters),
            E.Total_industrial_lots = sum(LUseCat %in% "Industrial & manufacturing"& e_d_f <= exposure_criteria_meters)) %>%
  mutate(M.PCT_residential_lots = 100 * M.Total_residential_lots / Total_residential_lots,
         M.PCT_basement_residential_lots = 100 * M.Total_basement_residential_lots / Total_basement_residential_lots,
         M.PCT_mixed_comres_lots = 100 * M.Total_mixed_comres_lots / Total_mixed_comres_lots,
         M.PCT_commercial_lots = 100 * M.Total_commercial_lots / Total_commercial_lots,
         M.PCT_industrial_lots = 100 * M.Total_industrial_lots / Total_industrial_lots,
         E.PCT_residential_lots = 100 * E.Total_residential_lots / Total_residential_lots,
         E.PCT_basement_residential_lots = 100 * E.Total_basement_residential_lots / Total_basement_residential_lots,
         E.PCT_mixed_comres_lots = 100 * E.Total_mixed_comres_lots / Total_mixed_comres_lots,
         E.PCT_commercial_lots = 100 * E.Total_commercial_lots / Total_commercial_lots,
         E.PCT_industrial_lots = 100 * E.Total_industrial_lots / Total_industrial_lots)

final_table <- left_join(final_table, tax_lots_flooding_summary, by = c("Geography" = "boro_cd"))

## Buildings exposure is added to generate data requested by reviewers to illustrate sensitivity of results to different geometry types.

buildings_flooding <- st_read("data/2_intermediate/buildings_CD_flood.shp") %>%
  st_drop_geometry() %>%
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

## we summarize the total number of buildings exposed to each flooding scenario under different distance thresholds (0 meters, 5, 15, 30 and 50)

buildings_flooding_summary <- buildings_flooding %>% 
  group_by(boro_cd) %>% 
  summarize(Total_buildings = n(),
            M.Total_buildings_0 = sum(m_d_f <= 0),
            M.Total_buildings_5 = sum(m_d_f <= 5),
            M.Total_buildings_15 = sum(m_d_f <= 15),
            M.Total_buildings_30 = sum(m_d_f <= 30),
            M.Total_buildings_50 = sum(m_d_f <= 50),
            E.Total_buildings_0 = sum(e_d_f <= 0),
            E.Total_buildings_5 = sum(e_d_f <= 5),
            E.Total_buildings_15 = sum(e_d_f <= 15),
            E.Total_buildings_30 = sum(e_d_f <= 30),
            E.Total_buildings_50 = sum(e_d_f <= 50)) %>% 
  mutate(M.PCT_buildings_0 = 100 * M.Total_buildings_0 / Total_buildings,
         M.PCT_buildings_5 = 100 * M.Total_buildings_5 / Total_buildings,
         M.PCT_buildings_15 = 100 * M.Total_buildings_15 / Total_buildings,
         M.PCT_buildings_30 = 100 * M.Total_buildings_30 / Total_buildings,
         M.PCT_buildings_50 = 100 * M.Total_buildings_50 / Total_buildings,
         E.PCT_buildings_0 = 100 * E.Total_buildings_0 / Total_buildings,
         E.PCT_buildings_5 = 100 * E.Total_buildings_5 / Total_buildings,
         E.PCT_buildings_15 = 100 * E.Total_buildings_15 / Total_buildings,
         E.PCT_buildings_30 = 100 * E.Total_buildings_30 / Total_buildings,
         E.PCT_buildings_50 = 100 * E.Total_buildings_50 / Total_buildings)

final_table <- left_join(final_table, buildings_flooding_summary, by = c("Geography" = "boro_cd"))

## Critical infrastructures and services 

facilities_data_points <- st_read("data/2_intermediate/facilities_20210811_flooding.shp") %>% 
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

facility_categories <- unique(facilities_data_points$CatFac)
facility_subgroups <- unique(facilities_data_points$FACSUBGRP)

## the loop below writes the stats of each of the 4 facility categories (education, health, welfare, and public safety) for each CD
## stats include the total number for each CD, and exposure metrics for each scenario

for(facility_cat in facility_categories){
  
  print(paste0("   ", facility_cat))
  
  sliced_catfac <- facilities_data_points[facilities_data_points$CatFac == facility_cat,]
  
  for(comdist in final_table$Geography){
    
    sliced_catfac_CD <- sliced_catfac[sliced_catfac$boro_cd == comdist,]
    
    total_nr_facility <- nrow(sliced_catfac_CD)
    M.Flooded_nr_facility <-nrow(sliced_catfac_CD[sliced_catfac_CD$m_d_f <= exposure_criteria_meters,])
    E.Flooded_nr_facility <-nrow(sliced_catfac_CD[sliced_catfac_CD$e_d_f <= exposure_criteria_meters,])
    M.Flooded_PCT_facility <- 100 * M.Flooded_nr_facility  / total_nr_facility
    E.Flooded_PCT_facility <- 100 * E.Flooded_nr_facility  / total_nr_facility
    
    final_table[final_table$Geography == comdist, paste0("Total_", facility_cat, "_facilities")] <- total_nr_facility
    
    final_table[final_table$Geography == comdist, paste0("M.Total_", facility_cat, "_facilities")] <- M.Flooded_nr_facility
    final_table[final_table$Geography == comdist, paste0("E.Total_", facility_cat, "_facilities")] <- E.Flooded_nr_facility
    
    final_table[final_table$Geography == comdist, paste0("M.PCT_", facility_cat, "_facilities")] <- M.Flooded_PCT_facility
    final_table[final_table$Geography == comdist, paste0("E.PCT_", facility_cat, "_facilities")] <- E.Flooded_PCT_facility
    
    rm(sliced_catfac_CD, M.Flooded_nr_facility, E.Flooded_nr_facility, M.Flooded_PCT_facility, E.Flooded_PCT_facility)
    
  }
  
  rm(facility_cat, comdist, sliced_catfac)
  
}

## we can also write a csv table with the full disaggregated stats, considering all the subcategories of facilities within each broad theme

facilities_summary_citywide <- data.frame(SUBGRP = facility_subgroups, "Total" = 0, "M.Total" = 0, "E.Total" = 0, "M.PCT" =0, "E.PCT"=0)

for(facility_subgrp in facility_subgroups){
  
  print(paste0("   ", facility_subgrp))
  
  sliced_facsbgrp <- facilities_data_points[facilities_data_points$FACSUBGRP == facility_subgrp,]
  
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "Total"] <- nrow(sliced_facsbgrp)
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "M.Total"] <- nrow(sliced_facsbgrp[sliced_facsbgrp$m_d_f <= exposure_criteria_meters,])
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "E.Total"] <- nrow(sliced_facsbgrp[sliced_facsbgrp$e_d_f <= exposure_criteria_meters,])
  
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "M.PCT"] <- 100 * nrow(sliced_facsbgrp[sliced_facsbgrp$m_d_f <= exposure_criteria_meters,]) / nrow(sliced_facsbgrp)
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "E.PCT"] <- 100 * nrow(sliced_facsbgrp[sliced_facsbgrp$e_d_f <= exposure_criteria_meters,]) / nrow(sliced_facsbgrp)
  
  for(comdist in final_table$Geography){
    
    sliced_facsbgrp_CD <- sliced_facsbgrp[sliced_facsbgrp$boro_cd == comdist,]
    
    total_nr_subgrp <- nrow(sliced_facsbgrp_CD)
    M.Flooded_nr_subgrp <-nrow(sliced_facsbgrp_CD[sliced_facsbgrp_CD$m_d_f <= exposure_criteria_meters,])
    E.Flooded_nr_subgrp <-nrow(sliced_facsbgrp_CD[sliced_facsbgrp_CD$e_d_f <= exposure_criteria_meters,])
    M.Flooded_PCT_facility <- 100 * M.Flooded_nr_subgrp  / total_nr_subgrp
    E.Flooded_PCT_facility <- 100 * E.Flooded_nr_subgrp  / total_nr_subgrp
    
    final_table[final_table$Geography == comdist, paste0("Total_", facility_subgrp, "_facilities")] <- total_nr_subgrp
    
    final_table[final_table$Geography == comdist, paste0("M.Total_", facility_subgrp, "_facilities")] <- M.Flooded_nr_subgrp
    final_table[final_table$Geography == comdist, paste0("E.Total_", facility_subgrp, " facilities")] <- E.Flooded_nr_subgrp
    
    final_table[final_table$Geography == comdist, paste0("M.PCT_", facility_subgrp, "_facilities")] <- M.Flooded_PCT_facility
    final_table[final_table$Geography == comdist, paste0("E.PCT_", facility_subgrp, "_facilities")] <- E.Flooded_PCT_facility
    
  }
  
}

write.csv(facilities_summary_citywide,
          row.names = FALSE,
          "data/3_output/stormwater_analysis_final_database_facility_subgroups_CITYWIDE.csv")

## now we include exposure indicators for transportation
## bus stops and subway entrances are selected using distance criteria, as it was measured already in the sub-script.
## roads and bus routes, on the other side, are assessed here to generate a % of total road area that floods in each CD under each flooding scenario.

bus_stops_summary <- st_read("data/2_intermediate/bus_stops_flooding.shp") %>%
  st_drop_geometry() %>%
  group_by(boro_cd) %>%
  summarize(Total_bus_stops = n(),
            M.Total_bus_stops = sum(m_d_f <= exposure_criteria_meters),
            E.Total_bus_stops = sum(e_d_f <= exposure_criteria_meters)) %>%
  mutate(M.PCT_bus_stops = 100 * M.Total_bus_stops / Total_bus_stops,
         E.PCT_bus_stops = 100 * E.Total_bus_stops / Total_bus_stops)
  
subway_entrances_summary <- st_read("data/2_intermediate/subway_entrances_flooding.shp") %>%
  st_drop_geometry() %>%
  group_by(boro_cd) %>%
  summarize(Total_subway_stops = n(),
            M.Total_subway_stops = sum(m_d_f <= exposure_criteria_meters),
            E.Total_subway_stops = sum(e_d_f <= exposure_criteria_meters)) %>%
  mutate(M.PCT_subway_stops = 100 * M.Total_subway_stops / Total_subway_stops,
         E.PCT_subway_stops = 100 * E.Total_subway_stops / Total_subway_stops)

final_table <- final_table %>%
  left_join(bus_stops_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(subway_entrances_summary, by = c("Geography" = "boro_cd"))

## for bus routes, we load them here

bus_routes <- st_read("data/1_raw/bus_routes_nyc_nov2020.shp") %>% 
  st_transform(UTM_18N_meter) %>%
  st_intersection(CD)

## the loop below will iterate through each CD, collect the bus routes that cut through it, and assess the number and % of bus routes that intersect with flooding

for(comdist in unique(bus_routes$boro_cd)){
  
  print(comdist)
  
  cd_routes <- filter(bus_routes, boro_cd == comdist)
  
  M.Flooded_routes <- st_intersects(cd_routes, moderate_flooding)
  E.Flooded_routes <- st_intersects(cd_routes, extreme_flooding)
  
  total_nr_routes <- nrow(cd_routes)
  
  M.Flooded_nr_routes <- sum(lengths(M.Flooded_routes) > 0)
  E.Flooded_nr_routes <- sum(lengths(E.Flooded_routes) > 0)
  
  M.Flooded_PCT_routes <- 100 * M.Flooded_nr_routes / total_nr_routes
  E.Flooded_PCT_routes <- 100 * E.Flooded_nr_routes / total_nr_routes
  
  final_table[final_table$Geography == comdist, "Total_bus_routes"] <- total_nr_routes
  
  final_table[final_table$Geography == comdist, "M.Total_bus_routes"] <- M.Flooded_nr_routes
  final_table[final_table$Geography == comdist, "E.Total_bus_routes"] <- E.Flooded_nr_routes
  
  final_table[final_table$Geography == comdist, "M.PCT_bus_routes"] <- round(M.Flooded_PCT_routes, 1)
  final_table[final_table$Geography == comdist, "E.PCT_bus_routes"] <- round(E.Flooded_PCT_routes, 1)
  
  }

## % roads flooded

roads <- st_read("data/1_raw/geo_export_4443d165-7281-4b57-a1a1-f651c232b7ab.shp") %>%
  st_transform(UTM_18N_meter) %>%
  filter(rw_type %nin% c(2,3,9, 14, 4)) # remove road types that are not walkable / drivable. IM ALSO REMOVING 3 (bridges) because flooding will show the underpass flooding  and 14 (ferry routes) and 4 (tunnels) because flooding is in the surface

## road data is complex (lots of entities with complex geometries).
## to make the process easier, the loop below will iterate through each CD to calculate total road area, 
## and the amount / % of road area that overlaps with flooding under each scenario

for (comdist in CD$boro_cd){
  print(comdist)
  
  AOI <- CD[CD$boro_cd == comdist,]
  
  roads_cd <- roads[AOI, ]
  
  roads_cd.buf <- st_buffer(roads_cd, (roads_cd$st_width / (2*3.281)), endCapStyle = "FLAT") %>% ## the dataset includes a width field that we can use to transfer the data from a line to a road's foorprint
    st_make_valid()
  
  roads_cd.buf.int <- st_intersection(roads_cd.buf, AOI)
  
  roads_cd.buf.int.union <- roads_cd.buf.int %>% st_union() %>% st_make_valid() %>% st_cast("POLYGON")
  
  final_table[final_table$Geography == comdist, "Total_road_area"] <- sum(st_area(roads_cd.buf.int.union))
  
  moderate_flooding.int.road <- st_intersection(moderate_flooding, roads_cd.buf.int.union)
  
  final_table[final_table$Geography == comdist, "M.Total_road_area"] <- sum(st_area(moderate_flooding.int.road))
  final_table[final_table$Geography == comdist, "M.PCT_road_area"] <- 100 * sum(st_area(moderate_flooding.int.road)) / sum(st_area(roads_cd.buf.int.union))
  
  extreme_flooding.int.road <-  st_intersection(extreme_flooding, roads_cd.buf.int.union)
  
  final_table[final_table$Geography == comdist, "E.Total_road_area"] <- sum(st_area(extreme_flooding.int.road))
  final_table[final_table$Geography == comdist, "E.PCT_road_area"] <- 100 * sum(st_area(extreme_flooding.int.road)) / sum(st_area(roads_cd.buf.int.union))
  
}


## the final piece of the puzzle is social vulnerability. 
## we already compiled age and race indicators at the census block level, 
## so now we move on to census block group level ACS data
## the main challenge here will be to aggregate the data to the CD level. Because of the estimates' standard errors, we have to apply error propagation considerations

SOVI_data.CD <- read_csv("data/2_intermediate/NYC_ACS_2018.csv") %>% 
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595)) %>%
  mutate(boro_cd = as.numeric(boro_cd))

## We will work with each CD's populations separately (total population, exposed, and non-exposed). 
## accounting that exposed / unexposed tables are generated for each scenario, we will be dealing with 5 aggregations in total
## when aggregating census block groups to each CD, we may encounter several CBGs with 0 as the estimated value (e.g. 0 people living in poverty).
## theUS Census Bureau recommends accounting for the MoE of 0-value estimates only once when performing an aggregation, in order to avoid a super inflated MoE value. See slide 52 here - https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/20180418_MOE_Webinar_Transcript.pdf
## we apply the custom function "second_zero_NAs()" to remove the Standard Error of all 0-values except for the first that occurs in each column


ACS_demographic_summary <- (SOVI_data.CD) %>% second_zero_NAs()                         # total population in each CD

M.ACS_demographic_summary <- (filter(SOVI_data.CD, m_d_f == 0)) %>% second_zero_NAs()   # Moderate scenario - exposed
M.N.ACS_demographic_summary <- (filter(SOVI_data.CD, m_d_f > 0)) %>% second_zero_NAs()  # Moderate scenario - not exposed

E.ACS_demographic_summary <- (filter(SOVI_data.CD, e_d_f == 0)) %>% second_zero_NAs()
E.N.ACS_demographic_summary <- (filter(SOVI_data.CD, e_d_f > 0)) %>% second_zero_NAs()

## now one by one we will aggregate and recalculate estimated social vulnerability indicators for each CD
## first for each CD's total population

ACS_demographic_summary <- (ACS_demographic_summary) %>%
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(Belpov_e = sum(Belpov_e, na.rm = TRUE),
                   Belpov_s = (sum(Belpov_s^2, na.rm = TRUE))^0.5,                      # square root of the sum of the squared standard errors is used to calculate the new standard error of summed estimates
                   Total_Belpov_e = sum(Total_Belpov_e, na.rm = TRUE),
                   Total_Belpov_s = (sum(Total_Belpov_s^2, na.rm = TRUE))^0.5,

                   Total_HH_Income_e = sum(Total_HH_Income_e, na.rm = TRUE),
                   Total_HH_Income_s = (sum(Total_HH_Income_s^2, na.rm = TRUE))^0.5,
                   HH_Income_Bel75K_e = sum(HH_Income_Bel75K_e, na.rm = TRUE),
                   HH_Income_Bel75K_s = (sum(HH_Income_Bel75K_s^2, na.rm = TRUE))^0.5,
                   
                   Total_HH_Dis_e = sum(Total_HH_Dis_e, na.rm = TRUE),
                   Total_HH_Dis_s = (sum(Total_HH_Dis_s^2, na.rm = TRUE))^0.5,
                   HH_Disability_e = sum(HH_Disability_e, na.rm = TRUE),
                   HH_Disability_s = (sum(HH_Disability_s^2, na.rm = TRUE))^0.5,
                   
                   Total_Rent_HH_e = sum(Total_Rent_HH_e, na.rm = TRUE),
                   Total_Rent_HH_s = (sum(Total_Rent_HH_s^2, na.rm = TRUE))^0.5,
                   RB_HH_e = sum(RB_HH_e, na.rm = TRUE),
                   RB_HH_s = (sum(RB_HH_s^2, na.rm = TRUE))^0.5,
                   
                   Total_Owned_HH_e = sum(Total_Owned_HH_e, na.rm = TRUE),
                   Total_Owned_HH_s = (sum(Total_Owned_HH_s^2, na.rm = TRUE))^0.5,
                   CB_HH_e = sum(CB_HH_e, na.rm = TRUE),
                   CB_HH_s = (sum(CB_HH_s^2, na.rm = TRUE))^0.5
                   ) %>%
  mutate(PCT_Belpov_e = 100*(Belpov_e / Total_Belpov_e),
         PCT_Belpov_s = 100*ifelse((Belpov_e / Total_Belpov_e) != 100,
                             ifelse((Belpov_s^2 - ((Belpov_e / Total_Belpov_e)^2 * Total_Belpov_s^2)) > 0,
                             (1 / Total_Belpov_e) * ((Belpov_s^2 - ((Belpov_e / Total_Belpov_e)^2 * Total_Belpov_s^2))^0.5),
                             (1 / Total_Belpov_e) * ((Belpov_s^2 + ((Belpov_e / Total_Belpov_e)^2 * Total_Belpov_s^2))^0.5)),
         Belpov_s / Total_Belpov_e),
         
         PCT_HH_Income_Bel75K_e = 100*(HH_Income_Bel75K_e / Total_HH_Income_e),
         PCT_HH_Income_Bel75K_s = 100*ifelse((HH_Income_Bel75K_e / Total_HH_Income_e) != 100,
                                       ifelse((HH_Income_Bel75K_s^2 - ((HH_Income_Bel75K_e / Total_HH_Income_e)^2 * Total_HH_Income_s^2)) > 0,
                                              (1 / Total_HH_Income_e) * (HH_Income_Bel75K_s^2 - ((HH_Income_Bel75K_e / Total_HH_Income_e)^2 * Total_HH_Income_s^2))^0.5,
                                              (1 / Total_HH_Income_e) * (HH_Income_Bel75K_s^2 + ((HH_Income_Bel75K_e / Total_HH_Income_e)^2 * Total_HH_Income_s^2))^0.5),
                                       HH_Income_Bel75K_s / Total_HH_Income_e),
         
         PCT_Disability_e = 100*(HH_Disability_e / Total_HH_Dis_e),
         PCT_Disability_s = 100*ifelse((HH_Disability_e / Total_HH_Dis_e) != 100,
                                 ifelse((HH_Disability_s^2 - ((HH_Disability_e / Total_HH_Dis_e)^2 * Total_HH_Dis_s^2)) > 0,
                                        (1 / Total_HH_Dis_e) * (HH_Disability_s^2 - ((HH_Disability_e / Total_HH_Dis_e)^2 * Total_HH_Dis_s^2))^0.5,
                                        (1 / Total_HH_Dis_e) * (HH_Disability_s^2 + ((HH_Disability_e / Total_HH_Dis_e)^2 * Total_HH_Dis_s^2))^0.5),
                                 HH_Disability_s / Total_HH_Dis_e),
         
         PCT_RB_e = 100*(RB_HH_e / Total_Rent_HH_e),
         PCT_RB_s = 100*ifelse((RB_HH_e / Total_Rent_HH_e) != 100,
                                       ifelse((RB_HH_s^2 - ((RB_HH_e / Total_Rent_HH_e)^2 * Total_Rent_HH_s^2)) > 0,
                                              (1 / Total_Rent_HH_e) * (RB_HH_s^2 - ((RB_HH_e / Total_Rent_HH_e)^2 * Total_Rent_HH_s^2))^0.5,
                                              (1 / Total_Rent_HH_e) * (RB_HH_s^2 + ((RB_HH_e / Total_Rent_HH_e)^2 * Total_Rent_HH_s^2))^0.5),
                                       RB_HH_s / Total_Rent_HH_e),
         
         PCT_CB_e = 100*(CB_HH_e / Total_Owned_HH_e),
         PCT_CB_s = 100*ifelse((CB_HH_e / Total_Owned_HH_e) != 100,
                                       ifelse((CB_HH_s^2 - ((CB_HH_e / Total_Owned_HH_e)^2 * Total_Owned_HH_s^2)) > 0,
                                              (1 / Total_Owned_HH_e) * (CB_HH_s^2 - ((CB_HH_e / Total_Owned_HH_e)^2 * Total_Owned_HH_s^2))^0.5,
                                              (1 / Total_Owned_HH_e) * (CB_HH_s^2 + ((CB_HH_e / Total_Owned_HH_e)^2 * Total_Owned_HH_s^2))^0.5),
                                       CB_HH_s / Total_Owned_HH_e)
         )

## now for each CD's exposed population (moderate scenario)

M.ACS_demographic_summary <- (M.ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(M.Belpov_e = sum(Belpov_e, na.rm = TRUE),
                   M.Belpov_s = (sum(Belpov_s^2, na.rm = TRUE))^0.5,
                   M.Total_Belpov_e = sum(Total_Belpov_e, na.rm = TRUE),
                   M.Total_Belpov_s = (sum(Total_Belpov_s^2, na.rm = TRUE))^0.5,
                   
                   M.Total_HH_Income_e = sum(Total_HH_Income_e, na.rm = TRUE),
                   M.Total_HH_Income_s = (sum(Total_HH_Income_s^2, na.rm = TRUE))^0.5,
                   M.HH_Income_Bel75K_e = sum(HH_Income_Bel75K_e, na.rm = TRUE),
                   M.HH_Income_Bel75K_s = (sum(HH_Income_Bel75K_s^2, na.rm = TRUE))^0.5,
                   
                   M.Total_HH_Dis_e = sum(Total_HH_Dis_e, na.rm = TRUE),
                   M.Total_HH_Dis_s = (sum(Total_HH_Dis_s^2, na.rm = TRUE))^0.5,
                   M.HH_Disability_e = sum(HH_Disability_e, na.rm = TRUE),
                   M.HH_Disability_s = (sum(HH_Disability_s^2, na.rm = TRUE))^0.5,
                   
                   M.Total_Rent_HH_e = sum(Total_Rent_HH_e, na.rm = TRUE),
                   M.Total_Rent_HH_s = (sum(Total_Rent_HH_s^2, na.rm = TRUE))^0.5,
                   M.RB_HH_e = sum(RB_HH_e, na.rm = TRUE),
                   M.RB_HH_s = (sum(RB_HH_s^2, na.rm = TRUE))^0.5,
                   
                   M.Total_Owned_HH_e = sum(Total_Owned_HH_e, na.rm = TRUE),
                   M.Total_Owned_HH_s = (sum(Total_Owned_HH_s^2, na.rm = TRUE))^0.5,
                   M.CB_HH_e = sum(CB_HH_e, na.rm = TRUE),
                   M.CB_HH_s = (sum(CB_HH_s^2, na.rm = TRUE))^0.5
  ) %>%
  mutate(M.PCT_Belpov_e = 100*(M.Belpov_e / M.Total_Belpov_e),
         M.PCT_Belpov_s = 100*ifelse((M.Belpov_e / M.Total_Belpov_e) != 100,
                                   ifelse((M.Belpov_s^2 - ((M.Belpov_e / M.Total_Belpov_e)^2 * M.Total_Belpov_s^2)) > 0,
                                          (1 / M.Total_Belpov_e) * ((M.Belpov_s^2 - ((M.Belpov_e / M.Total_Belpov_e)^2 * M.Total_Belpov_s^2))^0.5),
                                          (1 / M.Total_Belpov_e) * ((M.Belpov_s^2 + ((M.Belpov_e / M.Total_Belpov_e)^2 * M.Total_Belpov_s^2))^0.5)),
                                   M.Belpov_s / M.Total_Belpov_e),
         
         M.PCT_HH_Income_Bel75K_e = 100*(M.HH_Income_Bel75K_e / M.Total_HH_Income_e),
         M.PCT_HH_Income_Bel75K_s = 100*ifelse((M.HH_Income_Bel75K_e / M.Total_HH_Income_e) != 100,
                                             ifelse((M.HH_Income_Bel75K_s^2 - ((M.HH_Income_Bel75K_e / M.Total_HH_Income_e)^2 * M.Total_HH_Income_s^2)) > 0,
                                                    (1 / M.Total_HH_Income_e) * (M.HH_Income_Bel75K_s^2 - ((M.HH_Income_Bel75K_e / M.Total_HH_Income_e)^2 * M.Total_HH_Income_s^2))^0.5,
                                                    (1 / M.Total_HH_Income_e) * (M.HH_Income_Bel75K_s^2 + ((M.HH_Income_Bel75K_e / M.Total_HH_Income_e)^2 * M.Total_HH_Income_s^2))^0.5),
                                             M.HH_Income_Bel75K_s / M.Total_HH_Income_e),
         
         M.PCT_Disability_e = 100*(M.HH_Disability_e / M.Total_HH_Dis_e),
         M.PCT_Disability_s = 100*ifelse((M.HH_Disability_e / M.Total_HH_Dis_e) != 100,
                                       ifelse((M.HH_Disability_s^2 - ((M.HH_Disability_e / M.Total_HH_Dis_e)^2 * M.Total_HH_Dis_s^2)) > 0,
                                              (1 / M.Total_HH_Dis_e) * (M.HH_Disability_s^2 - ((M.HH_Disability_e / M.Total_HH_Dis_e)^2 * M.Total_HH_Dis_s^2))^0.5,
                                              (1 / M.Total_HH_Dis_e) * (M.HH_Disability_s^2 + ((M.HH_Disability_e / M.Total_HH_Dis_e)^2 * M.Total_HH_Dis_s^2))^0.5),
                                       M.HH_Disability_s / M.Total_HH_Dis_e),
         
         M.PCT_RB_e = 100*(M.RB_HH_e / M.Total_Rent_HH_e),
         M.PCT_RB_s = 100*ifelse((M.RB_HH_e / M.Total_Rent_HH_e) != 100,
                               ifelse((M.RB_HH_s^2 - ((M.RB_HH_e / M.Total_Rent_HH_e)^2 * M.Total_Rent_HH_s^2)) > 0,
                                      (1 / M.Total_Rent_HH_e) * (M.RB_HH_s^2 - ((M.RB_HH_e / M.Total_Rent_HH_e)^2 * M.Total_Rent_HH_s^2))^0.5,
                                      (1 / M.Total_Rent_HH_e) * (M.RB_HH_s^2 + ((M.RB_HH_e / M.Total_Rent_HH_e)^2 * M.Total_Rent_HH_s^2))^0.5),
                               M.RB_HH_s / M.Total_Rent_HH_e),
         
         M.PCT_CB_e = 100*(M.CB_HH_e / M.Total_Owned_HH_e),
         M.PCT_CB_s = 100*ifelse((M.CB_HH_e / M.Total_Owned_HH_e) != 100,
                               ifelse((M.CB_HH_s^2 - ((M.CB_HH_e / M.Total_Owned_HH_e)^2 * M.Total_Owned_HH_s^2)) > 0,
                                      (1 / M.Total_Owned_HH_e) * (M.CB_HH_s^2 - ((M.CB_HH_e / M.Total_Owned_HH_e)^2 * M.Total_Owned_HH_s^2))^0.5,
                                      (1 / M.Total_Owned_HH_e) * (M.CB_HH_s^2 + ((M.CB_HH_e / M.Total_Owned_HH_e)^2 * M.Total_Owned_HH_s^2))^0.5),
                               M.CB_HH_s / M.Total_Owned_HH_e)
  )


## now for each CD's not-exposed population (moderate scenario)

M.N.ACS_demographic_summary <- (M.N.ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(M.N.Belpov_e = sum(Belpov_e, na.rm = TRUE),
                   M.N.Belpov_s = (sum(Belpov_s^2, na.rm = TRUE))^0.5,
                   M.N.Total_Belpov_e = sum(Total_Belpov_e, na.rm = TRUE),
                   M.N.Total_Belpov_s = (sum(Total_Belpov_s^2, na.rm = TRUE))^0.5,
                   
                   M.N.Total_HH_Income_e = sum(Total_HH_Income_e, na.rm = TRUE),
                   M.N.Total_HH_Income_s = (sum(Total_HH_Income_s^2, na.rm = TRUE))^0.5,
                   M.N.HH_Income_Bel75K_e = sum(HH_Income_Bel75K_e, na.rm = TRUE),
                   M.N.HH_Income_Bel75K_s = (sum(HH_Income_Bel75K_s^2, na.rm = TRUE))^0.5,
                   
                   M.N.Total_HH_Dis_e = sum(Total_HH_Dis_e, na.rm = TRUE),
                   M.N.Total_HH_Dis_s = (sum(Total_HH_Dis_s^2, na.rm = TRUE))^0.5,
                   M.N.HH_Disability_e = sum(HH_Disability_e, na.rm = TRUE),
                   M.N.HH_Disability_s = (sum(HH_Disability_s^2, na.rm = TRUE))^0.5,
                   
                   M.N.Total_Rent_HH_e = sum(Total_Rent_HH_e, na.rm = TRUE),
                   M.N.Total_Rent_HH_s = (sum(Total_Rent_HH_s^2, na.rm = TRUE))^0.5,
                   M.N.RB_HH_e = sum(RB_HH_e, na.rm = TRUE),
                   M.N.RB_HH_s = (sum(RB_HH_s^2, na.rm = TRUE))^0.5,
                   
                   M.N.Total_Owned_HH_e = sum(Total_Owned_HH_e, na.rm = TRUE),
                   M.N.Total_Owned_HH_s = (sum(Total_Owned_HH_s^2, na.rm = TRUE))^0.5,
                   M.N.CB_HH_e = sum(CB_HH_e, na.rm = TRUE),
                   M.N.CB_HH_s = (sum(CB_HH_s^2, na.rm = TRUE))^0.5
  ) %>%
  mutate(M.N.PCT_Belpov_e = 100*(M.N.Belpov_e / M.N.Total_Belpov_e),
         M.N.PCT_Belpov_s = 100*ifelse((M.N.Belpov_e / M.N.Total_Belpov_e) != 100,
                                     ifelse((M.N.Belpov_s^2 - ((M.N.Belpov_e / M.N.Total_Belpov_e)^2 * M.N.Total_Belpov_s^2)) > 0,
                                            (1 / M.N.Total_Belpov_e) * ((M.N.Belpov_s^2 - ((M.N.Belpov_e / M.N.Total_Belpov_e)^2 * M.N.Total_Belpov_s^2))^0.5),
                                            (1 / M.N.Total_Belpov_e) * ((M.N.Belpov_s^2 + ((M.N.Belpov_e / M.N.Total_Belpov_e)^2 * M.N.Total_Belpov_s^2))^0.5)),
                                     M.N.Belpov_s / M.N.Total_Belpov_e),
         
         M.N.PCT_HH_Income_Bel75K_e = 100*(M.N.HH_Income_Bel75K_e / M.N.Total_HH_Income_e),
         M.N.PCT_HH_Income_Bel75K_s = 100*ifelse((M.N.HH_Income_Bel75K_e / M.N.Total_HH_Income_e) != 100,
                                               ifelse((M.N.HH_Income_Bel75K_s^2 - ((M.N.HH_Income_Bel75K_e / M.N.Total_HH_Income_e)^2 * M.N.Total_HH_Income_s^2)) > 0,
                                                      (1 / M.N.Total_HH_Income_e) * (M.N.HH_Income_Bel75K_s^2 - ((M.N.HH_Income_Bel75K_e / M.N.Total_HH_Income_e)^2 * M.N.Total_HH_Income_s^2))^0.5,
                                                      (1 / M.N.Total_HH_Income_e) * (M.N.HH_Income_Bel75K_s^2 + ((M.N.HH_Income_Bel75K_e / M.N.Total_HH_Income_e)^2 * M.N.Total_HH_Income_s^2))^0.5),
                                               M.N.HH_Income_Bel75K_s / M.N.Total_HH_Income_e),
         
         M.N.PCT_Disability_e = 100*(M.N.HH_Disability_e / M.N.Total_HH_Dis_e),
         M.N.PCT_Disability_s = 100*ifelse((M.N.HH_Disability_e / M.N.Total_HH_Dis_e) != 100,
                                         ifelse((M.N.HH_Disability_s^2 - ((M.N.HH_Disability_e / M.N.Total_HH_Dis_e)^2 * M.N.Total_HH_Dis_s^2)) > 0,
                                                (1 / M.N.Total_HH_Dis_e) * (M.N.HH_Disability_s^2 - ((M.N.HH_Disability_e / M.N.Total_HH_Dis_e)^2 * M.N.Total_HH_Dis_s^2))^0.5,
                                                (1 / M.N.Total_HH_Dis_e) * (M.N.HH_Disability_s^2 + ((M.N.HH_Disability_e / M.N.Total_HH_Dis_e)^2 * M.N.Total_HH_Dis_s^2))^0.5),
                                         M.N.HH_Disability_s / M.N.Total_HH_Dis_e),
         
         M.N.PCT_RB_e = 100*(M.N.RB_HH_e / M.N.Total_Rent_HH_e),
         M.N.PCT_RB_s = 100*ifelse((M.N.RB_HH_e / M.N.Total_Rent_HH_e) != 100,
                                 ifelse((M.N.RB_HH_s^2 - ((M.N.RB_HH_e / M.N.Total_Rent_HH_e)^2 * M.N.Total_Rent_HH_s^2)) > 0,
                                        (1 / M.N.Total_Rent_HH_e) * (M.N.RB_HH_s^2 - ((M.N.RB_HH_e / M.N.Total_Rent_HH_e)^2 * M.N.Total_Rent_HH_s^2))^0.5,
                                        (1 / M.N.Total_Rent_HH_e) * (M.N.RB_HH_s^2 + ((M.N.RB_HH_e / M.N.Total_Rent_HH_e)^2 * M.N.Total_Rent_HH_s^2))^0.5),
                                 M.N.RB_HH_s / M.N.Total_Rent_HH_e),
         
         M.N.PCT_CB_e = 100*(M.N.CB_HH_e / M.N.Total_Owned_HH_e),
         M.N.PCT_CB_s = 100*ifelse((M.N.CB_HH_e / M.N.Total_Owned_HH_e) != 100,
                                 ifelse((M.N.CB_HH_s^2 - ((M.N.CB_HH_e / M.N.Total_Owned_HH_e)^2 * M.N.Total_Owned_HH_s^2)) > 0,
                                        (1 / M.N.Total_Owned_HH_e) * (M.N.CB_HH_s^2 - ((M.N.CB_HH_e / M.N.Total_Owned_HH_e)^2 * M.N.Total_Owned_HH_s^2))^0.5,
                                        (1 / M.N.Total_Owned_HH_e) * (M.N.CB_HH_s^2 + ((M.N.CB_HH_e / M.N.Total_Owned_HH_e)^2 * M.N.Total_Owned_HH_s^2))^0.5),
                                 M.N.CB_HH_s / M.N.Total_Owned_HH_e)
  )

## now for each CD's exposed population (extreme scenario)

E.ACS_demographic_summary <- (E.ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(E.Belpov_e = sum(Belpov_e, na.rm = TRUE),
                   E.Belpov_s = (sum(Belpov_s^2, na.rm = TRUE))^0.5,
                   E.Total_Belpov_e = sum(Total_Belpov_e, na.rm = TRUE),
                   E.Total_Belpov_s = (sum(Total_Belpov_s^2, na.rm = TRUE))^0.5,
                   
                   E.Total_HH_Income_e = sum(Total_HH_Income_e, na.rm = TRUE),
                   E.Total_HH_Income_s = (sum(Total_HH_Income_s^2, na.rm = TRUE))^0.5,
                   E.HH_Income_Bel75K_e = sum(HH_Income_Bel75K_e, na.rm = TRUE),
                   E.HH_Income_Bel75K_s = (sum(HH_Income_Bel75K_s^2, na.rm = TRUE))^0.5,
                   
                   E.Total_HH_Dis_e = sum(Total_HH_Dis_e, na.rm = TRUE),
                   E.Total_HH_Dis_s = (sum(Total_HH_Dis_s^2, na.rm = TRUE))^0.5,
                   E.HH_Disability_e = sum(HH_Disability_e, na.rm = TRUE),
                   E.HH_Disability_s = (sum(HH_Disability_s^2, na.rm = TRUE))^0.5,
                   
                   E.Total_Rent_HH_e = sum(Total_Rent_HH_e, na.rm = TRUE),
                   E.Total_Rent_HH_s = (sum(Total_Rent_HH_s^2, na.rm = TRUE))^0.5,
                   E.RB_HH_e = sum(RB_HH_e, na.rm = TRUE),
                   E.RB_HH_s = (sum(RB_HH_s^2, na.rm = TRUE))^0.5,
                   
                   E.Total_Owned_HH_e = sum(Total_Owned_HH_e, na.rm = TRUE),
                   E.Total_Owned_HH_s = (sum(Total_Owned_HH_s^2, na.rm = TRUE))^0.5,
                   E.CB_HH_e = sum(CB_HH_e, na.rm = TRUE),
                   E.CB_HH_s = (sum(CB_HH_s^2, na.rm = TRUE))^0.5
  ) %>%
  mutate(E.PCT_Belpov_e = 100*(E.Belpov_e / E.Total_Belpov_e),
         E.PCT_Belpov_s = 100*ifelse((E.Belpov_e / E.Total_Belpov_e) != 100,
                                       ifelse((E.Belpov_s^2 - ((E.Belpov_e / E.Total_Belpov_e)^2 * E.Total_Belpov_s^2)) > 0,
                                              (1 / E.Total_Belpov_e) * ((E.Belpov_s^2 - ((E.Belpov_e / E.Total_Belpov_e)^2 * E.Total_Belpov_s^2))^0.5),
                                              (1 / E.Total_Belpov_e) * ((E.Belpov_s^2 + ((E.Belpov_e / E.Total_Belpov_e)^2 * E.Total_Belpov_s^2))^0.5)),
                                       E.Belpov_s / E.Total_Belpov_e),
         
         E.PCT_HH_Income_Bel75K_e = 100*(E.HH_Income_Bel75K_e / E.Total_HH_Income_e),
         E.PCT_HH_Income_Bel75K_s = 100*ifelse((E.HH_Income_Bel75K_e / E.Total_HH_Income_e) != 100,
                                                 ifelse((E.HH_Income_Bel75K_s^2 - ((E.HH_Income_Bel75K_e / E.Total_HH_Income_e)^2 * E.Total_HH_Income_s^2)) > 0,
                                                        (1 / E.Total_HH_Income_e) * (E.HH_Income_Bel75K_s^2 - ((E.HH_Income_Bel75K_e / E.Total_HH_Income_e)^2 * E.Total_HH_Income_s^2))^0.5,
                                                        (1 / E.Total_HH_Income_e) * (E.HH_Income_Bel75K_s^2 + ((E.HH_Income_Bel75K_e / E.Total_HH_Income_e)^2 * E.Total_HH_Income_s^2))^0.5),
                                                 E.HH_Income_Bel75K_s / E.Total_HH_Income_e),
         
         E.PCT_Disability_e = 100*(E.HH_Disability_e / E.Total_HH_Dis_e),
         E.PCT_Disability_s = 100*ifelse((E.HH_Disability_e / E.Total_HH_Dis_e) != 100,
                                           ifelse((E.HH_Disability_s^2 - ((E.HH_Disability_e / E.Total_HH_Dis_e)^2 * E.Total_HH_Dis_s^2)) > 0,
                                                  (1 / E.Total_HH_Dis_e) * (E.HH_Disability_s^2 - ((E.HH_Disability_e / E.Total_HH_Dis_e)^2 * E.Total_HH_Dis_s^2))^0.5,
                                                  (1 / E.Total_HH_Dis_e) * (E.HH_Disability_s^2 + ((E.HH_Disability_e / E.Total_HH_Dis_e)^2 * E.Total_HH_Dis_s^2))^0.5),
                                           E.HH_Disability_s / E.Total_HH_Dis_e),
         
         E.PCT_RB_e = 100*(E.RB_HH_e / E.Total_Rent_HH_e),
         E.PCT_RB_s = 100*ifelse((E.RB_HH_e / E.Total_Rent_HH_e) != 100,
                                   ifelse((E.RB_HH_s^2 - ((E.RB_HH_e / E.Total_Rent_HH_e)^2 * E.Total_Rent_HH_s^2)) > 0,
                                          (1 / E.Total_Rent_HH_e) * (E.RB_HH_s^2 - ((E.RB_HH_e / E.Total_Rent_HH_e)^2 * E.Total_Rent_HH_s^2))^0.5,
                                          (1 / E.Total_Rent_HH_e) * (E.RB_HH_s^2 + ((E.RB_HH_e / E.Total_Rent_HH_e)^2 * E.Total_Rent_HH_s^2))^0.5),
                                   E.RB_HH_s / E.Total_Rent_HH_e),
         
         E.PCT_CB_e = 100*(E.CB_HH_e / E.Total_Owned_HH_e),
         E.PCT_CB_s = 100*ifelse((E.CB_HH_e / E.Total_Owned_HH_e) != 100,
                                   ifelse((E.CB_HH_s^2 - ((E.CB_HH_e / E.Total_Owned_HH_e)^2 * E.Total_Owned_HH_s^2)) > 0,
                                          (1 / E.Total_Owned_HH_e) * (E.CB_HH_s^2 - ((E.CB_HH_e / E.Total_Owned_HH_e)^2 * E.Total_Owned_HH_s^2))^0.5,
                                          (1 / E.Total_Owned_HH_e) * (E.CB_HH_s^2 + ((E.CB_HH_e / E.Total_Owned_HH_e)^2 * E.Total_Owned_HH_s^2))^0.5),
                                   E.CB_HH_s / E.Total_Owned_HH_e)
  )

## now for each CD's not-exposed population (extreme scenario)

E.N.ACS_demographic_summary <- (E.N.ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(E.N.Belpov_e = sum(Belpov_e, na.rm = TRUE),
                   E.N.Belpov_s = (sum(Belpov_s^2, na.rm = TRUE))^0.5,
                   E.N.Total_Belpov_e = sum(Total_Belpov_e, na.rm = TRUE),
                   E.N.Total_Belpov_s = (sum(Total_Belpov_s^2, na.rm = TRUE))^0.5,
                   
                   E.N.Total_HH_Income_e = sum(Total_HH_Income_e, na.rm = TRUE),
                   E.N.Total_HH_Income_s = (sum(Total_HH_Income_s^2, na.rm = TRUE))^0.5,
                   E.N.HH_Income_Bel75K_e = sum(HH_Income_Bel75K_e, na.rm = TRUE),
                   E.N.HH_Income_Bel75K_s = (sum(HH_Income_Bel75K_s^2, na.rm = TRUE))^0.5,
                   
                   E.N.Total_HH_Dis_e = sum(Total_HH_Dis_e, na.rm = TRUE),
                   E.N.Total_HH_Dis_s = (sum(Total_HH_Dis_s^2, na.rm = TRUE))^0.5,
                   E.N.HH_Disability_e = sum(HH_Disability_e, na.rm = TRUE),
                   E.N.HH_Disability_s = (sum(HH_Disability_s^2, na.rm = TRUE))^0.5,
                   
                   E.N.Total_Rent_HH_e = sum(Total_Rent_HH_e, na.rm = TRUE),
                   E.N.Total_Rent_HH_s = (sum(Total_Rent_HH_s^2, na.rm = TRUE))^0.5,
                   E.N.RB_HH_e = sum(RB_HH_e, na.rm = TRUE),
                   E.N.RB_HH_s = (sum(RB_HH_s^2, na.rm = TRUE))^0.5,
                   
                   E.N.Total_Owned_HH_e = sum(Total_Owned_HH_e, na.rm = TRUE),
                   E.N.Total_Owned_HH_s = (sum(Total_Owned_HH_s^2, na.rm = TRUE))^0.5,
                   E.N.CB_HH_e = sum(CB_HH_e, na.rm = TRUE),
                   E.N.CB_HH_s = (sum(CB_HH_s^2, na.rm = TRUE))^0.5
  ) %>%
  mutate(E.N.PCT_Belpov_e = 100*(E.N.Belpov_e / E.N.Total_Belpov_e),
         E.N.PCT_Belpov_s = 100*ifelse((E.N.Belpov_e / E.N.Total_Belpov_e) != 100,
                                       ifelse((E.N.Belpov_s^2 - ((E.N.Belpov_e / E.N.Total_Belpov_e)^2 * E.N.Total_Belpov_s^2)) > 0,
                                              (1 / E.N.Total_Belpov_e) * ((E.N.Belpov_s^2 - ((E.N.Belpov_e / E.N.Total_Belpov_e)^2 * E.N.Total_Belpov_s^2))^0.5),
                                              (1 / E.N.Total_Belpov_e) * ((E.N.Belpov_s^2 + ((E.N.Belpov_e / E.N.Total_Belpov_e)^2 * E.N.Total_Belpov_s^2))^0.5)),
                                       E.N.Belpov_s / E.N.Total_Belpov_e),
         
         E.N.PCT_HH_Income_Bel75K_e = 100*(E.N.HH_Income_Bel75K_e / E.N.Total_HH_Income_e),
         E.N.PCT_HH_Income_Bel75K_s = 100*ifelse((E.N.HH_Income_Bel75K_e / E.N.Total_HH_Income_e) != 100,
                                                 ifelse((E.N.HH_Income_Bel75K_s^2 - ((E.N.HH_Income_Bel75K_e / E.N.Total_HH_Income_e)^2 * E.N.Total_HH_Income_s^2)) > 0,
                                                        (1 / E.N.Total_HH_Income_e) * (E.N.HH_Income_Bel75K_s^2 - ((E.N.HH_Income_Bel75K_e / E.N.Total_HH_Income_e)^2 * E.N.Total_HH_Income_s^2))^0.5,
                                                        (1 / E.N.Total_HH_Income_e) * (E.N.HH_Income_Bel75K_s^2 + ((E.N.HH_Income_Bel75K_e / E.N.Total_HH_Income_e)^2 * E.N.Total_HH_Income_s^2))^0.5),
                                                 E.N.HH_Income_Bel75K_s / E.N.Total_HH_Income_e),
         
         E.N.PCT_Disability_e = 100*(E.N.HH_Disability_e / E.N.Total_HH_Dis_e),
         E.N.PCT_Disability_s = 100*ifelse((E.N.HH_Disability_e / E.N.Total_HH_Dis_e) != 100,
                                           ifelse((E.N.HH_Disability_s^2 - ((E.N.HH_Disability_e / E.N.Total_HH_Dis_e)^2 * E.N.Total_HH_Dis_s^2)) > 0,
                                                  (1 / E.N.Total_HH_Dis_e) * (E.N.HH_Disability_s^2 - ((E.N.HH_Disability_e / E.N.Total_HH_Dis_e)^2 * E.N.Total_HH_Dis_s^2))^0.5,
                                                  (1 / E.N.Total_HH_Dis_e) * (E.N.HH_Disability_s^2 + ((E.N.HH_Disability_e / E.N.Total_HH_Dis_e)^2 * E.N.Total_HH_Dis_s^2))^0.5),
                                           E.N.HH_Disability_s / E.N.Total_HH_Dis_e),
         
         E.N.PCT_RB_e = 100*(E.N.RB_HH_e / E.N.Total_Rent_HH_e),
         E.N.PCT_RB_s = 100*ifelse((E.N.RB_HH_e / E.N.Total_Rent_HH_e) != 100,
                                   ifelse((E.N.RB_HH_s^2 - ((E.N.RB_HH_e / E.N.Total_Rent_HH_e)^2 * E.N.Total_Rent_HH_s^2)) > 0,
                                          (1 / E.N.Total_Rent_HH_e) * (E.N.RB_HH_s^2 - ((E.N.RB_HH_e / E.N.Total_Rent_HH_e)^2 * E.N.Total_Rent_HH_s^2))^0.5,
                                          (1 / E.N.Total_Rent_HH_e) * (E.N.RB_HH_s^2 + ((E.N.RB_HH_e / E.N.Total_Rent_HH_e)^2 * E.N.Total_Rent_HH_s^2))^0.5),
                                   E.N.RB_HH_s / E.N.Total_Rent_HH_e),
         
         E.N.PCT_CB_e = 100*(E.N.CB_HH_e / E.N.Total_Owned_HH_e),
         E.N.PCT_CB_s = 100*ifelse((E.N.CB_HH_e / E.N.Total_Owned_HH_e) != 100,
                                   ifelse((E.N.CB_HH_s^2 - ((E.N.CB_HH_e / E.N.Total_Owned_HH_e)^2 * E.N.Total_Owned_HH_s^2)) > 0,
                                          (1 / E.N.Total_Owned_HH_e) * (E.N.CB_HH_s^2 - ((E.N.CB_HH_e / E.N.Total_Owned_HH_e)^2 * E.N.Total_Owned_HH_s^2))^0.5,
                                          (1 / E.N.Total_Owned_HH_e) * (E.N.CB_HH_s^2 + ((E.N.CB_HH_e / E.N.Total_Owned_HH_e)^2 * E.N.Total_Owned_HH_s^2))^0.5),
                                   E.N.CB_HH_s / E.N.Total_Owned_HH_e)
  )


## now we join all the SV data tables to the database 

final_table <- final_table %>%
  left_join(ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>% 
  left_join(M.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(M.N.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(E.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(E.N.ACS_demographic_summary, by = c("Geography" = "boro_cd"))

## Now that the data is ready, it would be nice to have an additional row capturing the statistics for the entire city (all CDs aggregated) 
## In preparation for that, we begin with aggregating indicators and data linked to ACS - sv data in order to keep track of recalculated standard errors
## the aggregation of all the ACS data takes lots of code rows due to hard-coding the error propagation of standard errors and doign the same operation 5 times (once for each group - total population, moderate exposed, moderate unexposed, extreme exposed, extreme unexposed)
## we thus source the code in a separate file to declutter a bit

source("src/NYCF_citywide_ACS_aggregation.R")

## Now we can create a list with ALL the variables that will be added to a final row in the final table

NYC_row_data <- list("NYC", 
                  sum_NONA(final_table[,"Total_area"]),
                  sum_NONA(final_table[,"Total_road_area"]),
                  sum_NONA(final_table[,"M.Total_road_area"]),
                  100 * sum_NONA(final_table[,"M.Total_road_area"] / sum_NONA(final_table[,"Total_road_area"])),
                  sum_NONA(final_table[,"E.Total_road_area"]),
                  100 * sum_NONA(final_table[,"E.Total_road_area"] / sum_NONA(final_table[,"Total_road_area"])),
                  sum_NONA(final_table[,"M.Total_area"]),
                  sum_NONA(final_table[,"E.Total_area"]),
                  100 * sum_NONA(final_table[,"M.Total_area"]) / sum_NONA(final_table[,"Total_area"]),
                  100 * sum_NONA(final_table[,"E.Total_area"]) / sum_NONA(final_table[,"Total_area"]),

                  
                  sum_NONA(final_table[,"Total_population"]), 
                  sum_NONA(final_table[,"Total_black_population"]), 
                  sum_NONA(final_table[,"Total_hispanic_population"]),
                  sum_NONA(final_table[,"Total_white_population"]),
                  100 * sum_NONA(final_table[,"Total_black_population"]) / sum_NONA(final_table[,"Total_population"]),
                  100 * sum_NONA(final_table[,"Total_hispanic_population"]) / sum_NONA(final_table[,"Total_population"]),
                  100 * sum_NONA(final_table[,"Total_white_population"]) / sum_NONA(final_table[,"Total_population"]),
                  sum_NONA(final_table[,"Total_below5_population"]),
                  sum_NONA(final_table[,"Total_above65_population"]),
                  100 * sum_NONA(final_table[,"Total_below5_population"]) / sum_NONA(final_table[,"Total_population"]),
                  100 * sum_NONA(final_table[,"Total_above65_population"]) / sum_NONA(final_table[,"Total_population"]),
                  
                  sum_NONA(final_table[,"Total_bipoc_population"]),
                  100 * sum_NONA(final_table[,"Total_bipoc_population"]) / sum_NONA(final_table[,"Total_population"]),
                  
                  sum_NONA(final_table[,"M.Total_population"]), 
                  sum_NONA(final_table[,"M.Total_black_population"]), 
                  sum_NONA(final_table[,"M.Total_hispanic_population"]),
                  sum_NONA(final_table[,"M.Total_white_population"]),
                  100 * sum_NONA(final_table[,"M.Total_black_population"]) / sum_NONA(final_table[,"M.Total_population"]),
                  100 * sum_NONA(final_table[,"M.Total_hispanic_population"]) / sum_NONA(final_table[,"M.Total_population"]),
                  100 * sum_NONA(final_table[,"M.Total_white_population"]) / sum_NONA(final_table[,"M.Total_population"]),
                  sum_NONA(final_table[,"M.Total_below5_population"]),
                  sum_NONA(final_table[,"M.Total_above65_population"]),
                  100 * sum_NONA(final_table[,"M.Total_below5_population"]) / sum_NONA(final_table[,"M.Total_population"]),
                  100 * sum_NONA(final_table[,"M.Total_above65_population"]) / sum_NONA(final_table[,"M.Total_population"]),
                  
                  sum_NONA(final_table[,"M.Total_bipoc_population"]),
                  100 * sum_NONA(final_table[,"M.Total_bipoc_population"]) / sum_NONA(final_table[,"M.Total_population"]),
                  
                  sum_NONA(final_table[,"M.N.Total_population"]), 
                  sum_NONA(final_table[,"M.N.Total_black_population"]), 
                  sum_NONA(final_table[,"M.N.Total_hispanic_population"]),
                  sum_NONA(final_table[,"M.N.Total_white_population"]),
                  100 * sum_NONA(final_table[,"M.N.Total_black_population"]) / sum_NONA(final_table[,"M.N.Total_population"]),
                  100 * sum_NONA(final_table[,"M.N.Total_hispanic_population"]) / sum_NONA(final_table[,"M.N.Total_population"]),
                  100 * sum_NONA(final_table[,"M.N.Total_white_population"]) / sum_NONA(final_table[,"M.N.Total_population"]),
                  sum_NONA(final_table[,"M.N.Total_below5_population"]),
                  sum_NONA(final_table[,"M.N.Total_above65_population"]),
                  100 * sum_NONA(final_table[,"M.N.Total_below5_population"]) / sum_NONA(final_table[,"M.N.Total_population"]),
                  100 * sum_NONA(final_table[,"M.N.Total_above65_population"]) / sum_NONA(final_table[,"M.N.Total_population"]),
                  
                  sum_NONA(final_table[,"M.N.Total_bipoc_population"]),
                  100 * sum_NONA(final_table[,"M.N.Total_bipoc_population"]) / sum_NONA(final_table[,"M.N.Total_population"]),
                  
                  sum_NONA(final_table[,"E.Total_population"]), 
                  sum_NONA(final_table[,"E.Total_black_population"]), 
                  sum_NONA(final_table[,"E.Total_hispanic_population"]),
                  sum_NONA(final_table[,"E.Total_white_population"]),
                  100 * sum_NONA(final_table[,"E.Total_black_population"]) / sum_NONA(final_table[,"E.Total_population"]),
                  100 * sum_NONA(final_table[,"E.Total_hispanic_population"]) / sum_NONA(final_table[,"E.Total_population"]),
                  100 * sum_NONA(final_table[,"E.Total_white_population"]) / sum_NONA(final_table[,"E.Total_population"]),
                  sum_NONA(final_table[,"E.Total_below5_population"]),
                  sum_NONA(final_table[,"E.Total_above65_population"]),
                  100 * sum_NONA(final_table[,"E.Total_below5_population"]) / sum_NONA(final_table[,"E.Total_population"]),
                  100 * sum_NONA(final_table[,"E.Total_above65_population"]) / sum_NONA(final_table[,"E.Total_population"]),
                  
                  sum_NONA(final_table[,"E.Total_bipoc_population"]),
                  100 * sum_NONA(final_table[,"E.Total_bipoc_population"]) / sum_NONA(final_table[,"E.Total_population"]),
                  
                  sum_NONA(final_table[,"E.N.Total_population"]), 
                  sum_NONA(final_table[,"E.N.Total_black_population"]), 
                  sum_NONA(final_table[,"E.N.Total_hispanic_population"]),
                  sum_NONA(final_table[,"E.N.Total_white_population"]),
                  100 * sum_NONA(final_table[,"E.N.Total_black_population"]) / sum_NONA(final_table[,"E.N.Total_population"]),
                  100 * sum_NONA(final_table[,"E.N.Total_hispanic_population"]) / sum_NONA(final_table[,"E.N.Total_population"]),
                  100 * sum_NONA(final_table[,"E.N.Total_white_population"]) / sum_NONA(final_table[,"E.N.Total_population"]),
                  sum_NONA(final_table[,"E.N.Total_below5_population"]),
                  sum_NONA(final_table[,"E.N.Total_above65_population"]),
                  100 * sum_NONA(final_table[,"E.N.Total_below5_population"]) / sum_NONA(final_table[,"E.N.Total_population"]),
                  100 * sum_NONA(final_table[,"E.N.Total_above65_population"]) / sum_NONA(final_table[,"E.N.Total_population"]),
                  
                  sum_NONA(final_table[,"E.N.Total_bipoc_population"]),
                  100 * sum_NONA(final_table[,"E.N.Total_bipoc_population"]) / sum_NONA(final_table[,"E.N.Total_population"]),
                  
                  100 * sum_NONA(final_table[,"M.Total_population"]) / sum_NONA(final_table[,"Total_population"]),
                  100 * sum_NONA(final_table[,"E.Total_population"]) / sum_NONA(final_table[,"Total_population"]),
                    
                  sum_NONA(final_table[,"Total_residential_lots"]),
                  sum_NONA(final_table[,"Total_basement_residential_lots"]),
                  sum_NONA(final_table[,"Total_mixed_comres_lots"]),
                  sum_NONA(final_table[,"Total_commercial_lots"]),
                  sum_NONA(final_table[,"Total_industrial_lots"]),
                  
                  sum_NONA(final_table[,"M.Total_residential_lots_0"]),
                  sum_NONA(final_table[,"M.Total_residential_lots_5"]),
                  sum_NONA(final_table[,"M.Total_residential_lots_15"]),
                  sum_NONA(final_table[,"M.Total_residential_lots_30"]),
                  sum_NONA(final_table[,"M.Total_residential_lots_50"]),
                  sum_NONA(final_table[,"M.Total_residential_lots"]),
                  sum_NONA(final_table[,"M.Total_basement_residential_lots"]),
                  sum_NONA(final_table[,"M.Total_mixed_comres_lots"]),
                  sum_NONA(final_table[,"M.Total_commercial_lots"]),
                  sum_NONA(final_table[,"M.Total_industrial_lots"]),
                  
                  sum_NONA(final_table[,"E.Total_residential_lots_0"]),
                  sum_NONA(final_table[,"E.Total_residential_lots_5"]),
                  sum_NONA(final_table[,"E.Total_residential_lots_15"]),
                  sum_NONA(final_table[,"E.Total_residential_lots_30"]),
                  sum_NONA(final_table[,"E.Total_residential_lots_50"]),
                  sum_NONA(final_table[,"E.Total_residential_lots"]),
                  sum_NONA(final_table[,"E.Total_basement_residential_lots"]),
                  sum_NONA(final_table[,"E.Total_mixed_comres_lots"]),
                  sum_NONA(final_table[,"E.Total_commercial_lots"]),
                  sum_NONA(final_table[,"E.Total_industrial_lots"]),
                  
                  100 * sum_NONA(final_table[,"M.Total_residential_lots"]) / sum_NONA(final_table[,"Total_residential_lots"]),
                  100 * sum_NONA(final_table[,"M.Total_basement_residential_lots"]) / sum_NONA(final_table[,"Total_basement_residential_lots"]),
                  100 * sum_NONA(final_table[,"M.Total_mixed_comres_lots"]) / sum_NONA(final_table[,"Total_mixed_comres_lots"]),
                  100 * sum_NONA(final_table[,"M.Total_commercial_lots"]) / sum_NONA(final_table[,"Total_commercial_lots"]),
                  100 * sum_NONA(final_table[,"M.Total_industrial_lots"]) / sum_NONA(final_table[,"Total_industrial_lots"]),
                  
                  100 * sum_NONA(final_table[,"E.Total_residential_lots"]) / sum_NONA(final_table[,"Total_residential_lots"]),
                  100 * sum_NONA(final_table[,"E.Total_basement_residential_lots"]) / sum_NONA(final_table[,"Total_basement_residential_lots"]),
                  100 * sum_NONA(final_table[,"E.Total_mixed_comres_lots"]) / sum_NONA(final_table[,"Total_mixed_comres_lots"]),
                  100 * sum_NONA(final_table[,"E.Total_commercial_lots"]) / sum_NONA(final_table[,"Total_commercial_lots"]),
                  100 * sum_NONA(final_table[,"E.Total_industrial_lots"]) / sum_NONA(final_table[,"Total_industrial_lots"]),
                  
                  sum_NONA(final_table[,"Total_buildings"]),
                  sum_NONA(final_table[,"M.Total_buildings_0"]),
                  sum_NONA(final_table[,"M.Total_buildings_5"]),
                  sum_NONA(final_table[,"M.Total_buildings_15"]),
                  sum_NONA(final_table[,"M.Total_buildings_30"]),
                  sum_NONA(final_table[,"M.Total_buildings_50"]),
                  sum_NONA(final_table[,"E.Total_buildings_0"]),
                  sum_NONA(final_table[,"E.Total_buildings_5"]),
                  sum_NONA(final_table[,"E.Total_buildings_15"]),
                  sum_NONA(final_table[,"E.Total_buildings_30"]),
                  sum_NONA(final_table[,"E.Total_buildings_50"]),
                  
                  100 * sum_NONA(final_table[,"M.Total_buildings_0"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"M.Total_buildings_5"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"M.Total_buildings_15"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"M.Total_buildings_30"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"M.Total_buildings_50"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"E.Total_buildings_0"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"E.Total_buildings_5"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"E.Total_buildings_15"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"E.Total_buildings_30"]) / sum_NONA(final_table[,"Total_buildings"]),
                  100 * sum_NONA(final_table[,"E.Total_buildings_50"]) / sum_NONA(final_table[,"Total_buildings"]),
                  
                  sum_NONA(final_table[,"Total_Education_facilities"]),
                  sum_NONA(final_table[,"M.Total_Education_facilities"]),
                  sum_NONA(final_table[,"E.Total_Education_facilities"]),
                  100* sum_NONA(final_table[,"M.Total_Education_facilities"]) / sum_NONA(final_table[,"Total_Education_facilities"]),
                  100* sum_NONA(final_table[,"E.Total_Education_facilities"]) / sum_NONA(final_table[,"Total_Education_facilities"]),
                  
                  sum_NONA(final_table[,"Total_Human_Welfare_Services_facilities"]),
                  sum_NONA(final_table[,"M.Total_Human_Welfare_Services_facilities"]),
                  sum_NONA(final_table[,"E.Total_Human_Welfare_Services_facilities"]),
                  100* sum_NONA(final_table[,"M.Total_Human_Welfare_Services_facilities"]) / sum_NONA(final_table[,"Total_Human_Welfare_Services_facilities"]),
                  100* sum_NONA(final_table[,"E.Total_Human_Welfare_Services_facilities"]) / sum_NONA(final_table[,"Total_Human_Welfare_Services_facilities"]),
                  
                  sum_NONA(final_table[,"Total_Healthcare_facilities"]),
                  sum_NONA(final_table[,"M.Total_Healthcare_facilities"]),
                  sum_NONA(final_table[,"E.Total_Healthcare_facilities"]),
                  100* sum_NONA(final_table[,"M.Total_Healthcare_facilities"]) / sum_NONA(final_table[,"Total_Healthcare_facilities"]),
                  100* sum_NONA(final_table[,"E.Total_Healthcare_facilities"]) / sum_NONA(final_table[,"Total_Healthcare_facilities"]),
                  
                  sum_NONA(final_table[,"Total_Public_Safety_facilities"]),
                  sum_NONA(final_table[,"M.Total_Public_Safety_facilities"]),
                  sum_NONA(final_table[,"E.Total_Public_Safety_facilities"]),
                  100* sum_NONA(final_table[,"M.Total_Public_Safety_facilities"]) / sum_NONA(final_table[,"Total_Public_Safety_facilities"]),
                  100* sum_NONA(final_table[,"E.Total_Public_Safety_facilities"]) / sum_NONA(final_table[,"Total_Public_Safety_facilities"]),
                  
                  sum_NONA(final_table[,"Total_bus_stops"]),
                  sum_NONA(final_table[,"M.Total_bus_stops"]),
                  sum_NONA(final_table[,"E.Total_bus_stops"]),
                  100* sum_NONA(final_table[,"M.Total_bus_stops"]) / sum_NONA(final_table[,"Total_bus_stops"]),
                  100* sum_NONA(final_table[,"E.Total_bus_stops"]) / sum_NONA(final_table[,"Total_bus_stops"]),
                  
                  sum_NONA(final_table[,"Total_subway_stops"]),
                  sum_NONA(final_table[,"M.Total_subway_stops"]),
                  sum_NONA(final_table[,"E.Total_subway_stops"]),
                  100* sum_NONA(final_table[,"M.Total_subway_stops"]) / sum_NONA(final_table[,"Total_subway_stops"]),
                  100* sum_NONA(final_table[,"E.Total_subway_stops"]) / sum_NONA(final_table[,"Total_subway_stops"]),
                  
                  sum_NONA(final_table[,"Total_bus_routes"]),
                  sum_NONA(final_table[,"M.Total_bus_routes"]),
                  sum_NONA(final_table[,"E.Total_bus_routes"]),
                  100* sum_NONA(final_table[,"M.Total_bus_routes"]) / sum_NONA(final_table[,"Total_bus_routes"]),
                  100* sum_NONA(final_table[,"E.Total_bus_routes"]) / sum_NONA(final_table[,"Total_bus_routes"]),
                  
                  NYC.Belpov_e,
                  NYC.Belpov_s,
                  NYC.Total_Belpov_e,
                  NYC.Total_Belpov_s,
                  NYC.Total_HH_Income_e,
                  NYC.Total_HH_Income_s,
                  NYC.HH_Income_Bel75K_e,
                  NYC.HH_Income_Bel75K_s,
                  NYC.Total_HH_Dis_e,
                  NYC.Total_HH_Dis_s,
                  NYC.HH_Disability_e,
                  NYC.HH_Disability_s,
                  NYC.Total_Rent_HH_e,
                  NYC.Total_Rent_HH_s,
                  NYC.RB_HH_e,
                  NYC.RB_HH_s,
                  NYC.Total_Owned_HH_e,
                  NYC.Total_Owned_HH_s,
                  NYC.CB_HH_e,
                  NYC.CB_HH_s,
                  NYC.PCT_Belpov_e,
                  NYC.PCT_Belpov_s,
                  NYC.PCT_HH_Income_Bel75K_e,
                  NYC.PCT_HH_Income_Bel75K_s,
                  NYC.PCT_HH_Disability_e,
                  NYC.PCT_HH_Disability_s,
                  NYC.PCT_RB_e,
                  NYC.PCT_RB_s,
                  NYC.PCT_CB_e,
                  NYC.PCT_CB_s,
                  
                  NYC.M.Belpov_e,
                  NYC.M.Belpov_s,
                  NYC.M.Total_Belpov_e,
                  NYC.M.Total_Belpov_s,
                  NYC.M.Total_HH_Income_e,
                  NYC.M.Total_HH_Income_s,
                  NYC.M.HH_Income_Bel75K_e,
                  NYC.M.HH_Income_Bel75K_s,
                  NYC.M.Total_HH_Dis_e,
                  NYC.M.Total_HH_Dis_s,
                  NYC.M.HH_Disability_e,
                  NYC.M.HH_Disability_s,
                  NYC.M.Total_Rent_HH_e,
                  NYC.M.Total_Rent_HH_s,
                  NYC.M.RB_HH_e,
                  NYC.M.RB_HH_s,
                  NYC.M.Total_Owned_HH_e,
                  NYC.M.Total_Owned_HH_s,
                  NYC.M.CB_HH_e,
                  NYC.M.CB_HH_s,
                  NYC.M.PCT_Belpov_e,
                  NYC.M.PCT_Belpov_s,
                  NYC.M.PCT_HH_Income_Bel75K_e,
                  NYC.M.PCT_HH_Income_Bel75K_s,
                  NYC.M.PCT_HH_Disability_e,
                  NYC.M.PCT_HH_Disability_s,
                  NYC.M.PCT_RB_e,
                  NYC.M.PCT_RB_s,
                  NYC.M.PCT_CB_e,
                  NYC.M.PCT_CB_s,
                  
                  NYC.M.N.Belpov_e,
                  NYC.M.N.Belpov_s,
                  NYC.M.N.Total_Belpov_e,
                  NYC.M.N.Total_Belpov_s,
                  NYC.M.N.Total_HH_Income_e,
                  NYC.M.N.Total_HH_Income_s,
                  NYC.M.N.HH_Income_Bel75K_e,
                  NYC.M.N.HH_Income_Bel75K_s,
                  NYC.M.N.Total_HH_Dis_e,
                  NYC.M.N.Total_HH_Dis_s,
                  NYC.M.N.HH_Disability_e,
                  NYC.M.N.HH_Disability_s,
                  NYC.M.N.Total_Rent_HH_e,
                  NYC.M.N.Total_Rent_HH_s,
                  NYC.M.N.RB_HH_e,
                  NYC.M.N.RB_HH_s,
                  NYC.M.N.Total_Owned_HH_e,
                  NYC.M.N.Total_Owned_HH_s,
                  NYC.M.N.CB_HH_e,
                  NYC.M.N.CB_HH_s,
                  NYC.M.N.PCT_Belpov_e,
                  NYC.M.N.PCT_Belpov_s,
                  NYC.M.N.PCT_HH_Income_Bel75K_e,
                  NYC.M.N.PCT_HH_Income_Bel75K_s,
                  NYC.M.N.PCT_HH_Disability_e,
                  NYC.M.N.PCT_HH_Disability_s,
                  NYC.M.N.PCT_RB_e,
                  NYC.M.N.PCT_RB_s,
                  NYC.M.N.PCT_CB_e,
                  NYC.M.N.PCT_CB_s,
                  
                  NYC.E.Belpov_e,
                  NYC.E.Belpov_s,
                  NYC.E.Total_Belpov_e,
                  NYC.E.Total_Belpov_s,
                  NYC.E.Total_HH_Income_e,
                  NYC.E.Total_HH_Income_s,
                  NYC.E.HH_Income_Bel75K_e,
                  NYC.E.HH_Income_Bel75K_s,
                  NYC.E.Total_HH_Dis_e,
                  NYC.E.Total_HH_Dis_s,
                  NYC.E.HH_Disability_e,
                  NYC.E.HH_Disability_s,
                  NYC.E.Total_Rent_HH_e,
                  NYC.E.Total_Rent_HH_s,
                  NYC.E.RB_HH_e,
                  NYC.E.RB_HH_s,
                  NYC.E.Total_Owned_HH_e,
                  NYC.E.Total_Owned_HH_s,
                  NYC.E.CB_HH_e,
                  NYC.E.CB_HH_s,
                  NYC.E.PCT_Belpov_e,
                  NYC.E.PCT_Belpov_s,
                  NYC.E.PCT_HH_Income_Bel75K_e,
                  NYC.E.PCT_HH_Income_Bel75K_s,
                  NYC.E.PCT_HH_Disability_e,
                  NYC.E.PCT_HH_Disability_s,
                  NYC.E.PCT_RB_e,
                  NYC.E.PCT_RB_s,
                  NYC.E.PCT_CB_e,
                  NYC.E.PCT_CB_s,
                  
                  NYC.E.N.Belpov_e,
                  NYC.E.N.Belpov_s,
                  NYC.E.N.Total_Belpov_e,
                  NYC.E.N.Total_Belpov_s,
                  NYC.E.N.Total_HH_Income_e,
                  NYC.E.N.Total_HH_Income_s,
                  NYC.E.N.HH_Income_Bel75K_e,
                  NYC.E.N.HH_Income_Bel75K_s,
                  NYC.E.N.Total_HH_Dis_e,
                  NYC.E.N.Total_HH_Dis_s,
                  NYC.E.N.HH_Disability_e,
                  NYC.E.N.HH_Disability_s,
                  NYC.E.N.Total_Rent_HH_e,
                  NYC.E.N.Total_Rent_HH_s,
                  NYC.E.N.RB_HH_e,
                  NYC.E.N.RB_HH_s,
                  NYC.E.N.Total_Owned_HH_e,
                  NYC.E.N.Total_Owned_HH_s,
                  NYC.E.N.CB_HH_e,
                  NYC.E.N.CB_HH_s,
                  NYC.E.N.PCT_Belpov_e,
                  NYC.E.N.PCT_Belpov_s,
                  NYC.E.N.PCT_HH_Income_Bel75K_e,
                  NYC.E.N.PCT_HH_Income_Bel75K_s,
                  NYC.E.N.PCT_HH_Disability_e,
                  NYC.E.N.PCT_HH_Disability_s,
                  NYC.E.N.PCT_RB_e,
                  NYC.E.N.PCT_RB_s,
                  NYC.E.N.PCT_CB_e,
                  NYC.E.N.PCT_CB_s
)

## noww we have all the exposure and vulnerability data in a single database. it is quite large, so I decided to set the chunk of columns related to facility types aside

final_table_facility_subgroups <- final_table[,c(1,155:229)] ## columns 155-229 correspond to the individual facility types

write.csv(final_table_facility_subgroups,
          row.names = FALSE,
          "data/3_output/stormwater_analysis_final_database_facility_subgroups.csv")

final_table <- final_table[,-(155:229)]

final_table_NYC <- rbind(final_table, NYC_row_data)

write.csv(final_table_NYC,
          row.names = FALSE,
          "data/3_output/stormwater_analysis_final_database.csv")

gc()

## and finally, here is a subscript that will generate a table for the paper - this script takes the NYC wide row and creates a summary table reporting the impacts of flooding across the entire city

source("src/NYCF_summary_tables.R")