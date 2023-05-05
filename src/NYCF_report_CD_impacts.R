
source("src/NYCF_housekeeping_GIS_vars.R")

## load CDs layer without and with parks

CD <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd) %>%
  filter(boro_cd %nin% c(164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

CD_with_parks <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd)

## load flooding data

extreme_flooding <- sf::st_read(dsn = "data/1_raw/NYC_Stormwater_Flood_Map_-_Extreme_Flood.gdb/NYC Stormwater Flood Map - Extreme Flood.gdb",
                                layer = "ExtremeFlood_single_part") %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>%
  st_transform(UTM_18N_meter) %>% 
  st_dissolve() %>% 
  st_intersection(CD)

moderate_flooding <- sf::st_read(dsn = "data/1_raw/NYC_Stormwater_Flood_Map_-_Moderate_Flood.gdb/NYC Stormwater Flood Map - Moderate Flood.gdb",
                                 layer = "ModerateFlood_single_part") %>%
  st_cast("MULTIPOLYGON") %>%
  st_make_valid() %>%
  st_transform(UTM_18N_meter) %>% 
  st_dissolve() %>% 
  st_intersection(CD)

# Run script ONCE to save decennial census data
# source("src/NYCF_decennial_census_data.R")

# Run script ONCE to save decennial ACS data
# source("src/NYCF_ACS_data.R")

# Run script ONCE to save tax lot data
# source("src/NYCF_tax_lots_data.R")

# Run script ONCE to save facilities data
# source("src/NYCF_facilities_data.R")

# Run script ONCE to save transportation data
# source("src/NYCF_transportation_data.R")

# Final table template on which we will keep on adding fields

columns <- c("Geography", 
             "Total_area", 
             "M.Total_area", 
             "M.PCT_area", 
             "E.Total_area", 
             "E.PCT_area",
             "Total_road_area",
             "M.Total_road_area",
             "M.PCT_road_area",
             "E.Total_road_area",
             "E.PCT_road_area")

final_table <- data.frame(matrix(ncol = length(columns), nrow = nrow(CD)))
names(final_table) <- columns
final_table$Geography <- c(CD$boro_cd)

## geographic total areas (based on CDs layer)

final_table["Total_area"] <- c(st_area(CD))

### assess impact of flooding - areas within city and case CDs

for (comdist in CD$boro_cd){
  
  print(comdist)
  
  neighborhood <- CD[CD$boro_cd == comdist,]
  
  mod.flood <- filter(moderate_flooding, boro_cd == comdist)
  mod.area <- sum(st_area(mod.flood))

  ext.flood <- filter(extreme_flooding, boro_cd == comdist)
  ext.area <- sum(st_area(ext.flood))
  
  final_table[final_table$Geography == comdist, "M.Total_area"] <- mod.area
  final_table[final_table$Geography == comdist, "E.Total_area"] <- ext.area
  
  final_table[final_table$Geography == comdist, "M.PCT_area"] <- 100 * final_table[final_table$Geography == comdist, "M.Total_area"] / final_table[final_table$Geography == comdist, "Total_area"]
  final_table[final_table$Geography == comdist, "E.PCT_area"] <- 100 * final_table[final_table$Geography == comdist, "E.Total_area"] / final_table[final_table$Geography == comdist, "Total_area"]
  
  rm(comdist, mod.flood, mod.area, ext.flood, ext.area)
  
}

NYC_flooding_blocks.joined.CDs <- st_read("data/2_intermediate/NYC_blocks_CD_demo_flood.shp") %>%
  st_drop_geometry()

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

### Write demographic variables in the final table!
final_table <- inner_join(final_table, demographic_summary, by = c("Geography" = "boro_cd"))

### Now demographic data for flooded census blocks

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
            M.Total_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= 30),
            M.Total_basement_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES == "Basement in residential lot"& m_d_f <= 30),
            M.Total_mixed_comres_lots = sum(LUseCat %in% "Mixed residential and commercial"& m_d_f <= 30),
            M.Total_commercial_lots = sum(LUseCat %in% "Commercial"& m_d_f <= 30),
            M.Total_industrial_lots = sum(LUseCat %in% "Industrial & manufacturing"& m_d_f <= 30),
            E.Total_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= 30),
            E.Total_basement_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES == "Basement in residential lot"& e_d_f <= 30),
            E.Total_mixed_comres_lots = sum(LUseCat %in% "Mixed residential and commercial"& e_d_f <= 30),
            E.Total_commercial_lots = sum(LUseCat %in% "Commercial"& e_d_f <= 30),
            E.Total_industrial_lots = sum(LUseCat %in% "Industrial & manufacturing"& e_d_f <= 30)) %>%
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

### Critical infrastructure 

facilities_data_points <- st_read("data/2_intermediate/facilities_20210811_flooding.shp") %>% 
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

facility_categories <- unique(facilities_data_points$CatFac)
facility_subgroups <- unique(facilities_data_points$FACSUBGRP)

for(facility_cat in facility_categories){
  
  print(paste0("   ", facility_cat))
  
  sliced_catfac <- facilities_data_points[facilities_data_points$CatFac == facility_cat,]
  
  for(comdist in final_table$Geography){
    
    sliced_catfac_CD <- sliced_catfac[sliced_catfac$boro_cd == comdist,]
    
    total_nr_facility <- nrow(sliced_catfac_CD)
    M.Flooded_nr_facility <-nrow(sliced_catfac_CD[sliced_catfac_CD$m_d_f <= 30,])
    E.Flooded_nr_facility <-nrow(sliced_catfac_CD[sliced_catfac_CD$e_d_f <= 30,])
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


facilities_summary_citywide <- data.frame(SUBGRP = facility_subgroups, "Total" = 0, "M.Total" = 0, "E.Total" = 0, "M.PCT" =0, "E.PCT"=0)

for(facility_subgrp in facility_subgroups){
  
  print(paste0("   ", facility_subgrp))
  
  sliced_facsbgrp <- facilities_data_points[facilities_data_points$FACSUBGRP == facility_subgrp,]
  
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "Total"] <- nrow(sliced_facsbgrp)
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "M.Total"] <- nrow(sliced_facsbgrp[sliced_facsbgrp$m_d_f <= 30,])
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "E.Total"] <- nrow(sliced_facsbgrp[sliced_facsbgrp$e_d_f <= 30,])
  
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "M.PCT"] <- 100 * nrow(sliced_facsbgrp[sliced_facsbgrp$m_d_f <= 30,]) / nrow(sliced_facsbgrp)
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "E.PCT"] <- 100 * nrow(sliced_facsbgrp[sliced_facsbgrp$e_d_f <= 30,]) / nrow(sliced_facsbgrp)
  
  for(comdist in final_table$Geography){
    
    sliced_facsbgrp_CD <- sliced_facsbgrp[sliced_facsbgrp$boro_cd == comdist,]
    
    total_nr_subgrp <- nrow(sliced_facsbgrp_CD)
    M.Flooded_nr_subgrp <-nrow(sliced_facsbgrp_CD[sliced_facsbgrp_CD$m_d_f <= 30,])
    E.Flooded_nr_subgrp <-nrow(sliced_facsbgrp_CD[sliced_facsbgrp_CD$e_d_f <= 30,])
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


### Transportation indicators

bus_stops_summary <- st_read("data/2_intermediate/bus_stops_flooding.shp") %>%
  st_drop_geometry() %>%
  group_by(boro_cd) %>%
  summarize(Total_bus_stops = n(),
            M.Total_bus_stops = sum(m_d_f <= 30),
            E.Total_bus_stops = sum(e_d_f <= 30)) %>%
  mutate(M.PCT_bus_stops = 100 * M.Total_bus_stops / Total_bus_stops,
         E.PCT_bus_stops = 100 * E.Total_bus_stops / Total_bus_stops)
  
subway_entrances_summary <- st_read("data/2_intermediate/subway_entrances_flooding.shp") %>%
  st_drop_geometry() %>%
  group_by(boro_cd) %>%
  summarize(Total_subway_stops = n(),
            M.Total_subway_stops = sum(m_d_f <= 30),
            E.Total_subway_stops = sum(e_d_f <= 30)) %>%
  mutate(M.PCT_subway_stops = 100 * M.Total_subway_stops / Total_subway_stops,
         E.PCT_subway_stops = 100 * E.Total_subway_stops / Total_subway_stops)

final_table <- final_table %>%
  left_join(bus_stops_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(subway_entrances_summary, by = c("Geography" = "boro_cd"))

bus_routes <- st_read("data/1_raw/bus_routes_nyc_nov2020.shp") %>% 
  st_transform(UTM_18N_meter) %>%
  st_intersection(CD)

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

### % roads flooded

roads <- st_read("data/1_raw/geo_export_4443d165-7281-4b57-a1a1-f651c232b7ab.shp") %>%
  st_transform(UTM_18N_meter) %>%
  filter(rw_type %nin% c(2,3,9, 14, 4)) # IM ALSO REMOVING 3 (bridges) because flooding will show the underpass flooding  and 14 (ferry routes) and 4 (tunnels) because flooding is in the surface

for (comdist in CD$boro_cd){
  print(comdist)
  
  AOI <- CD[CD$boro_cd == comdist,]
  
  roads_cd <- roads[AOI, ]
  
  roads_cd.buf <- st_buffer(roads_cd, (roads_cd$st_width / (2*3.281)), endCapStyle = "FLAT") %>% 
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


### Load SOVI data

SOVI_data.CD <- read_csv("data/2_intermediate/NYC_ACS_2018.csv") %>% 
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595)) %>%
  mutate(boro_cd = as.numeric(boro_cd)) %>%
  filter(ALAND > 0)

### Change to NA MoEs of second zeros

### Separate data here, then change MoE of second zeros to NAs

ACS_demographic_summary <- (SOVI_data.CD) %>% second_zero_NAs()

M.ACS_demographic_summary <- (filter(SOVI_data.CD, m_d_f == 0)) %>% second_zero_NAs()
M.N.ACS_demographic_summary <- (filter(SOVI_data.CD, m_d_f > 0)) %>% second_zero_NAs()

E.ACS_demographic_summary <- (filter(SOVI_data.CD, e_d_f == 0)) %>% second_zero_NAs()
E.N.ACS_demographic_summary <- (filter(SOVI_data.CD, e_d_f > 0)) %>% second_zero_NAs()

ACS_demographic_summary <- (ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(Belpov_e = sum(Belpov_e, na.rm = TRUE),
                   Belpov_s = (sum(Belpov_s^2, na.rm = TRUE))^0.5,
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

final_table <- final_table %>%
  left_join(ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>% 
  left_join(M.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(M.N.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(E.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(E.N.ACS_demographic_summary, by = c("Geography" = "boro_cd"))

# For the NYC row, lets calculate ACS variables first, then add them (so I dont go crazy)

## CityWide

NYC.Total_Belpov_e <- sum(final_table[,"Total_Belpov_e"])
NYC.Total_Belpov_s <- (sum(final_table[,"Total_Belpov_s"]^2))^0.5
NYC.Belpov_e <- sum(final_table[,"Belpov_e"])
NYC.Belpov_s <- (sum(final_table[,"Belpov_s"]^2))^0.5

NYC.Total_HH_Income_e <- sum(final_table[,"Total_HH_Income_e"])
NYC.Total_HH_Income_s <- (sum(final_table[,"Total_HH_Income_s"]^2))^0.5
NYC.HH_Income_Bel75K_e <- sum(final_table[,"HH_Income_Bel75K_e"])
NYC.HH_Income_Bel75K_s <- (sum(final_table[,"HH_Income_Bel75K_s"]^2))^0.5

NYC.Total_HH_Dis_e <- sum(final_table[,"Total_HH_Dis_e"])
NYC.Total_HH_Dis_s <- (sum(final_table[,"Total_HH_Dis_s"]^2))^0.5
NYC.HH_Disability_e <- sum(final_table[,"HH_Disability_e"])
NYC.HH_Disability_s <- (sum(final_table[,"HH_Disability_s"]^2))^0.5

NYC.Total_Rent_HH_e <- sum(final_table[,"Total_Rent_HH_e"])
NYC.Total_Rent_HH_s <- (sum(final_table[,"Total_Rent_HH_s"]^2))^0.5
NYC.RB_HH_e <- sum(final_table[,"RB_HH_e"])
NYC.RB_HH_s <- (sum(final_table[,"RB_HH_s"]^2))^0.5

NYC.Total_Owned_HH_e <- sum(final_table[,"Total_Owned_HH_e"])
NYC.Total_Owned_HH_s <- (sum(final_table[,"Total_Owned_HH_s"]^2))^0.5
NYC.CB_HH_e <- sum(final_table[,"CB_HH_e"])
NYC.CB_HH_s <- (sum(final_table[,"CB_HH_s"]^2))^0.5

NYC.PCT_Belpov_e <- 100 * NYC.Belpov_e / NYC.Total_Belpov_e
NYC.PCT_Belpov_s <- 100 * ifelse(NYC.PCT_Belpov_e != 100,
                             ifelse((NYC.Belpov_s^2 - ((NYC.Belpov_e / NYC.Total_Belpov_e)^2 * NYC.Total_Belpov_s^2)) > 0,
                                    (1 / NYC.Total_Belpov_e) * ((NYC.Belpov_s^2 - ((NYC.Belpov_e / NYC.Total_Belpov_e)^2 * NYC.Total_Belpov_s^2))^0.5),
                                    (1 / NYC.Total_Belpov_e) * ((NYC.Belpov_s^2 + ((NYC.Belpov_e / NYC.Total_Belpov_e)^2 * NYC.Total_Belpov_s^2))^0.5)),
                             NYC.Belpov_s / NYC.Total_Belpov_e)

NYC.PCT_HH_Income_Bel75K_e <- 100 * NYC.HH_Income_Bel75K_e / NYC.Total_HH_Income_e
NYC.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.PCT_HH_Income_Bel75K_e != 100,
                                 ifelse((NYC.HH_Income_Bel75K_s^2 - ((NYC.HH_Income_Bel75K_e / NYC.Total_HH_Income_e)^2 * NYC.Total_HH_Income_s^2)) > 0,
                                        (1 / NYC.Total_HH_Income_e) * ((NYC.HH_Income_Bel75K_s^2 - ((NYC.HH_Income_Bel75K_e / NYC.Total_HH_Income_e)^2 * NYC.Total_HH_Income_s^2))^0.5),
                                        (1 / NYC.Total_HH_Income_e) * ((NYC.HH_Income_Bel75K_s^2 + ((NYC.HH_Income_Bel75K_e / NYC.Total_HH_Income_e)^2 * NYC.Total_HH_Income_s^2))^0.5)),
                                 NYC.HH_Income_Bel75K_s / NYC.Total_HH_Income_e)

NYC.PCT_HH_Disability_e <- 100 * NYC.HH_Disability_e / NYC.Total_HH_Dis_e
NYC.PCT_HH_Disability_s <- 100 * ifelse(NYC.PCT_HH_Disability_e != 100,
                                 ifelse((NYC.HH_Disability_s^2 - ((NYC.HH_Disability_e / NYC.Total_HH_Dis_e)^2 * NYC.Total_HH_Dis_s^2)) > 0,
                                        (1 / NYC.Total_HH_Dis_e) * ((NYC.HH_Disability_s^2 - ((NYC.HH_Disability_e / NYC.Total_HH_Dis_e)^2 * NYC.Total_HH_Dis_s^2))^0.5),
                                        (1 / NYC.Total_HH_Dis_e) * ((NYC.HH_Disability_s^2 + ((NYC.HH_Disability_e / NYC.Total_HH_Dis_e)^2 * NYC.Total_HH_Dis_s^2))^0.5)),
                                 NYC.HH_Disability_s / NYC.Total_HH_Dis_e)

NYC.PCT_RB_e <- 100 * NYC.RB_HH_e / NYC.Total_Rent_HH_e
NYC.PCT_RB_s <- 100 * ifelse(NYC.PCT_RB_e != 100,
                                 ifelse((NYC.RB_HH_s^2 - ((NYC.RB_HH_e / NYC.Total_Rent_HH_e)^2 * NYC.Total_Rent_HH_s^2)) > 0,
                                        (1 / NYC.Total_Rent_HH_e) * ((NYC.RB_HH_s^2 - ((NYC.RB_HH_e / NYC.Total_Rent_HH_e)^2 * NYC.Total_Rent_HH_s^2))^0.5),
                                        (1 / NYC.Total_Rent_HH_e) * ((NYC.RB_HH_s^2 + ((NYC.RB_HH_e / NYC.Total_Rent_HH_e)^2 * NYC.Total_Rent_HH_s^2))^0.5)),
                             NYC.RB_HH_s / NYC.Total_Rent_HH_e)

NYC.PCT_CB_e <- 100 * NYC.CB_HH_e / NYC.Total_Owned_HH_e
NYC.PCT_CB_s <- 100 * ifelse(NYC.PCT_CB_e != 100,
                             ifelse((NYC.CB_HH_s^2 - ((NYC.CB_HH_e / NYC.Total_Owned_HH_e)^2 * NYC.Total_Owned_HH_s^2)) > 0,
                                    (1 / NYC.Total_Owned_HH_e) * ((NYC.CB_HH_s^2 - ((NYC.CB_HH_e / NYC.Total_Owned_HH_e)^2 * NYC.Total_Owned_HH_s^2))^0.5),
                                    (1 / NYC.Total_Owned_HH_e) * ((NYC.CB_HH_s^2 + ((NYC.CB_HH_e / NYC.Total_Owned_HH_e)^2 * NYC.Total_Owned_HH_s^2))^0.5)),
                             NYC.CB_HH_s / NYC.Total_Owned_HH_e)

## Moderate CityWide

NYC.M.Total_Belpov_e <- sum(final_table[,"M.Total_Belpov_e"])
NYC.M.Total_Belpov_s <- (sum(final_table[,"M.Total_Belpov_s"]^2))^0.5
NYC.M.Belpov_e <- sum(final_table[,"M.Belpov_e"])
NYC.M.Belpov_s <- (sum(final_table[,"M.Belpov_s"]^2))^0.5

NYC.M.Total_HH_Income_e <- sum(final_table[,"M.Total_HH_Income_e"])
NYC.M.Total_HH_Income_s <- (sum(final_table[,"M.Total_HH_Income_s"]^2))^0.5
NYC.M.HH_Income_Bel75K_e <- sum(final_table[,"M.HH_Income_Bel75K_e"])
NYC.M.HH_Income_Bel75K_s <- (sum(final_table[,"M.HH_Income_Bel75K_s"]^2))^0.5

NYC.M.Total_HH_Dis_e <- sum(final_table[,"M.Total_HH_Dis_e"])
NYC.M.Total_HH_Dis_s <- (sum(final_table[,"M.Total_HH_Dis_s"]^2))^0.5
NYC.M.HH_Disability_e <- sum(final_table[,"M.HH_Disability_e"])
NYC.M.HH_Disability_s <- (sum(final_table[,"M.HH_Disability_s"]^2))^0.5

NYC.M.Total_Rent_HH_e <- sum(final_table[,"M.Total_Rent_HH_e"])
NYC.M.Total_Rent_HH_s <- (sum(final_table[,"M.Total_Rent_HH_s"]^2))^0.5
NYC.M.RB_HH_e <- sum(final_table[,"M.RB_HH_e"])
NYC.M.RB_HH_s <- (sum(final_table[,"M.RB_HH_s"]^2))^0.5

NYC.M.Total_Owned_HH_e <- sum(final_table[,"M.Total_Owned_HH_e"])
NYC.M.Total_Owned_HH_s <- (sum(final_table[,"M.Total_Owned_HH_s"]^2))^0.5
NYC.M.CB_HH_e <- sum(final_table[,"M.CB_HH_e"])
NYC.M.CB_HH_s <- (sum(final_table[,"M.CB_HH_s"]^2))^0.5

NYC.M.PCT_Belpov_e <- 100 * NYC.M.Belpov_e / NYC.M.Total_Belpov_e
NYC.M.PCT_Belpov_s <- 100 * ifelse(NYC.M.PCT_Belpov_e != 100,
                                 ifelse((NYC.M.Belpov_s^2 - ((NYC.M.Belpov_e / NYC.M.Total_Belpov_e)^2 * NYC.M.Total_Belpov_s^2)) > 0,
                                        (1 / NYC.M.Total_Belpov_e) * ((NYC.M.Belpov_s^2 - ((NYC.M.Belpov_e / NYC.M.Total_Belpov_e)^2 * NYC.M.Total_Belpov_s^2))^0.5),
                                        (1 / NYC.M.Total_Belpov_e) * ((NYC.M.Belpov_s^2 + ((NYC.M.Belpov_e / NYC.M.Total_Belpov_e)^2 * NYC.M.Total_Belpov_s^2))^0.5)),
                                 NYC.M.Belpov_s / NYC.M.Total_Belpov_e)

NYC.M.PCT_HH_Income_Bel75K_e <- 100 * NYC.M.HH_Income_Bel75K_e / NYC.M.Total_HH_Income_e
NYC.M.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.M.PCT_HH_Income_Bel75K_e != 100,
                                           ifelse((NYC.M.HH_Income_Bel75K_s^2 - ((NYC.M.HH_Income_Bel75K_e / NYC.M.Total_HH_Income_e)^2 * NYC.M.Total_HH_Income_s^2)) > 0,
                                                  (1 / NYC.M.Total_HH_Income_e) * ((NYC.M.HH_Income_Bel75K_s^2 - ((NYC.M.HH_Income_Bel75K_e / NYC.M.Total_HH_Income_e)^2 * NYC.M.Total_HH_Income_s^2))^0.5),
                                                  (1 / NYC.M.Total_HH_Income_e) * ((NYC.M.HH_Income_Bel75K_s^2 + ((NYC.M.HH_Income_Bel75K_e / NYC.M.Total_HH_Income_e)^2 * NYC.M.Total_HH_Income_s^2))^0.5)),
                                           NYC.M.HH_Income_Bel75K_s / NYC.M.Total_HH_Income_e)

NYC.M.PCT_HH_Disability_e <- 100 * NYC.M.HH_Disability_e / NYC.M.Total_HH_Dis_e
NYC.M.PCT_HH_Disability_s <- 100 * ifelse(NYC.M.PCT_HH_Disability_e != 100,
                                        ifelse((NYC.M.HH_Disability_s^2 - ((NYC.M.HH_Disability_e / NYC.M.Total_HH_Dis_e)^2 * NYC.M.Total_HH_Dis_s^2)) > 0,
                                               (1 / NYC.M.Total_HH_Dis_e) * ((NYC.M.HH_Disability_s^2 - ((NYC.M.HH_Disability_e / NYC.M.Total_HH_Dis_e)^2 * NYC.M.Total_HH_Dis_s^2))^0.5),
                                               (1 / NYC.M.Total_HH_Dis_e) * ((NYC.M.HH_Disability_s^2 + ((NYC.M.HH_Disability_e / NYC.M.Total_HH_Dis_e)^2 * NYC.M.Total_HH_Dis_s^2))^0.5)),
                                        NYC.M.HH_Disability_s / NYC.M.Total_HH_Dis_e)

NYC.M.PCT_RB_e <- 100 * NYC.M.RB_HH_e / NYC.M.Total_Rent_HH_e
NYC.M.PCT_RB_s <- 100 * ifelse(NYC.M.PCT_RB_e != 100,
                             ifelse((NYC.M.RB_HH_s^2 - ((NYC.M.RB_HH_e / NYC.M.Total_Rent_HH_e)^2 * NYC.M.Total_Rent_HH_s^2)) > 0,
                                    (1 / NYC.M.Total_Rent_HH_e) * ((NYC.M.RB_HH_s^2 - ((NYC.M.RB_HH_e / NYC.M.Total_Rent_HH_e)^2 * NYC.M.Total_Rent_HH_s^2))^0.5),
                                    (1 / NYC.M.Total_Rent_HH_e) * ((NYC.M.RB_HH_s^2 + ((NYC.M.RB_HH_e / NYC.M.Total_Rent_HH_e)^2 * NYC.M.Total_Rent_HH_s^2))^0.5)),
                             NYC.M.RB_HH_s / NYC.M.Total_Rent_HH_e)

NYC.M.PCT_CB_e <- 100 * NYC.M.CB_HH_e / NYC.M.Total_Owned_HH_e
NYC.M.PCT_CB_s <- 100 * ifelse(NYC.M.PCT_CB_e != 100,
                             ifelse((NYC.M.CB_HH_s^2 - ((NYC.M.CB_HH_e / NYC.M.Total_Owned_HH_e)^2 * NYC.M.Total_Owned_HH_s^2)) > 0,
                                    (1 / NYC.M.Total_Owned_HH_e) * ((NYC.M.CB_HH_s^2 - ((NYC.M.CB_HH_e / NYC.M.Total_Owned_HH_e)^2 * NYC.M.Total_Owned_HH_s^2))^0.5),
                                    (1 / NYC.M.Total_Owned_HH_e) * ((NYC.M.CB_HH_s^2 + ((NYC.M.CB_HH_e / NYC.M.Total_Owned_HH_e)^2 * NYC.M.Total_Owned_HH_s^2))^0.5)),
                             NYC.M.CB_HH_s / NYC.M.Total_Owned_HH_e)

## Moderate CityWide Unexposed

NYC.M.N.Total_Belpov_e <- sum(final_table[,"M.N.Total_Belpov_e"])
NYC.M.N.Total_Belpov_s <- (sum(final_table[,"M.N.Total_Belpov_s"]^2))^0.5
NYC.M.N.Belpov_e <- sum(final_table[,"M.N.Belpov_e"])
NYC.M.N.Belpov_s <- (sum(final_table[,"M.N.Belpov_s"]^2))^0.5

NYC.M.N.Total_HH_Income_e <- sum(final_table[,"M.N.Total_HH_Income_e"])
NYC.M.N.Total_HH_Income_s <- (sum(final_table[,"M.N.Total_HH_Income_s"]^2))^0.5
NYC.M.N.HH_Income_Bel75K_e <- sum(final_table[,"M.N.HH_Income_Bel75K_e"])
NYC.M.N.HH_Income_Bel75K_s <- (sum(final_table[,"M.N.HH_Income_Bel75K_s"]^2))^0.5

NYC.M.N.Total_HH_Dis_e <- sum(final_table[,"M.N.Total_HH_Dis_e"])
NYC.M.N.Total_HH_Dis_s <- (sum(final_table[,"M.N.Total_HH_Dis_s"]^2))^0.5
NYC.M.N.HH_Disability_e <- sum(final_table[,"M.N.HH_Disability_e"])
NYC.M.N.HH_Disability_s <- (sum(final_table[,"M.N.HH_Disability_s"]^2))^0.5

NYC.M.N.Total_Rent_HH_e <- sum(final_table[,"M.N.Total_Rent_HH_e"])
NYC.M.N.Total_Rent_HH_s <- (sum(final_table[,"M.N.Total_Rent_HH_s"]^2))^0.5
NYC.M.N.RB_HH_e <- sum(final_table[,"M.N.RB_HH_e"])
NYC.M.N.RB_HH_s <- (sum(final_table[,"M.N.RB_HH_s"]^2))^0.5

NYC.M.N.Total_Owned_HH_e <- sum(final_table[,"M.N.Total_Owned_HH_e"])
NYC.M.N.Total_Owned_HH_s <- (sum(final_table[,"M.N.Total_Owned_HH_s"]^2))^0.5
NYC.M.N.CB_HH_e <- sum(final_table[,"M.N.CB_HH_e"])
NYC.M.N.CB_HH_s <- (sum(final_table[,"M.N.CB_HH_s"]^2))^0.5

NYC.M.N.PCT_Belpov_e <- 100 * NYC.M.N.Belpov_e / NYC.M.N.Total_Belpov_e
NYC.M.N.PCT_Belpov_s <- 100 * ifelse(NYC.M.N.PCT_Belpov_e != 100,
                                   ifelse((NYC.M.N.Belpov_s^2 - ((NYC.M.N.Belpov_e / NYC.M.N.Total_Belpov_e)^2 * NYC.M.N.Total_Belpov_s^2)) > 0,
                                          (1 / NYC.M.N.Total_Belpov_e) * ((NYC.M.N.Belpov_s^2 - ((NYC.M.N.Belpov_e / NYC.M.N.Total_Belpov_e)^2 * NYC.M.N.Total_Belpov_s^2))^0.5),
                                          (1 / NYC.M.N.Total_Belpov_e) * ((NYC.M.N.Belpov_s^2 + ((NYC.M.N.Belpov_e / NYC.M.N.Total_Belpov_e)^2 * NYC.M.N.Total_Belpov_s^2))^0.5)),
                                   NYC.M.N.Belpov_s / NYC.M.N.Total_Belpov_e)

NYC.M.N.PCT_HH_Income_Bel75K_e <- 100 * NYC.M.N.HH_Income_Bel75K_e / NYC.M.N.Total_HH_Income_e
NYC.M.N.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.M.N.PCT_HH_Income_Bel75K_e != 100,
                                             ifelse((NYC.M.N.HH_Income_Bel75K_s^2 - ((NYC.M.N.HH_Income_Bel75K_e / NYC.M.N.Total_HH_Income_e)^2 * NYC.M.N.Total_HH_Income_s^2)) > 0,
                                                    (1 / NYC.M.N.Total_HH_Income_e) * ((NYC.M.N.HH_Income_Bel75K_s^2 - ((NYC.M.N.HH_Income_Bel75K_e / NYC.M.N.Total_HH_Income_e)^2 * NYC.M.N.Total_HH_Income_s^2))^0.5),
                                                    (1 / NYC.M.N.Total_HH_Income_e) * ((NYC.M.N.HH_Income_Bel75K_s^2 + ((NYC.M.N.HH_Income_Bel75K_e / NYC.M.N.Total_HH_Income_e)^2 * NYC.M.N.Total_HH_Income_s^2))^0.5)),
                                             NYC.M.N.HH_Income_Bel75K_s / NYC.M.N.Total_HH_Income_e)

NYC.M.N.PCT_HH_Disability_e <- 100 * NYC.M.N.HH_Disability_e / NYC.M.N.Total_HH_Dis_e
NYC.M.N.PCT_HH_Disability_s <- 100 * ifelse(NYC.M.N.PCT_HH_Disability_e != 100,
                                          ifelse((NYC.M.N.HH_Disability_s^2 - ((NYC.M.N.HH_Disability_e / NYC.M.N.Total_HH_Dis_e)^2 * NYC.M.N.Total_HH_Dis_s^2)) > 0,
                                                 (1 / NYC.M.N.Total_HH_Dis_e) * ((NYC.M.N.HH_Disability_s^2 - ((NYC.M.N.HH_Disability_e / NYC.M.N.Total_HH_Dis_e)^2 * NYC.M.N.Total_HH_Dis_s^2))^0.5),
                                                 (1 / NYC.M.N.Total_HH_Dis_e) * ((NYC.M.N.HH_Disability_s^2 + ((NYC.M.N.HH_Disability_e / NYC.M.N.Total_HH_Dis_e)^2 * NYC.M.N.Total_HH_Dis_s^2))^0.5)),
                                          NYC.M.N.HH_Disability_s / NYC.M.N.Total_HH_Dis_e)

NYC.M.N.PCT_RB_e <- 100 * NYC.M.N.RB_HH_e / NYC.M.N.Total_Rent_HH_e
NYC.M.N.PCT_RB_s <- 100 * ifelse(NYC.M.N.PCT_RB_e != 100,
                               ifelse((NYC.M.N.RB_HH_s^2 - ((NYC.M.N.RB_HH_e / NYC.M.N.Total_Rent_HH_e)^2 * NYC.M.N.Total_Rent_HH_s^2)) > 0,
                                      (1 / NYC.M.N.Total_Rent_HH_e) * ((NYC.M.N.RB_HH_s^2 - ((NYC.M.N.RB_HH_e / NYC.M.N.Total_Rent_HH_e)^2 * NYC.M.N.Total_Rent_HH_s^2))^0.5),
                                      (1 / NYC.M.N.Total_Rent_HH_e) * ((NYC.M.N.RB_HH_s^2 + ((NYC.M.N.RB_HH_e / NYC.M.N.Total_Rent_HH_e)^2 * NYC.M.N.Total_Rent_HH_s^2))^0.5)),
                               NYC.M.N.RB_HH_s / NYC.M.N.Total_Rent_HH_e)

NYC.M.N.PCT_CB_e <- 100 * NYC.M.N.CB_HH_e / NYC.M.N.Total_Owned_HH_e
NYC.M.N.PCT_CB_s <- 100 * ifelse(NYC.M.N.PCT_CB_e != 100,
                               ifelse((NYC.M.N.CB_HH_s^2 - ((NYC.M.N.CB_HH_e / NYC.M.N.Total_Owned_HH_e)^2 * NYC.M.N.Total_Owned_HH_s^2)) > 0,
                                      (1 / NYC.M.N.Total_Owned_HH_e) * ((NYC.M.N.CB_HH_s^2 - ((NYC.M.N.CB_HH_e / NYC.M.N.Total_Owned_HH_e)^2 * NYC.M.N.Total_Owned_HH_s^2))^0.5),
                                      (1 / NYC.M.N.Total_Owned_HH_e) * ((NYC.M.N.CB_HH_s^2 + ((NYC.M.N.CB_HH_e / NYC.M.N.Total_Owned_HH_e)^2 * NYC.M.N.Total_Owned_HH_s^2))^0.5)),
                               NYC.M.N.CB_HH_s / NYC.M.N.Total_Owned_HH_e)

## Extreme CityWide

NYC.E.Total_Belpov_e <- sum(final_table[,"E.Total_Belpov_e"])
NYC.E.Total_Belpov_s <- (sum(final_table[,"E.Total_Belpov_s"]^2))^0.5
NYC.E.Belpov_e <- sum(final_table[,"E.Belpov_e"])
NYC.E.Belpov_s <- (sum(final_table[,"E.Belpov_s"]^2))^0.5

NYC.E.Total_HH_Income_e <- sum(final_table[,"E.Total_HH_Income_e"])
NYC.E.Total_HH_Income_s <- (sum(final_table[,"E.Total_HH_Income_s"]^2))^0.5
NYC.E.HH_Income_Bel75K_e <- sum(final_table[,"E.HH_Income_Bel75K_e"])
NYC.E.HH_Income_Bel75K_s <- (sum(final_table[,"E.HH_Income_Bel75K_s"]^2))^0.5

NYC.E.Total_HH_Dis_e <- sum(final_table[,"E.Total_HH_Dis_e"])
NYC.E.Total_HH_Dis_s <- (sum(final_table[,"E.Total_HH_Dis_s"]^2))^0.5
NYC.E.HH_Disability_e <- sum(final_table[,"E.HH_Disability_e"])
NYC.E.HH_Disability_s <- (sum(final_table[,"E.HH_Disability_s"]^2))^0.5

NYC.E.Total_Rent_HH_e <- sum(final_table[,"E.Total_Rent_HH_e"])
NYC.E.Total_Rent_HH_s <- (sum(final_table[,"E.Total_Rent_HH_s"]^2))^0.5
NYC.E.RB_HH_e <- sum(final_table[,"E.RB_HH_e"])
NYC.E.RB_HH_s <- (sum(final_table[,"E.RB_HH_s"]^2))^0.5

NYC.E.Total_Owned_HH_e <- sum(final_table[,"E.Total_Owned_HH_e"])
NYC.E.Total_Owned_HH_s <- (sum(final_table[,"E.Total_Owned_HH_s"]^2))^0.5
NYC.E.CB_HH_e <- sum(final_table[,"E.CB_HH_e"])
NYC.E.CB_HH_s <- (sum(final_table[,"E.CB_HH_s"]^2))^0.5

NYC.E.PCT_Belpov_e <- 100 * NYC.E.Belpov_e / NYC.E.Total_Belpov_e
NYC.E.PCT_Belpov_s <- 100 * ifelse(NYC.E.PCT_Belpov_e != 100,
                                   ifelse((NYC.E.Belpov_s^2 - ((NYC.E.Belpov_e / NYC.E.Total_Belpov_e)^2 * NYC.E.Total_Belpov_s^2)) > 0,
                                          (1 / NYC.E.Total_Belpov_e) * ((NYC.E.Belpov_s^2 - ((NYC.E.Belpov_e / NYC.E.Total_Belpov_e)^2 * NYC.E.Total_Belpov_s^2))^0.5),
                                          (1 / NYC.E.Total_Belpov_e) * ((NYC.E.Belpov_s^2 + ((NYC.E.Belpov_e / NYC.E.Total_Belpov_e)^2 * NYC.E.Total_Belpov_s^2))^0.5)),
                                   NYC.E.Belpov_s / NYC.E.Total_Belpov_e)

NYC.E.PCT_HH_Income_Bel75K_e <- 100 * NYC.E.HH_Income_Bel75K_e / NYC.E.Total_HH_Income_e
NYC.E.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.E.PCT_HH_Income_Bel75K_e != 100,
                                             ifelse((NYC.E.HH_Income_Bel75K_s^2 - ((NYC.E.HH_Income_Bel75K_e / NYC.E.Total_HH_Income_e)^2 * NYC.E.Total_HH_Income_s^2)) > 0,
                                                    (1 / NYC.E.Total_HH_Income_e) * ((NYC.E.HH_Income_Bel75K_s^2 - ((NYC.E.HH_Income_Bel75K_e / NYC.E.Total_HH_Income_e)^2 * NYC.E.Total_HH_Income_s^2))^0.5),
                                                    (1 / NYC.E.Total_HH_Income_e) * ((NYC.E.HH_Income_Bel75K_s^2 + ((NYC.E.HH_Income_Bel75K_e / NYC.E.Total_HH_Income_e)^2 * NYC.E.Total_HH_Income_s^2))^0.5)),
                                             NYC.E.HH_Income_Bel75K_s / NYC.E.Total_HH_Income_e)

NYC.E.PCT_HH_Disability_e <- 100 * NYC.E.HH_Disability_e / NYC.E.Total_HH_Dis_e
NYC.E.PCT_HH_Disability_s <- 100 * ifelse(NYC.E.PCT_HH_Disability_e != 100,
                                          ifelse((NYC.E.HH_Disability_s^2 - ((NYC.E.HH_Disability_e / NYC.E.Total_HH_Dis_e)^2 * NYC.E.Total_HH_Dis_s^2)) > 0,
                                                 (1 / NYC.E.Total_HH_Dis_e) * ((NYC.E.HH_Disability_s^2 - ((NYC.E.HH_Disability_e / NYC.E.Total_HH_Dis_e)^2 * NYC.E.Total_HH_Dis_s^2))^0.5),
                                                 (1 / NYC.E.Total_HH_Dis_e) * ((NYC.E.HH_Disability_s^2 + ((NYC.E.HH_Disability_e / NYC.E.Total_HH_Dis_e)^2 * NYC.E.Total_HH_Dis_s^2))^0.5)),
                                          NYC.E.HH_Disability_s / NYC.E.Total_HH_Dis_e)

NYC.E.PCT_RB_e <- 100 * NYC.E.RB_HH_e / NYC.E.Total_Rent_HH_e
NYC.E.PCT_RB_s <- 100 * ifelse(NYC.E.PCT_RB_e != 100,
                               ifelse((NYC.E.RB_HH_s^2 - ((NYC.E.RB_HH_e / NYC.E.Total_Rent_HH_e)^2 * NYC.E.Total_Rent_HH_s^2)) > 0,
                                      (1 / NYC.E.Total_Rent_HH_e) * ((NYC.E.RB_HH_s^2 - ((NYC.E.RB_HH_e / NYC.E.Total_Rent_HH_e)^2 * NYC.E.Total_Rent_HH_s^2))^0.5),
                                      (1 / NYC.E.Total_Rent_HH_e) * ((NYC.E.RB_HH_s^2 + ((NYC.E.RB_HH_e / NYC.E.Total_Rent_HH_e)^2 * NYC.E.Total_Rent_HH_s^2))^0.5)),
                               NYC.E.RB_HH_s / NYC.E.Total_Rent_HH_e)

NYC.E.PCT_CB_e <- 100 * NYC.E.CB_HH_e / NYC.E.Total_Owned_HH_e
NYC.E.PCT_CB_s <- 100 * ifelse(NYC.E.PCT_CB_e != 100,
                               ifelse((NYC.E.CB_HH_s^2 - ((NYC.E.CB_HH_e / NYC.E.Total_Owned_HH_e)^2 * NYC.E.Total_Owned_HH_s^2)) > 0,
                                      (1 / NYC.E.Total_Owned_HH_e) * ((NYC.E.CB_HH_s^2 - ((NYC.E.CB_HH_e / NYC.E.Total_Owned_HH_e)^2 * NYC.E.Total_Owned_HH_s^2))^0.5),
                                      (1 / NYC.E.Total_Owned_HH_e) * ((NYC.E.CB_HH_s^2 + ((NYC.E.CB_HH_e / NYC.E.Total_Owned_HH_e)^2 * NYC.E.Total_Owned_HH_s^2))^0.5)),
                               NYC.E.CB_HH_s / NYC.E.Total_Owned_HH_e)

## Extreme Citiwide Unexposed

NYC.E.N.Total_Belpov_e <- sum(final_table[,"E.N.Total_Belpov_e"])
NYC.E.N.Total_Belpov_s <- (sum(final_table[,"E.N.Total_Belpov_s"]^2))^0.5
NYC.E.N.Belpov_e <- sum(final_table[,"E.N.Belpov_e"])
NYC.E.N.Belpov_s <- (sum(final_table[,"E.N.Belpov_s"]^2))^0.5

NYC.E.N.Total_HH_Income_e <- sum(final_table[,"E.N.Total_HH_Income_e"])
NYC.E.N.Total_HH_Income_s <- (sum(final_table[,"E.N.Total_HH_Income_s"]^2))^0.5
NYC.E.N.HH_Income_Bel75K_e <- sum(final_table[,"E.N.HH_Income_Bel75K_e"])
NYC.E.N.HH_Income_Bel75K_s <- (sum(final_table[,"E.N.HH_Income_Bel75K_s"]^2))^0.5

NYC.E.N.Total_HH_Dis_e <- sum(final_table[,"E.N.Total_HH_Dis_e"])
NYC.E.N.Total_HH_Dis_s <- (sum(final_table[,"E.N.Total_HH_Dis_s"]^2))^0.5
NYC.E.N.HH_Disability_e <- sum(final_table[,"E.N.HH_Disability_e"])
NYC.E.N.HH_Disability_s <- (sum(final_table[,"E.N.HH_Disability_s"]^2))^0.5

NYC.E.N.Total_Rent_HH_e <- sum(final_table[,"E.N.Total_Rent_HH_e"])
NYC.E.N.Total_Rent_HH_s <- (sum(final_table[,"E.N.Total_Rent_HH_s"]^2))^0.5
NYC.E.N.RB_HH_e <- sum(final_table[,"E.N.RB_HH_e"])
NYC.E.N.RB_HH_s <- (sum(final_table[,"E.N.RB_HH_s"]^2))^0.5

NYC.E.N.Total_Owned_HH_e <- sum(final_table[,"E.N.Total_Owned_HH_e"])
NYC.E.N.Total_Owned_HH_s <- (sum(final_table[,"E.N.Total_Owned_HH_s"]^2))^0.5
NYC.E.N.CB_HH_e <- sum(final_table[,"E.N.CB_HH_e"])
NYC.E.N.CB_HH_s <- (sum(final_table[,"E.N.CB_HH_s"]^2))^0.5

NYC.E.N.PCT_Belpov_e <- 100 * NYC.E.N.Belpov_e / NYC.E.N.Total_Belpov_e
NYC.E.N.PCT_Belpov_s <- 100 * ifelse(NYC.E.N.PCT_Belpov_e != 100,
                                     ifelse((NYC.E.N.Belpov_s^2 - ((NYC.E.N.Belpov_e / NYC.E.N.Total_Belpov_e)^2 * NYC.E.N.Total_Belpov_s^2)) > 0,
                                            (1 / NYC.E.N.Total_Belpov_e) * ((NYC.E.N.Belpov_s^2 - ((NYC.E.N.Belpov_e / NYC.E.N.Total_Belpov_e)^2 * NYC.E.N.Total_Belpov_s^2))^0.5),
                                            (1 / NYC.E.N.Total_Belpov_e) * ((NYC.E.N.Belpov_s^2 + ((NYC.E.N.Belpov_e / NYC.E.N.Total_Belpov_e)^2 * NYC.E.N.Total_Belpov_s^2))^0.5)),
                                     NYC.E.N.Belpov_s / NYC.E.N.Total_Belpov_e)

NYC.E.N.PCT_HH_Income_Bel75K_e <- 100 * NYC.E.N.HH_Income_Bel75K_e / NYC.E.N.Total_HH_Income_e
NYC.E.N.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.E.N.PCT_HH_Income_Bel75K_e != 100,
                                               ifelse((NYC.E.N.HH_Income_Bel75K_s^2 - ((NYC.E.N.HH_Income_Bel75K_e / NYC.E.N.Total_HH_Income_e)^2 * NYC.E.N.Total_HH_Income_s^2)) > 0,
                                                      (1 / NYC.E.N.Total_HH_Income_e) * ((NYC.E.N.HH_Income_Bel75K_s^2 - ((NYC.E.N.HH_Income_Bel75K_e / NYC.E.N.Total_HH_Income_e)^2 * NYC.E.N.Total_HH_Income_s^2))^0.5),
                                                      (1 / NYC.E.N.Total_HH_Income_e) * ((NYC.E.N.HH_Income_Bel75K_s^2 + ((NYC.E.N.HH_Income_Bel75K_e / NYC.E.N.Total_HH_Income_e)^2 * NYC.E.N.Total_HH_Income_s^2))^0.5)),
                                               NYC.E.N.HH_Income_Bel75K_s / NYC.E.N.Total_HH_Income_e)

NYC.E.N.PCT_HH_Disability_e <- 100 * NYC.E.N.HH_Disability_e / NYC.E.N.Total_HH_Dis_e
NYC.E.N.PCT_HH_Disability_s <- 100 * ifelse(NYC.E.N.PCT_HH_Disability_e != 100,
                                            ifelse((NYC.E.N.HH_Disability_s^2 - ((NYC.E.N.HH_Disability_e / NYC.E.N.Total_HH_Dis_e)^2 * NYC.E.N.Total_HH_Dis_s^2)) > 0,
                                                   (1 / NYC.E.N.Total_HH_Dis_e) * ((NYC.E.N.HH_Disability_s^2 - ((NYC.E.N.HH_Disability_e / NYC.E.N.Total_HH_Dis_e)^2 * NYC.E.N.Total_HH_Dis_s^2))^0.5),
                                                   (1 / NYC.E.N.Total_HH_Dis_e) * ((NYC.E.N.HH_Disability_s^2 + ((NYC.E.N.HH_Disability_e / NYC.E.N.Total_HH_Dis_e)^2 * NYC.E.N.Total_HH_Dis_s^2))^0.5)),
                                            NYC.E.N.HH_Disability_s / NYC.E.N.Total_HH_Dis_e)

NYC.E.N.PCT_RB_e <- 100 * NYC.E.N.RB_HH_e / NYC.E.N.Total_Rent_HH_e
NYC.E.N.PCT_RB_s <- 100 * ifelse(NYC.E.N.PCT_RB_e != 100,
                                 ifelse((NYC.E.N.RB_HH_s^2 - ((NYC.E.N.RB_HH_e / NYC.E.N.Total_Rent_HH_e)^2 * NYC.E.N.Total_Rent_HH_s^2)) > 0,
                                        (1 / NYC.E.N.Total_Rent_HH_e) * ((NYC.E.N.RB_HH_s^2 - ((NYC.E.N.RB_HH_e / NYC.E.N.Total_Rent_HH_e)^2 * NYC.E.N.Total_Rent_HH_s^2))^0.5),
                                        (1 / NYC.E.N.Total_Rent_HH_e) * ((NYC.E.N.RB_HH_s^2 + ((NYC.E.N.RB_HH_e / NYC.E.N.Total_Rent_HH_e)^2 * NYC.E.N.Total_Rent_HH_s^2))^0.5)),
                                 NYC.E.N.RB_HH_s / NYC.E.N.Total_Rent_HH_e)

NYC.E.N.PCT_CB_e <- 100 * NYC.E.N.CB_HH_e / NYC.E.N.Total_Owned_HH_e
NYC.E.N.PCT_CB_s <- 100 * ifelse(NYC.E.N.PCT_CB_e != 100,
                                 ifelse((NYC.E.N.CB_HH_s^2 - ((NYC.E.N.CB_HH_e / NYC.E.N.Total_Owned_HH_e)^2 * NYC.E.N.Total_Owned_HH_s^2)) > 0,
                                        (1 / NYC.E.N.Total_Owned_HH_e) * ((NYC.E.N.CB_HH_s^2 - ((NYC.E.N.CB_HH_e / NYC.E.N.Total_Owned_HH_e)^2 * NYC.E.N.Total_Owned_HH_s^2))^0.5),
                                        (1 / NYC.E.N.Total_Owned_HH_e) * ((NYC.E.N.CB_HH_s^2 + ((NYC.E.N.CB_HH_e / NYC.E.N.Total_Owned_HH_e)^2 * NYC.E.N.Total_Owned_HH_s^2))^0.5)),
                                 NYC.E.N.CB_HH_s / NYC.E.N.Total_Owned_HH_e)

# Now write final CityWide row

NYC_row_data <- list("NYC", 
                  sum_NONA(final_table[,"Total_area"]),
                  sum_NONA(final_table[,"M.Total_area"]),
                  100 * sum_NONA(final_table[,"M.Total_area"]) / sum_NONA(final_table[,"Total_area"]),
                  sum_NONA(final_table[,"E.Total_area"]),
                  100 * sum_NONA(final_table[,"E.Total_area"]) / sum_NONA(final_table[,"Total_area"]),
                  sum_NONA(final_table[,"Total_road_area"]),
                  sum_NONA(final_table[,"M.Total_road_area"]),
                  100 * sum_NONA(final_table[,"M.Total_road_area"] / sum_NONA(final_table[,"Total_road_area"])),
                  sum_NONA(final_table[,"E.Total_road_area"]),
                  100 * sum_NONA(final_table[,"E.Total_road_area"] / sum_NONA(final_table[,"Total_road_area"])),
                  
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
                  
                  sum_NONA(final_table[,"M.Total_residential_lots"]),
                  sum_NONA(final_table[,"M.Total_basement_residential_lots"]),
                  sum_NONA(final_table[,"M.Total_mixed_comres_lots"]),
                  sum_NONA(final_table[,"M.Total_commercial_lots"]),
                  sum_NONA(final_table[,"M.Total_industrial_lots"]),
                  
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

# lets break the final table, and save facility subgroup data separately. Otherwise my brain is exploding

final_table_facility_subgroups <- final_table[,c(1,124:198)]

write.csv(final_table_facility_subgroups,
          row.names = FALSE,
          "data/3_output/stormwater_analysis_final_database_facility_subgroups.csv")

final_table <- final_table[,-(124:198)]

final_table_NYC <- rbind(final_table, NYC_row_data)

write.csv(final_table_NYC,
          row.names = FALSE,
          "data/3_output/stormwater_analysis_final_database.csv")

gc()

#### RUN THIS LINE TO SAVE AN NYC SUMMARY
source("src/NYCF_summary_tables.R")

### NOW WE RUN THIS TO CREATE INDEX DATA FOR M AND E SCENARIOS



#### barplots // testing significance

data_plot <- read_xlsx("C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/PostProcess_stormwater_analysis_final_database_NOSLR.xlsx", sheet = "ParaPlotear") %>%
  rename_all(make.names) %>% filter(CD != "NYC")

data_plot$Scenario <- factor(data_plot$Scenario, levels = c("Moderate", "Extreme", "Total"))

plot.P_Area_Flooded <- ggplot(data_plot[data_plot$Scenario != "Total",], aes(fill = Scenario, y = PCT_Area_Flooded, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=paste0(round((Area_Flooded/10000),1), "ha")), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Total Area Flooded") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))


plot.P_TotPop_Flooded <- ggplot(data_plot[data_plot$Scenario != "Total",],
                                aes(fill = Scenario, y = PCT_Total_Population, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Total_Population), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Total Population \n Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Res_Flooded <- ggplot(data_plot[data_plot$Scenario != "Total",],
                              aes(fill = Scenario, y = PCT_Residential_Lots, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Residential_Lots), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Residential Lots Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))


plot.P_ResBsmnt_Flooded <- ggplot(data_plot[data_plot$Scenario != "Total",],
                             aes(fill = Scenario, y = PCT_Residential_Lots_Below_Grade_Basement, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Residential_Lots_Below_Grade_Basement), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Residential Lots With A \n BG Basement Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Mixed_Floded <- ggplot(data_plot[data_plot$Scenario != "Total",],
                              aes(fill = Scenario, y = PCT_Mixed_Residential_Commercial_Lots, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Mixed_Residential_Commercial_lots), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Mixed Residential & Commercial \n Lots Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Commercial_Flooded <- ggplot(data_plot[data_plot$Scenario != "Total",],
                                    aes(fill = Scenario, y = PCT_Commercial_Lots, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Commercial_Lots), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Commercial Lots Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))


plot.P_Industrial_Flooded <- ggplot(data_plot[data_plot$Scenario != "Total",],
                                    aes(fill = Scenario, y = PCT_Industrial_Lots, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Industrial_Lots), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Industrial Lots Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

frame <- ggarrange(plot.P_Res_Flooded,
                   plot.P_ResBsmnt_Flooded, 
                   plot.P_Mixed_Floded,
                   plot.P_Commercial_Flooded,
                   plot.P_Industrial_Flooded,
                   plot.P_TotPop_Flooded,
                   common.legend = TRUE, nrow = 2, ncol = 3, label.y = 1, legend = "bottom", align = "hv") %>%
  annotate_figure(top = text_grob("\n Land Use Exposure Indicators and Population Impacted Per Community District \n", 
                                  color = "black", face = "bold", size = 25))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/barplots_LU_exposure.png", width = 1282, height = 1300) 

plot.P_Road_Flooded <- ggplot(data_plot[data_plot$Scenario != "Total",], 
                              aes(fill = Scenario, y = PCT_Road_Area_Flooded, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=paste0(round((Road_Area_Flooded/10000),1), "ha")), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Road Area Flooded") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Bus_Stops <- ggplot(data_plot[data_plot$Scenario != "Total",], 
                               aes(fill = Scenario, y = PCT_Bus_Stops, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Bus_Stops), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Bus Stops Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Bus_Routes <- ggplot(data_plot[data_plot$Scenario != "Total",], 
                            aes(fill = Scenario, y = PCT_Bus_Routes, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Bus_Routes), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Bus Routes Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.Subway_Entrances <- ggplot(data_plot[data_plot$Scenario != "Total",], 
                                aes(fill = Scenario, y = PCT_Subway_Entrances, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Subway_Entrances), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Subway Entrances Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        #axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

frame <- ggarrange(plot.P_Road_Flooded,
                   plot.P_Bus_Stops, 
                   plot.P_Bus_Routes,
                   plot.Subway_Entrances,
                   common.legend = TRUE, nrow = 2, ncol = 2, label.y = 1, legend = "bottom", align = "hv") %>%
  annotate_figure(top = text_grob("\n Transportation Exposure Indicators Per Community District \n", 
                                  color = "black", face = "bold", size = 25))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/barplots_Transport_exposure.png", width = 1282, height = 1300) 

plot.P_Fac_Public_Safety <- ggplot(data_plot[data_plot$Scenario != "Total",],
                                  aes(fill = Scenario, y = PCT_Public_Safety_Facilities, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Emergency_and_Public_Safety_Facilities), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Public Safety \n Facilities Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Fac_Healthcare <- ggplot(data_plot[data_plot$Scenario != "Total",],
                         aes(fill = Scenario, y = PCT_Healthcare_Facilities, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Healthcare_Facilities), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Healthcare \n Facilities Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Fac_Education <- ggplot(data_plot[data_plot$Scenario != "Total",],
                            aes(fill = Scenario, y = PCT_Education_Facilities, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Education_Facilities), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Education \n Facilities Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Fac_Welfare <- ggplot(data_plot[data_plot$Scenario != "Total",],
                           aes(fill = Scenario, y = PCT_Human_Welfare_Services_Facilities, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Human_Welfare_Services_Facilities), position=position_dodge(width=0.9), vjust=-0.4) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Human Welfare \n Facilities Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        # axis.text.x = element_blank(),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

frame <- ggarrange(plot.P_Fac_Public_Safety,
                   plot.P_Fac_Healthcare, 
                   plot.P_Fac_Education,
                   plot.P_Fac_Welfare,
                   common.legend = TRUE, nrow = 2, ncol = 2, label.y = 1, legend = "bottom", align = "hv") %>%
  annotate_figure(top = text_grob("\n Critical Facilities Exposure Indicators Per Community District \n", 
                                  color = "black", face = "bold", size = 25))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/barplots_Facilities_exposure.png", width = 1282, height = 1300) 



frame <- ggarrange(plot.P_Road_Flooded, 
                   plot.P_Res_Flooded,
                   plot.P_ResBsmnt_Flooded, 
                   plot.P_EmFac_Flooded, 
                   plot.P_HealthEdFac_Flooded,
                   common.legend = TRUE, nrow = 2, ncol = 3, label.y = 1, legend = "bottom", align = "hv") %>%
  annotate_figure(top = text_grob("\n Exposure Indicators Per Community District \n", 
                                  color = "black", face = "bold", size = 25))


frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/barplots_exposure.png", width = 1282, height = 1300) 


plot.P_Children_Flooded <- ggplot(data_plot,
                                aes(fill = Scenario, y = PCT_Children, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Children") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Elderly_Flooded <- ggplot(data_plot,
                                  aes(fill = Scenario, y = PCT_Elderly, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Elderly") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_AfAm_Flooded <- ggplot(data_plot,
                                 aes(fill = Scenario, y = PCT_African_American, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% African American") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_Hisp_Flooded <- ggplot(data_plot,
                                 aes(fill = Scenario, y = PCT_Hispanic_Latinx, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Hispanic / Latinx Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.P_White_Flooded <- ggplot(data_plot,
                              aes(fill = Scenario, y = PCT_White, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% White") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.PovRate <- ggplot(data_plot,
                               aes(fill = Scenario, y = PovRate_e, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = PovRate_e - PovRate_s, ymax = PovRate_e + PovRate_s), 
                width = 0.2, position=position_dodge(.9)) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Population Below Poverty") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.PCT_NoCar <- ggplot(data_plot,
                       aes(fill = Scenario, y = PCT_NoCar_e, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = PCT_NoCar_e - PCT_NoCar_s, ymax = PCT_NoCar_e + PCT_NoCar_s), 
                width = 0.2, position=position_dodge(.9)) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Households Without a Car") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

plot.PCT_Rented <- ggplot(data_plot,
       aes(fill = Scenario, y = PCT_Rented_e, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = PCT_Rented_e - PCT_Rented_s, ymax = PCT_Rented_e + PCT_Rented_s), 
                width = 0.2, position=position_dodge(.9)) +
  scale_fill_manual(values=c('#3485AD','#004466', "#777678")) + 
  labs( x="Community District", y = "% Occupied Households Rented") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18))

frame <- ggarrange(plot.P_Children_Flooded,
                   plot.P_Elderly_Flooded, 
                   plot.P_AfAm_Flooded,
                   plot.P_Hisp_Flooded,
                   plot.P_White_Flooded,
                   plot.PovRate,
                   plot.PCT_NoCar,
                   plot.PCT_Rented,
                   common.legend = TRUE, nrow = 4, ncol = 2, label.y = 1, legend = "bottom", align = "hv") %>%
  annotate_figure(top = text_grob("\n Social Vulnerability Indicators Per Community District \n", 
                                  color = "black", face = "bold", size = 25))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/barplots_sv_demo.png", width = 1282, height = 1300) 


data_acs_test <- data_plot[, grepl(c("CD|Scenario|_e|_s"), names(data_plot))]

df_sig_test <- data.frame(CD = unique(data_acs_test$CD))

Mod_Ext <- c("Moderate", "Extreme")
Mod_Tot <- c("Moderate", "Total")
Ext_Tot <- c("Extreme", "Total")

for(CD in unique(df_sig_test$CD)){
  
  for(estimate in names(data_acs_test[grepl("_e", names(data_acs_test))])){
    
    estimate_2 <- str_remove(estimate, "_e")
    
    data_estimate <- cbind(select(data_acs_test, c(CD, Scenario)), data_acs_test[, grepl(estimate_2, names(data_acs_test))])
    
    A <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Mod_Ext[1], estimate])
    B <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Mod_Ext[2], estimate])
    SE_A <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Mod_Ext[1], paste0(estimate_2, "_s")])
    SE_B <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Mod_Ext[2], paste0(estimate_2, "_s")])
                       
    Z_Mod_Ext <- (A-B)/((SE_A^2 + SE_B^2)^0.5)
    
    A <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Mod_Tot[1], estimate])
    B <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Mod_Tot[2], estimate])
    SE_A <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Mod_Tot[1], paste0(estimate_2, "_s")])
    SE_B <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Mod_Tot[2], paste0(estimate_2, "_s")])
    
    Z_Mod_Tot <- (A-B)/((SE_A^2 + SE_B^2)^0.5)
    
    A <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Ext_Tot[1], estimate])
    B <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Ext_Tot[2], estimate])
    SE_A <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Ext_Tot[1], paste0(estimate_2, "_s")])
    SE_B <- as.numeric(data_acs_test[data_acs_test$CD == CD & data_acs_test$Scenario == Ext_Tot[2], paste0(estimate_2, "_s")])
    
    Z_Ext_Tot <- (A-B)/((SE_A^2 + SE_B^2)^0.5)
    
    df_sig_test[df_sig_test$CD == CD, paste0(estimate_2, "_", Mod_Ext[1],"_", Mod_Ext[2])] <- Z_Mod_Ext
    df_sig_test[df_sig_test$CD == CD, paste0(estimate_2, "_", Mod_Tot[1],"_", Mod_Tot[2])] <- Z_Mod_Tot
    df_sig_test[df_sig_test$CD == CD, paste0(estimate_2, "_", Ext_Tot[1],"_", Ext_Tot[2])] <- Z_Ext_Tot
  }
  
}


# Now the same step but prepping a dataset with the SOVI data of ALL the CDs

final_table <- read_csv("C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/stormwater_analysis_final_database_NOSLR.csv") %>% 
  filter(Geography != "NYC")

template <- data.frame(CD = final_table$Geography)

# moderate

moderate <- template %>%
  mutate(Scenario = "Moderate",
         PCT_Children = final_table$`M.Flooded PCT 0-5 children`,
         PCT_Elderly = final_table$`M.Flooded PCT +65 elder`,
         PCT_African_American = final_table$`M.Flooded PCT Black`,
         PCT_Hispanic_Latinx = final_table$`M.Flooded PCT Hispanic`,
         PCT_White = final_table$`M.Flooded PCT White`,
         PovRate_e = final_table$M.Pov_rate_e,
         PovRate_s = final_table$M.Pov_rate_s,
         PCT_NoCar_e = final_table$M.PCT_No_Car_e,
         PCT_NoCar_s = final_table$M.PCT_No_Car_s,
         PCT_Rented_e = final_table$M.PCT_rented_e,
         PCT_Rented_s = final_table$M.PCT_rented_s)

moderate_unexposed <- template %>%
  mutate(Scenario = "Moderate Unexposed",
         PCT_Children = final_table$`M.N.Flooded PCT 0-5 children`,
         PCT_Elderly = final_table$`M.N.Flooded PCT +65 elder`,
         PCT_African_American = final_table$`M.N.Flooded PCT Black`,
         PCT_Hispanic_Latinx = final_table$`M.N.Flooded PCT Hispanic`,
         PCT_White = final_table$`M.N.Flooded PCT White`,
         PovRate_e = final_table$M.N.Pov_rate_e,
         PovRate_s = final_table$M.N.Pov_rate_s,
         PCT_NoCar_e = final_table$M.N.PCT_No_Car_e,
         PCT_NoCar_s = final_table$M.N.PCT_No_Car_s,
         PCT_Rented_e = final_table$M.N.PCT_rented_e,
         PCT_Rented_s = final_table$M.N.PCT_rented_s)

# extreme

extreme <- template %>%
  mutate(Scenario = "Extreme",
         PCT_Children = final_table$`E.Flooded PCT 0-5 children`,
         PCT_Elderly = final_table$`E.Flooded PCT +65 elder`,
         PCT_African_American = final_table$`E.Flooded PCT Black`,
         PCT_Hispanic_Latinx = final_table$`E.Flooded PCT Hispanic`,
         PCT_White = final_table$`E.Flooded PCT White`,
         PovRate_e = final_table$E.Pov_rate_e,
         PovRate_s = final_table$E.Pov_rate_s,
         PCT_NoCar_e = final_table$E.PCT_No_Car_e,
         PCT_NoCar_s = final_table$E.PCT_No_Car_s,
         PCT_Rented_e = final_table$E.PCT_rented_e,
         PCT_Rented_s = final_table$E.PCT_rented_s)

extreme_unexposed <- template %>%
  mutate(Scenario = "Extreme Unexposed",
         PCT_Children = final_table$`E.N.Flooded PCT 0-5 children`,
         PCT_Elderly = final_table$`E.N.Flooded PCT +65 elder`,
         PCT_African_American = final_table$`E.N.Flooded PCT Black`,
         PCT_Hispanic_Latinx = final_table$`E.N.Flooded PCT Hispanic`,
         PCT_White = final_table$`E.N.Flooded PCT White`,
         PovRate_e = final_table$E.N.Pov_rate_e,
         PovRate_s = final_table$E.N.Pov_rate_s,
         PCT_NoCar_e = final_table$E.N.PCT_No_Car_e,
         PCT_NoCar_s = final_table$E.N.PCT_No_Car_s,
         PCT_Rented_e = final_table$E.N.PCT_rented_e,
         PCT_Rented_s = final_table$E.N.PCT_rented_s)

# total

total <- template %>%
  mutate(Scenario = "Total",
         PCT_Children = final_table$`PCT 0-5 children`,
         PCT_Elderly = final_table$`PCT +65 elder`,
         PCT_African_American = final_table$`PCT Black`,
         PCT_Hispanic_Latinx = final_table$`PCT Hispanic`,
         PCT_White = final_table$`PCT White`,
         PovRate_e = final_table$Pov_rate_e,
         PovRate_s = final_table$Pov_rate_s,
         PCT_NoCar_e = final_table$PCT_No_Car_e,
         PCT_NoCar_s = final_table$PCT_No_Car_s,
         PCT_Rented_e = final_table$PCT_rented_e,
         PCT_Rented_s = final_table$PCT_rented_s)

SOVI_plot_m <- rbind(moderate, moderate_unexposed, total)

SOVI_plot_m$Scenario <- factor(SOVI_plot_m$Scenario, levels = c("Moderate", 
                                                            "Moderate Unexposed", 
                                                            "Extreme",
                                                            "Extreme Unexposed",
                                                            "Total"))

SOVI_plot.P_Children_Flooded <- ggplot(SOVI_plot_m,
                                  aes(fill = Scenario, y = PCT_Children, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD','#ADBC7A', "#777678")) + 
  labs( x="\n Community District", y = "% Children \n") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=28, face = "bold"),
        legend.text = element_text(size=24),
        axis.title=element_text(size=30),
        legend.position = "bottom")

SOVI_plot.P_Children_Flooded %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/m_exposure_barplots_sv_all_children.png", width = 2000, height = 800) 

SOVI_plot.P_Elderly_Flooded <- ggplot(SOVI_plot_m,
                                       aes(fill = Scenario, y = PCT_Elderly, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD','#ADBC7A', "#777678")) + 
  labs( x="\n Community District", y = "% Elderly \n") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=28, face = "bold"),
        legend.text = element_text(size=24),
        axis.title=element_text(size=30),
        legend.position = "bottom")

SOVI_plot.P_Elderly_Flooded %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/m_exposure_barplots_sv_all_elderly.png", width = 2000, height = 800) 

SOVI_plot.P_AfAm_Flooded <- ggplot(SOVI_plot_m,
                                      aes(fill = Scenario, y = PCT_African_American, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD', '#ADBC7A', "#777678")) + 
  labs( x="\n Community District", y = "% African American \n") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=28, face = "bold"),
        legend.text = element_text(size=24),
        axis.title=element_text(size=30),
        legend.position = "bottom")

SOVI_plot.P_AfAm_Flooded %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/m_exposure_barplots_sv_all_AfAm.png", width = 2000, height = 800) 

SOVI_plot.P_Hisp_Flooded <- ggplot(SOVI_plot_m,
                                   aes(fill = Scenario, y = PCT_Hispanic_Latinx, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD','#ADBC7A', "#777678")) + 
  labs( x="\n Community District", y = "% Hispanic / Latinx \n") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=28, face = "bold"),
        legend.text = element_text(size=24),
        axis.title=element_text(size=30),
        legend.position = "bottom")

SOVI_plot.P_Hisp_Flooded %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/m_exposure_barplots_sv_all_Hisp.png", width = 2000, height = 800) 

SOVI_plot.P_White_Flooded <- ggplot(SOVI_plot_m,
                                   aes(fill = Scenario, y = PCT_White, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c('#3485AD', '#ADBC7A', "#777678")) + 
  labs( x="\n Community District", y = "% White \n") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=28, face = "bold"),
        legend.text = element_text(size=24),
        axis.title=element_text(size=30),
        legend.position = "bottom")

SOVI_plot.P_White_Flooded %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/m_exposure_barplots_sv_all_White.png", width = 2000, height = 800) 

SOVI_plot.PovRate <- ggplot(SOVI_plot_m,
                       aes(fill = Scenario, y = PovRate_e, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = PovRate_e - PovRate_s, ymax = PovRate_e + PovRate_s), 
                width = 0.2, position=position_dodge(.9)) +
  scale_fill_manual(values=c('#3485AD', '#ADBC7A', "#777678")) + 
  labs( x="\n Community District", y = "% Population Below Poverty \n") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=28, face = "bold"),
        legend.text = element_text(size=24),
        axis.title=element_text(size=30),
        legend.position = "bottom")

SOVI_plot.PovRate %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/m_exposure_barplots_sv_all_Poverty.png", width = 2000, height = 800) 

SOVI_plot.PCT_NoCar <- ggplot(SOVI_plot_m,
                       aes(fill = Scenario, y = PCT_NoCar_e, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = PCT_NoCar_e - PCT_NoCar_s, ymax = PCT_NoCar_e + PCT_NoCar_s), 
                width = 0.2, position=position_dodge(.9)) +
  scale_fill_manual(values=c('#3485AD','#ADBC7A', "#777678")) + 
  labs( x="\n Community District", y = "% Households Without a Car \n") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=28, face = "bold"),
        legend.text = element_text(size=24),
        axis.title=element_text(size=30),
        legend.position = "bottom")

SOVI_plot.PCT_NoCar %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/m_exposure_barplots_sv_all_NoCar.png", width = 2000, height = 800) 

SOVI_plot.PCT_Rented <- ggplot(SOVI_plot_m,
                              aes(fill = Scenario, y = PCT_NoCar_e, x = CD)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin = PCT_NoCar_e - PCT_NoCar_s, ymax = PCT_NoCar_e + PCT_NoCar_s), 
                width = 0.2, position=position_dodge(.9)) +
  scale_fill_manual(values=c('#3485AD','#ADBC7A', "#777678")) + 
  labs( x="\n Community District", y = "% Rented Households \n") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        # axis.title.x = element_blank(),
        text=element_text(size=20),
        legend.title = element_text(size=28, face = "bold"),
        legend.text = element_text(size=24),
        axis.title=element_text(size=30),
        legend.position = "bottom")

SOVI_plot.PCT_Rented %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/m_exposure_barplots_sv_all_Rented.png", width = 2000, height = 800) 
