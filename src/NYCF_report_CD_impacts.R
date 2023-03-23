
# Run script ONCE to save decennial census data
# source(".src/NYCF_decennial_census_data.R")

# Run script ONCE to save tax lot data
# source(".src/NYCF_tax_lots_data.R")

source(".src/NYCF_housekeeping_GIS_vars.R")

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

NYC_flooding_blocks.joined.CDs <- st_read("data/2_intermediate/NYC_blocks_CD_demo_flood.shp")

demographic_summary <- (NYC_flooding_blocks.joined.CDs) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(Total_population = sum(Total, na.rm = TRUE),
                   Total_black_population = sum(AfAm, na.rm = TRUE),
                   Total_hispanic_population = sum(Latinx, na.rm = TRUE),
                   Total_white_population = sum(White, na.rm = TRUE),
                   PCT_black_population = (100 * sum(AfAm, na.rm = TRUE) / sum(Total, na.rm = TRUE)),
                   PCT_hispanic_population = 100 * sum(Latinx, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   PCT_White_population = 100 * sum(White, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   Total_below5_population = sum(b5, na.rm = TRUE),
                   Total_above65_population = sum(a65, na.rm = TRUE),
                   PCT_below5_population = 100* sum(b5, na.rm = TRUE) / sum(Total, na.rm = TRUE),
                   PCT_above65_population = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE)) %>%
  st_drop_geometry()

### Write demographic variables in the final table!
final_table <- inner_join(final_table, demographic_summary, by = c("Geography" = "boro_cd"))

### Now demographic data for flooded census blocks

m.exposed_population <- filter(NYC_flooding_blocks.joined.CDs, m_d_f == 0)
m.unexposed_population <- filter(NYC_flooding_blocks.joined.CDs, m_d_f > 0)

e.exposed_population <- filter(NYC_flooding_blocks.joined.CDs, e_d_f == 0)
e.unexposed_population <- filter(NYC_flooding_blocks.joined.CDs, e_d_f > 0)

M.demographic_summary <- (m.exposed_population)  %>%
  st_drop_geometry() %>% 
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
                   "M.PCT_above65_population" = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE))

M.N.demographic_summary <- (m.unexposed_population) %>%
  st_drop_geometry() %>% 
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
                   "M.N.PCT_above65_population" = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE))

E.demographic_summary <- (e.exposed_population) %>%
  st_drop_geometry() %>% 
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
                   "E.PCT_above65_population" = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE))

E.N.demographic_summary <- (e.unexposed_population) %>%
  st_drop_geometry() %>% 
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
                   "E.N.PCT_above65_population" = 100 * sum(a65, na.rm = TRUE) / sum (Total, na.rm = TRUE))

Flood_summary_all <- left_join(M.demographic_summary, M.N.demographic_summary) %>%
  left_join(E.demographic_summary) %>%
  left_join(E.N.demographic_summary)

Flood_summary_all[is.na(Flood_summary_all)] <- 0

final_table <- left_join(final_table, Flood_summary_all, by = c("Geography" = "boro_cd"))

tax_lots_flooding <- st_read("data/2_intermediate/tax_lots_CD_flood.shp") %>%
  st_drop_geometry() %>%
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))
  
tax_lots_flooding_summary <- tax_lots_flooding %>%
  group_by(boro_cd) %>%
  summarize(Total_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"),
            Total_basement_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"),
            Total_mixed_comres_lots = sum(LUseCat %in% "Mixed residential and commercial"),
            Total_commercial_lots = sum(LUseCat %in% "Commercial"),
            Total_industrial_lots = sum(LUseCat %in% "Industrial & manufacturing"),
            M.Total_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= 30),
            M.Total_basement_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& m_d_f <= 30),
            M.Total_mixed_comres_lots = sum(LUseCat %in% "Mixed residential and commercial"& m_d_f <= 30),
            M.Total_commercial_lots = sum(LUseCat %in% "Commercial"& m_d_f <= 30),
            M.Total_industrial_lots = sum(LUseCat %in% "Industrial & manufacturing"& m_d_f <= 30),
            E.Total_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= 30),
            E.Total_basement_residential_lots = sum(LUseCat %in% "Residential" &  BSMT_RES != "Basement in residential lot"& e_d_f <= 30),
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

### Critical infra --> calculate min dist to each flooding scenario

facilities_data_points <- st_read("C:/Users/herrerop/Desktop/GIS/Original_Data/NYC/NYC_Facilities/facilities_20210811shp") %>%
  filter(FACSUBGRP %in% c("POLICE SERVICES", 
                           "OTHER PUBLIC SAFETY",
                           "FIRE SERVICES",
                           "OTHER EMERGENCY SERVICES",
                           "HOSPITALS AND CLINICS", 
                           "MENTAL HEALTH",
                           "RESIDENTIAL HEALTH CARE",
                           "CHEMICAL DEPENDENCY",
                           "HEALTH PROMOTION AND DISEASE PREVENTION",
                           "OTHER HEALTH CARE",
                           "SENIOR SERVICES",
                           "COMMUNITY CENTERS AND COMMUNITY SCHOOL PROGRAMS",
                           "FINANCIAL ASSISTANCE AND SOCIAL SERVICES",
                           "WORKFORCE DEVELOPMENT",
                           "LEGAL AND INTERVENTION SERVICES",
                           "PROGRAMS FOR PEOPLE WITH DISABILITIES",
                           "PERMANENT SUPPORTIVE SRO HOUSING",
                           "NON-RESIDENTIAL HOUSING AND HOMELESS SERVICES",
                           "SOUP KITCHENS AND FOOD PANTRIES",
                           "PUBLIC K-12 SCHOOLS",
                           "CHARTER K-12 SCHOOLS",
                           "NON-PUBLIC K-12 SCHOOLS",
                           "PUBLIC AND PRIVATE SPECIAL EDUCATION SCHOOLS",
                           "GED AND ALTERNATIVE HIGH SCHOOL EQUIVALENCY",
                           "DOE UNIVERSAL PRE-KINDERGARTEN",
                           "DAY CARE",
                           "PRESCHOOLS FOR STUDENTS WITH DISABILITIES",
                           "DUAL CHILD CARE AND UNIVERSAL PRE-K",
                           "COLLEGES OR UNIVERSITIES",
                           "PROPRIETARY SCHOOLS",
                          "FOSTER CARE SERVICES AND RESIDENTIAL CARE",
                          "PREVENTATIVE CARE, EVALUATION SERVICES, AND RESPITE",
                          "CHILD NUTRITION",
                          "YOUTH CENTERS, LITERACY PROGRAMS, AND JOB TRAINING SERVICES")) %>% 
  dplyr::select(FACNAME,BBL,CD,BOROCODE,FACSUBGRP,FACGROUP) %>%
  st_transform(UTM_18N_meter) %>% 
  mutate(FACSUBGRP = case_when(FACSUBGRP %in% c("OTHER PUBLIC SAFETY",
                                                "OTHER EMERGENCY SERVICES") ~ "OTHER SAFETY AND EMERGENCY FACILITIES",
                               FACSUBGRP %in% c("MENTAL HEALTH",
                                                "CHEMICAL DEPENDENCY",
                                                "HEALTH PROMOTION AND DISEASE PREVENTION") ~ "MENTAL HEALTH, HEALTH PROMOTION, AND CHEMICAL DEPENDENCY SERVICES",
                               FACSUBGRP %in% c("COMMUNITY CENTERS AND COMMUNITY SCHOOL PROGRAMS",
                                                "FINANCIAL ASSISTANCE AND SOCIAL SERVICES",
                                                "WORKFORCE DEVELOPMENT",
                                                "LEGAL AND INTERVENTION SERVICES",
                                                "YOUTH CENTERS, LITERACY PROGRAMS, AND JOB TRAINING SERVICES") ~ "HUMAN DEVELOPMENT SERVICES",
                               FACSUBGRP %in% c("SENIOR SERVICES",
                                                "PROGRAMS FOR PEOPLE WITH DISABILITIES") ~ "SENIOR AND DISSABILITY SERVICES",
                               FACSUBGRP %in% c("PERMANENT SUPPORTIVE SRO HOUSING",
                                                "NON-RESIDENTIAL HOUSING AND HOMELESS SERVICES",
                                                "SOUP KITCHENS AND FOOD PANTRIES") ~ "HOUSING AND FOOD SERVICES",
                               FACSUBGRP %in% c("FOSTER CARE SERVICES AND RESIDENTIAL CARE",
                                                "CHILD NUTRITION") ~ "CHILD SERVICES AND WELFARE SERVICES",
                               FACSUBGRP %in% c("PUBLIC K-12 SCHOOLS",
                                                "CHARTER K-12 SCHOOLS",
                                                "NON-PUBLIC K-12 SCHOOLS",
                                                "PUBLIC AND PRIVATE SPECIAL EDUCATION SCHOOLS",
                                                "GED AND ALTERNATIVE HIGH SCHOOL EQUIVALENCY") ~ "K-12 EDUCATION SERVICES",
                               FACSUBGRP %in% c("DOE UNIVERSAL PRE-KINDERGARTEN",
                                                "DAY CARE",
                                                "PRESCHOOLS FOR STUDENTS WITH DISABILITIES",
                                                "DUAL CHILD CARE AND UNIVERSAL PRE-K") ~ "DAY CARE AND PRE-KINDERGARTEN",
                               TRUE ~ paste0(.$FACSUBGRP, ""))) %>% 
  mutate(CatFac = case_when(FACSUBGRP %in% c("POLICE SERVICES", 
                                             "FIRE SERVICES",
                                             "OTHER SAFETY AND EMERGENCY FACILITIES") ~ "Public Safety",
                            FACSUBGRP %in% c("HOSPITALS AND CLINICS", 
                                             "RESIDENTIAL HEALTH CARE",
                                             "MENTAL HEALTH, HEALTH PROMOTION, AND CHEMICAL DEPENDENCY SERVICES",
                                             "OTHER HEALTH CARE") ~ "Healthcare",
                            FACSUBGRP %in% c("HUMAN DEVELOPMENT SERVICES",
                                             "SENIOR AND DISSABILITY SERVICES",
                                             "HOUSING AND FOOD SERVICES",
                                             "CHILD SERVICES AND WELFARE SERVICES") ~ "Human Welfare Services",
                            FACSUBGRP %in% c("K-12 EDUCATION SERVICES",
                                             "DAY CARE AND PRE-KINDERGARTEN",
                                             "COLLEGES OR UNIVERSITIES",
                                             "PROPRIETARY SCHOOLS") ~ "Education"
                            )
         ) %>% filter(CD %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))
  

facilities_data_points <- write_closest_flooding(facilities_data_points, 
                                                 moderate_flooding,
                                                 "m_d_f",
                                                 centroids = FALSE)

facilities_data_points <- write_closest_flooding(facilities_data_points, 
                                                 extreme_flooding,
                                                 "e_d_f",
                                                 centroids = FALSE)


facility_categories <- unique(facilities_data_points$CatFac)
facility_subgroups <- unique(facilities_data_points$FACSUBGRP)

 for(facility_cat in facility_categories){
  
  print(paste0("   ", facility_cat))
  
  sliced_catfac <- facilities_data_points[facilities_data_points$CatFac == facility_cat,]
  
  for(comdist in final_table$Geography){
    
    sliced_catfac_CD <- sliced_catfac[sliced_catfac$CD == comdist,]
    
    total_nr_facility <- nrow(sliced_catfac_CD)
    M.Flooded_nr_facility <-nrow(sliced_catfac_CD[sliced_catfac_CD$m_d_f <= 30,])
    E.Flooded_nr_facility <-nrow(sliced_catfac_CD[sliced_catfac_CD$e_d_f <= 30,])
    M.Flooded_PCT_facility <- 100 * M.Flooded_nr_facility  / total_nr_facility
    E.Flooded_PCT_facility <- 100 * E.Flooded_nr_facility  / total_nr_facility
    
    final_table[final_table$Geography == comdist, paste0("Total number of ", facility_cat, " facilities")] <- total_nr_facility
    
    final_table[final_table$Geography == comdist, paste0("M.Flooded number of ", facility_cat, " facilities")] <- M.Flooded_nr_facility
    final_table[final_table$Geography == comdist, paste0("E.Flooded number of ", facility_cat, " facilities")] <- E.Flooded_nr_facility
    
    final_table[final_table$Geography == comdist, paste0("M.Flooded PCT of ", facility_cat, " facilities")] <- M.Flooded_PCT_facility
    final_table[final_table$Geography == comdist, paste0("E.Flooded PCT of ", facility_cat, " facilities")] <- E.Flooded_PCT_facility
    
  }
  
}


facilities_summary_citywide <- data.frame(SUBGRP = facility_subgroups, "count" = 0, "M.count" = 0, "E.count" = 0, "M.PCT" =0, "E.PCT"=0)

for(facility_subgrp in facility_subgroups){
  
  print(paste0("   ", facility_subgrp))
  
  sliced_facsbgrp <- facilities_data_points[facilities_data_points$FACSUBGRP == facility_subgrp,]
  
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "count"] <- nrow(sliced_facsbgrp)
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "M.count"] <- nrow(sliced_facsbgrp[sliced_facsbgrp$m_d_f <= 30,])
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "E.count"] <- nrow(sliced_facsbgrp[sliced_facsbgrp$e_d_f <= 30,])
  
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "M.PCT"] <- 100 * nrow(sliced_facsbgrp[sliced_facsbgrp$m_d_f <= 30,]) / nrow(sliced_facsbgrp)
  facilities_summary_citywide[facilities_summary_citywide$SUBGRP == facility_subgrp, "E.PCT"] <- 100 * nrow(sliced_facsbgrp[sliced_facsbgrp$e_d_f <= 30,]) / nrow(sliced_facsbgrp)
  
  for(comdist in final_table$Geography){
    
    sliced_facsbgrp_CD <- sliced_facsbgrp[sliced_facsbgrp$CD == comdist,]
    
    total_nr_subgrp <- nrow(sliced_facsbgrp_CD)
    M.Flooded_nr_subgrp <-nrow(sliced_facsbgrp_CD[sliced_facsbgrp_CD$m_d_f <= 30,])
    E.Flooded_nr_subgrp <-nrow(sliced_facsbgrp_CD[sliced_facsbgrp_CD$e_d_f <= 30,])
    M.Flooded_PCT_facility <- 100 * M.Flooded_nr_subgrp  / total_nr_subgrp
    E.Flooded_PCT_facility <- 100 * E.Flooded_nr_subgrp  / total_nr_subgrp
    
    final_table[final_table$Geography == comdist, paste0("Total number of ", facility_subgrp, " facilities")] <- total_nr_subgrp
    
    final_table[final_table$Geography == comdist, paste0("M.Flooded number of ", facility_subgrp, " facilities")] <- M.Flooded_nr_subgrp
    final_table[final_table$Geography == comdist, paste0("E.Flooded number of ", facility_subgrp, " facilities")] <- E.Flooded_nr_subgrp
    
    final_table[final_table$Geography == comdist, paste0("M.Flooded PCT of ", facility_subgrp, " facilities")] <- M.Flooded_PCT_facility
    final_table[final_table$Geography == comdist, paste0("E.Flooded PCT of ", facility_subgrp, " facilities")] <- E.Flooded_PCT_facility
    
  }
  
}

write.csv(facilities_summary_citywide,
          row.names = FALSE,
          "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/stormwater_analysis_final_database_facility_subgroups_CITYWIDE_NOSLR.csv")


### Transportation indicators

bus_stops <- st_read("C:/Users/herrerop/Desktop/GIS/Original_Data/NYC/bus_lines/bus_stops_nyc_nov2020/bus_stops_nyc_nov2020.shp") %>% 
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


subway_entrances <- st_read("C:/Users/herrerop/Desktop/GIS/Original_Data/NYC/Subway_Entrances/geo_export_a9ca2d05-28dc-4a51-9b65-7e17888f49ec.shp") %>% 
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

# loop adding stops and entrances per scenario
for(comdist in unique(CD$boro_cd)){
  
  print(comdist)
  
  bus_stops_CD <- filter(bus_stops, boro_cd == comdist)
  total_nr_BusStops <- nrow(bus_stops_CD)
  
  M.Flooded_nr_BusStops <- nrow(filter(bus_stops_CD, m_d_f <= 30))
  E.Flooded_nr_BusStops <- nrow(filter(bus_stops_CD, e_d_f <= 30))
  
  M.Flooded_PCT_BusStops <- 100 * M.Flooded_nr_BusStops/total_nr_BusStops
  E.Flooded_PCT_BusStops <- 100 * E.Flooded_nr_BusStops/total_nr_BusStops
  
  subway_entrances_CD <- filter(subway_entrances, boro_cd == comdist)
  total_nr_SubwayEntrances <- nrow(subway_entrances_CD)
  
  M.Flooded_nr_SubwayEntrances <- nrow(filter(subway_entrances_CD, m_d_f <= 30))
  E.Flooded_nr_SubwayEntrances <- nrow(filter(subway_entrances_CD, e_d_f <= 30))
  
  M.Flooded_PCT_SubwayEntrances <- ifelse(total_nr_SubwayEntrances > 0, 100 * M.Flooded_nr_SubwayEntrances/total_nr_SubwayEntrances, 0) 
  E.Flooded_PCT_SubwayEntrances <- ifelse(total_nr_SubwayEntrances > 0, 100 * E.Flooded_nr_SubwayEntrances/total_nr_SubwayEntrances ,0) 
  
  final_table[final_table$Geography == comdist, "Total number of bus stops"] <- total_nr_BusStops
  final_table[final_table$Geography == comdist, "M.Flooded number of bus stops"] <- M.Flooded_nr_BusStops
  final_table[final_table$Geography == comdist, "E.Flooded number of bus stops"] <- E.Flooded_nr_BusStops
  final_table[final_table$Geography == comdist, "M.Flooded PCT bus stops"] <- M.Flooded_PCT_BusStops
  final_table[final_table$Geography == comdist, "E.Flooded PCT bus stops"] <- E.Flooded_PCT_BusStops
  
  final_table[final_table$Geography == comdist, "Total number of subway entrances"] <- total_nr_SubwayEntrances
  final_table[final_table$Geography == comdist, "M.Flooded number of subway entrances"] <- M.Flooded_nr_SubwayEntrances
  final_table[final_table$Geography == comdist, "E.Flooded number of subway entrances"] <- E.Flooded_nr_SubwayEntrances
  final_table[final_table$Geography == comdist, "M.Flooded PCT subway entrances"] <- M.Flooded_PCT_SubwayEntrances
  final_table[final_table$Geography == comdist, "E.Flooded PCT subway entrances"] <- E.Flooded_PCT_SubwayEntrances
  
  }

bus_routes <- st_read("C:/Users/herrerop/Desktop/GIS/Original_Data/NYC/bus_lines/bus_routes_nyc_nov2020/bus_routes_nyc_nov2020.shp") %>% 
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
  
  final_table[final_table$Geography == comdist, "Total number of bus routes"] <- total_nr_routes
  
  final_table[final_table$Geography == comdist, "M.Flooded number of bus routes"] <- M.Flooded_nr_routes
  final_table[final_table$Geography == comdist, "E.Flooded number of bus routes"] <- E.Flooded_nr_routes
  
  final_table[final_table$Geography == comdist, "M.Flooded PCT bus routes"] <- round(M.Flooded_PCT_routes, 1)
  final_table[final_table$Geography == comdist, "E.Flooded PCT bus routes"] <- round(E.Flooded_PCT_routes, 1)
  
  }

### % roads flooded

roads <- st_read("C:/Users/herrerop/Desktop/GIS/Original_Data/NYC/NYC Street Centerline (CSCL)/geo_export_4443d165-7281-4b57-a1a1-f651c232b7ab.shp") %>%
  st_transform(UTM_18N_meter) %>%
  filter(rw_type %nin% c(2,3,9, 14, 4)) # IM ALSO REMOVING 3 (bridges)because flooding will show the underpass flooding  and 14 (ferry routes) and 4 (tunnels) because flooding is in the surface

for (cd_code in CD$boro_cd){
  print(cd_code)
  
  AOI <- CD[CD$boro_cd == cd_code,]
  
  roads_cd <- roads[AOI, ]
  
  roads_cd.buf <- st_buffer(roads_cd, (roads_cd$st_width / (2*3.281)), endCapStyle = "FLAT") %>% 
    st_make_valid()
  
  roads_cd.buf.int <- st_intersection(roads_cd.buf, AOI)
  
  roads_cd.buf.int.union <- roads_cd.buf.int %>% st_union() %>% st_make_valid() %>% st_cast("POLYGON")
  
  CD[CD$boro_cd == cd_code, "Total road area"] <- sum(st_area(roads_cd.buf.int.union))
  
  moderate_flooding.int.road <- st_intersection(moderate_flooding, roads_cd.buf.int.union)
  
  CD[CD$boro_cd == cd_code, "M.Flooded road area"] <- sum(st_area(moderate_flooding.int.road))
  CD[CD$boro_cd == cd_code, "M.Flooded PCT road"] <- 100 * sum(st_area(moderate_flooding.int.road)) / sum(st_area(roads_cd.buf.int.union))
  
  extreme_flooding.int.road <-  st_intersection(extreme_flooding, roads_cd.buf.int.union)
  
  CD[CD$boro_cd == cd_code, "E.Flooded road area"] <- sum(st_area(extreme_flooding.int.road))
  CD[CD$boro_cd == cd_code, "E.Flooded PCT road"] <- 100 * sum(st_area(extreme_flooding.int.road)) / sum(st_area(roads_cd.buf.int.union))
  
}

final_table["Total road area"] <- CD$`Total road area`

final_table["M.Flooded road area"] <- CD$`M.Flooded road area`
final_table["M.Flooded PCT road"] <- CD$`M.Flooded PCT road`

final_table["E.Flooded road area"] <- CD$`E.Flooded road area`
final_table["E.Flooded PCT road"] <- CD$`E.Flooded PCT road`

### Calculate poverty rates || could also be people living under intense poverty rates (e.g. 22.82 as per PEJAS criteria or 23.59 as per NYC criteria)

SOVI_data <- st_read("C:/Users/herrerop/Desktop/GIS/Original_Data/ACS_TIGERLINES/BG/2018/ACS_2018_5YR_BG/USA_BGs_2018_vulnerability_final.shp") %>% 
  filter(STATEFP == 36) %>%
  filter(COUNTYF %in% c("005", "047", "061", "081", "085")) %>% 
  filter(ALAND != 0) %>%
  st_transform(UTM_18N_meter) %>% 
  st_make_valid() %>% 
  select(BelPov_e = BelPov_, BelPv_s,
         TotPop_e = TotPop_, TotPp_s,
         TotHH_e, TotHH_s,
         HhNoCr_e = HhNoCr_, HhNCr_s,
         OccHh_e, OccHh_s,
         Rented_e = Rented_, Rentd_s)

SOVI_data.CD <- st_join(SOVI_data, select(CD_with_parks, boro_cd), largest = TRUE)  %>% 
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

SOVI_data.CD <- write_closest_flooding(SOVI_data.CD, moderate_flooding, "m_d_f")

SOVI_data.CD <- write_closest_flooding(SOVI_data.CD, extreme_flooding, "e_d_f")

SOVI_data.CD$boro_cd <- as.numeric(SOVI_data.CD$boro_cd)

SOVI_data.CD <- SOVI_data.CD %>% st_drop_geometry()

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
  dplyr::summarise(Total_BelPov_e = sum(BelPov_e, na.rm = TRUE),
                   Total_BelPov_s = (sum(BelPv_s^2, na.rm = TRUE))^0.5,
                   Total_Pop_e = sum(TotPop_e, na.rm = TRUE),
                   Total_Pop_s = (sum(TotPp_s^2, na.rm = TRUE))^0.5,
                   # Pov_rate_e = 100*sum(BelPov_, na.rm = TRUE) / sum(TotPop_, na.rm = TRUE),
                   
                   Total_HH_e = sum(TotHH_e, na.rm = TRUE),
                   Total_HH_s = (sum(TotHH_s^2, na.rm = TRUE))^0.5,
                   
                   HH_No_Car_e = sum(HhNoCr_e, na.rm = TRUE),
                   HH_No_Car_s = (sum(HhNCr_s^2, na.rm = TRUE))^0.5,
                   # PCT_NoCar_e = 100 * sum(HhNoCr_, na.rm = TRUE) / sum(TotHH_e, na.rm = TRUE),
                   
                   Total_Occ_units_e = sum(OccHh_e, na.rm = TRUE),
                   Total_Occ_units_s = (sum(OccHh_s^2, na.rm = TRUE))^0.5,
                   Rented_units_e = sum(Rented_e, na.rm = TRUE),
                   Rented_units_s = (sum(Rentd_s^2, na.rm = TRUE))^0.5,
                   # PCT_rented_e = 100*sum(Rented_, na.rm = TRUE)/sum(OccHh_e, na.rm = TRUE)
                   ) %>%
  mutate(Pov_rate_e = 100*(Total_BelPov_e / Total_Pop_e),
         Pov_rate_s = 100*ifelse((Total_BelPov_e / Total_Pop_e) != 1,
                             ifelse((Total_BelPov_s^2 - ((Total_BelPov_e / Total_Pop_e)^2 * Total_Pop_s^2)) > 0,
                             (1 / Total_Pop_e) * (Total_BelPov_s^2 - ((Total_BelPov_e / Total_Pop_e)^2 * Total_Pop_s^2))^0.5,
                             (1 / Total_Pop_e) * (Total_BelPov_s^2 + ((Total_BelPov_e / Total_Pop_e)^2 * Total_Pop_s^2))^0.5),
         Total_BelPov_s / Total_Pop_e),
         
         PCT_No_Car_e = 100*(HH_No_Car_e / Total_HH_e),
         PCT_No_Car_s = 100*ifelse((HH_No_Car_e / Total_HH_e) != 1,
                                 ifelse((HH_No_Car_s^2 - ((HH_No_Car_e / Total_HH_e)^2 * Total_HH_s^2)) > 0,
                                        (1 / Total_HH_e) * (HH_No_Car_s^2 - ((HH_No_Car_e / Total_HH_e)^2 * Total_HH_s^2))^0.5,
                                        (1 / Total_HH_e) * (HH_No_Car_s^2 + ((HH_No_Car_e / Total_HH_e)^2 * Total_HH_s^2))^0.5),
                                 HH_No_Car_s / Total_HH_e),
         
         PCT_rented_e = 100*(Rented_units_e / Total_Occ_units_e),
         PCT_rented_s = 100*ifelse((Rented_units_e / Total_Occ_units_e) != 1,
                                   ifelse((Rented_units_s^2 - ((Rented_units_e / Total_Occ_units_e)^2 * Total_Occ_units_s^2)) > 0,
                                          (1 / Total_Occ_units_e) * (Rented_units_s^2 - ((Rented_units_e / Total_Occ_units_e)^2 * Total_Occ_units_s^2))^0.5,
                                          (1 / Total_Occ_units_e) * (Rented_units_s^2 + ((Rented_units_e / Total_Occ_units_e)^2 * Total_Occ_units_s^2))^0.5),
                                   Rented_units_s / Total_Occ_units_e)
         
         
)

M.ACS_demographic_summary <- (M.ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(M.Total_BelPov_e = sum(BelPov_e, na.rm = TRUE),
                   M.Total_BelPov_s = (sum(BelPv_s^2, na.rm = TRUE))^0.5,
                   M.Total_Pop_e = sum(TotPop_e, na.rm = TRUE),
                   M.Total_Pop_s = (sum(TotPp_s^2, na.rm = TRUE))^0.5,
                   # Pov_rate_e = 100*sum(BelPov_, na.rm = TRUE) / sum(TotPop_, na.rm = TRUE),
                   
                   M.Total_HH_e = sum(TotHH_e, na.rm = TRUE),
                   M.Total_HH_s = (sum(TotHH_s^2, na.rm = TRUE))^0.5,
                   
                   M.HH_No_Car_e = sum(HhNoCr_e, na.rm = TRUE),
                   M.HH_No_Car_s = (sum(HhNCr_s^2, na.rm = TRUE))^0.5,
                   # PCT_NoCar_e = 100 * sum(HhNoCr_, na.rm = TRUE) / sum(TotHH_e, na.rm = TRUE),
                   
                   M.Total_Occ_units_e = sum(OccHh_e, na.rm = TRUE),
                   M.Total_Occ_units_s = (sum(OccHh_s^2, na.rm = TRUE))^0.5,
                   M.Rented_units_e = sum(Rented_e, na.rm = TRUE),
                   M.Rented_units_s = (sum(Rentd_s^2, na.rm = TRUE))^0.5,
                   # PCT_rented_e = 100*sum(Rented_, na.rm = TRUE)/sum(OccHh_e, na.rm = TRUE)
  ) %>% 
  mutate(M.Pov_rate_e = 100*(M.Total_BelPov_e / M.Total_Pop_e),
         M.Pov_rate_s = 100*ifelse((M.Total_BelPov_e / M.Total_Pop_e) != 1,
                                 ifelse((M.Total_BelPov_s^2 - ((M.Total_BelPov_e / M.Total_Pop_e)^2 * M.Total_Pop_s^2)) > 0,
                                        (1 / M.Total_Pop_e) * (M.Total_BelPov_s^2 - ((M.Total_BelPov_e / M.Total_Pop_e)^2 * M.Total_Pop_s^2))^0.5,
                                        (1 / M.Total_Pop_e) * (M.Total_BelPov_s^2 + ((M.Total_BelPov_e / M.Total_Pop_e)^2 * M.Total_Pop_s^2))^0.5),
                                 M.Total_BelPov_s / M.Total_Pop_e),
         
         M.PCT_No_Car_e = 100*(M.HH_No_Car_e / M.Total_HH_e),
         M.PCT_No_Car_s = 100*ifelse((M.HH_No_Car_e / M.Total_HH_e) != 1,
                                   ifelse((M.HH_No_Car_s^2 - ((M.HH_No_Car_e / M.Total_HH_e)^2 * M.Total_HH_s^2)) > 0,
                                          (1 / M.Total_HH_e) * (M.HH_No_Car_s^2 - ((M.HH_No_Car_e / M.Total_HH_e)^2 * M.Total_HH_s^2))^0.5,
                                          (1 / M.Total_HH_e) * (M.HH_No_Car_s^2 + ((M.HH_No_Car_e / M.Total_HH_e)^2 * M.Total_HH_s^2))^0.5),
                                   M.HH_No_Car_s / M.Total_HH_e),
         
         M.PCT_rented_e = 100*(M.Rented_units_e / M.Total_Occ_units_e),
         M.PCT_rented_s = 100*ifelse((M.Rented_units_e / M.Total_Occ_units_e) != 1,
                                   ifelse((M.Rented_units_s^2 - ((M.Rented_units_e / M.Total_Occ_units_e)^2 * M.Total_Occ_units_s^2)) > 0,
                                          (1 / M.Total_Occ_units_e) * (M.Rented_units_s^2 - ((M.Rented_units_e / M.Total_Occ_units_e)^2 * M.Total_Occ_units_s^2))^0.5,
                                          (1 / M.Total_Occ_units_e) * (M.Rented_units_s^2 + ((M.Rented_units_e / M.Total_Occ_units_e)^2 * M.Total_Occ_units_s^2))^0.5),
                                   M.Rented_units_s / M.Total_Occ_units_e)
         
         
  )

M.N.ACS_demographic_summary <- (M.N.ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(M.N.Total_BelPov_e = sum(BelPov_e, na.rm = TRUE),
                   M.N.Total_BelPov_s = (sum(BelPv_s^2, na.rm = TRUE))^0.5,
                   M.N.Total_Pop_e = sum(TotPop_e, na.rm = TRUE),
                   M.N.Total_Pop_s = (sum(TotPp_s^2, na.rm = TRUE))^0.5,
                   # Pov_rate_e = 100*sum(BelPov_, na.rm = TRUE) / sum(TotPop_, na.rm = TRUE),
                   
                   M.N.Total_HH_e = sum(TotHH_e, na.rm = TRUE),
                   M.N.Total_HH_s = (sum(TotHH_s^2, na.rm = TRUE))^0.5,
                   
                   M.N.HH_No_Car_e = sum(HhNoCr_e, na.rm = TRUE),
                   M.N.HH_No_Car_s = (sum(HhNCr_s^2, na.rm = TRUE))^0.5,
                   # PCT_NoCar_e = 100 * sum(HhNoCr_, na.rm = TRUE) / sum(TotHH_e, na.rm = TRUE),
                   
                   M.N.Total_Occ_units_e = sum(OccHh_e, na.rm = TRUE),
                   M.N.Total_Occ_units_s = (sum(OccHh_s^2, na.rm = TRUE))^0.5,
                   M.N.Rented_units_e = sum(Rented_e, na.rm = TRUE),
                   M.N.Rented_units_s = (sum(Rentd_s^2, na.rm = TRUE))^0.5,
                   # PCT_rented_e = 100*sum(Rented_, na.rm = TRUE)/sum(OccHh_e, na.rm = TRUE)
  ) %>% 
  mutate(M.N.Pov_rate_e = 100*(M.N.Total_BelPov_e / M.N.Total_Pop_e),
         M.N.Pov_rate_s = 100*ifelse((M.N.Total_BelPov_e / M.N.Total_Pop_e) != 1,
                                   ifelse((M.N.Total_BelPov_s^2 - ((M.N.Total_BelPov_e / M.N.Total_Pop_e)^2 * M.N.Total_Pop_s^2)) > 0,
                                          (1 / M.N.Total_Pop_e) * (M.N.Total_BelPov_s^2 - ((M.N.Total_BelPov_e / M.N.Total_Pop_e)^2 * M.N.Total_Pop_s^2))^0.5,
                                          (1 / M.N.Total_Pop_e) * (M.N.Total_BelPov_s^2 + ((M.N.Total_BelPov_e / M.N.Total_Pop_e)^2 * M.N.Total_Pop_s^2))^0.5),
                                   M.N.Total_BelPov_s / M.N.Total_Pop_e),
         
         M.N.PCT_No_Car_e = 100*(M.N.HH_No_Car_e / M.N.Total_HH_e),
         M.N.PCT_No_Car_s = 100*ifelse((M.N.HH_No_Car_e / M.N.Total_HH_e) != 1,
                                     ifelse((M.N.HH_No_Car_s^2 - ((M.N.HH_No_Car_e / M.N.Total_HH_e)^2 * M.N.Total_HH_s^2)) > 0,
                                            (1 / M.N.Total_HH_e) * (M.N.HH_No_Car_s^2 - ((M.N.HH_No_Car_e / M.N.Total_HH_e)^2 * M.N.Total_HH_s^2))^0.5,
                                            (1 / M.N.Total_HH_e) * (M.N.HH_No_Car_s^2 + ((M.N.HH_No_Car_e / M.N.Total_HH_e)^2 * M.N.Total_HH_s^2))^0.5),
                                     M.N.HH_No_Car_s / M.N.Total_HH_e),
         
         M.N.PCT_rented_e = 100*(M.N.Rented_units_e / M.N.Total_Occ_units_e),
         M.N.PCT_rented_s = 100*ifelse((M.N.Rented_units_e / M.N.Total_Occ_units_e) != 1,
                                     ifelse((M.N.Rented_units_s^2 - ((M.N.Rented_units_e / M.N.Total_Occ_units_e)^2 * M.N.Total_Occ_units_s^2)) > 0,
                                            (1 / M.N.Total_Occ_units_e) * (M.N.Rented_units_s^2 - ((M.N.Rented_units_e / M.N.Total_Occ_units_e)^2 * M.N.Total_Occ_units_s^2))^0.5,
                                            (1 / M.N.Total_Occ_units_e) * (M.N.Rented_units_s^2 + ((M.N.Rented_units_e / M.N.Total_Occ_units_e)^2 * M.N.Total_Occ_units_s^2))^0.5),
                                     M.N.Rented_units_s / M.N.Total_Occ_units_e)
         
         
  )

E.ACS_demographic_summary <- (E.ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(E.Total_BelPov_e = sum(BelPov_e, na.rm = TRUE),
                   E.Total_BelPov_s = (sum(BelPv_s^2, na.rm = TRUE))^0.5,
                   E.Total_Pop_e = sum(TotPop_e, na.rm = TRUE),
                   E.Total_Pop_s = (sum(TotPp_s^2, na.rm = TRUE))^0.5,
                   # Pov_rate_e = 100*sum(BelPov_, na.rm = TRUE) / sum(TotPop_, na.rm = TRUE),
                   
                   E.Total_HH_e = sum(TotHH_e, na.rm = TRUE),
                   E.Total_HH_s = (sum(TotHH_s^2, na.rm = TRUE))^0.5,
                   
                   E.HH_No_Car_e = sum(HhNoCr_e, na.rm = TRUE),
                   E.HH_No_Car_s = (sum(HhNCr_s^2, na.rm = TRUE))^0.5,
                   # PCT_NoCar_e = 100 * sum(HhNoCr_, na.rm = TRUE) / sum(TotHH_e, na.rm = TRUE),
                   
                   E.Total_Occ_units_e = sum(OccHh_e, na.rm = TRUE),
                   E.Total_Occ_units_s = (sum(OccHh_s^2, na.rm = TRUE))^0.5,
                   E.Rented_units_e = sum(Rented_e, na.rm = TRUE),
                   E.Rented_units_s = (sum(Rentd_s^2, na.rm = TRUE))^0.5,
                   # PCT_rented_e = 100*sum(Rented_, na.rm = TRUE)/sum(OccHh_e, na.rm = TRUE)
  ) %>% 
  mutate(E.Pov_rate_e = 100*(E.Total_BelPov_e / E.Total_Pop_e),
         E.Pov_rate_s = 100*ifelse((E.Total_BelPov_e / E.Total_Pop_e) != 1,
                                   ifelse((E.Total_BelPov_s^2 - ((E.Total_BelPov_e / E.Total_Pop_e)^2 * E.Total_Pop_s^2)) > 0,
                                          (1 / E.Total_Pop_e) * (E.Total_BelPov_s^2 - ((E.Total_BelPov_e / E.Total_Pop_e)^2 * E.Total_Pop_s^2))^0.5,
                                          (1 / E.Total_Pop_e) * (E.Total_BelPov_s^2 + ((E.Total_BelPov_e / E.Total_Pop_e)^2 * E.Total_Pop_s^2))^0.5),
                                   E.Total_BelPov_s / E.Total_Pop_e),
         
         E.PCT_No_Car_e = 100*(E.HH_No_Car_e / E.Total_HH_e),
         E.PCT_No_Car_s = 100*ifelse((E.HH_No_Car_e / E.Total_HH_e) != 1,
                                     ifelse((E.HH_No_Car_s^2 - ((E.HH_No_Car_e / E.Total_HH_e)^2 * E.Total_HH_s^2)) > 0,
                                            (1 / E.Total_HH_e) * (E.HH_No_Car_s^2 - ((E.HH_No_Car_e / E.Total_HH_e)^2 * E.Total_HH_s^2))^0.5,
                                            (1 / E.Total_HH_e) * (E.HH_No_Car_s^2 + ((E.HH_No_Car_e / E.Total_HH_e)^2 * E.Total_HH_s^2))^0.5),
                                     E.HH_No_Car_s / E.Total_HH_e),
         
         E.PCT_rented_e = 100*(E.Rented_units_e / E.Total_Occ_units_e),
         E.PCT_rented_s = 100*ifelse((E.Rented_units_e / E.Total_Occ_units_e) != 1,
                                     ifelse((E.Rented_units_s^2 - ((E.Rented_units_e / E.Total_Occ_units_e)^2 * E.Total_Occ_units_s^2)) > 0,
                                            (1 / E.Total_Occ_units_e) * (E.Rented_units_s^2 - ((E.Rented_units_e / E.Total_Occ_units_e)^2 * E.Total_Occ_units_s^2))^0.5,
                                            (1 / E.Total_Occ_units_e) * (E.Rented_units_s^2 + ((E.Rented_units_e / E.Total_Occ_units_e)^2 * E.Total_Occ_units_s^2))^0.5),
                                     E.Rented_units_s / E.Total_Occ_units_e)
         
         
  )

E.N.ACS_demographic_summary <- (E.N.ACS_demographic_summary) %>% 
  drop_na(boro_cd) %>%
  dplyr::group_by(boro_cd) %>% 
  dplyr::summarise(E.N.Total_BelPov_e = sum(BelPov_e, na.rm = TRUE),
                   E.N.Total_BelPov_s = (sum(BelPv_s^2, na.rm = TRUE))^0.5,
                   E.N.Total_Pop_e = sum(TotPop_e, na.rm = TRUE),
                   E.N.Total_Pop_s = (sum(TotPp_s^2, na.rm = TRUE))^0.5,
                   # Pov_rate_e = 100*sum(BelPov_, na.rm = TRUE) / sum(TotPop_, na.rm = TRUE),
                   
                   E.N.Total_HH_e = sum(TotHH_e, na.rm = TRUE),
                   E.N.Total_HH_s = (sum(TotHH_s^2, na.rm = TRUE))^0.5,
                   
                   E.N.HH_No_Car_e = sum(HhNoCr_e, na.rm = TRUE),
                   E.N.HH_No_Car_s = (sum(HhNCr_s^2, na.rm = TRUE))^0.5,
                   # PCT_NoCar_e = 100 * sum(HhNoCr_, na.rm = TRUE) / sum(TotHH_e, na.rm = TRUE),
                   
                   E.N.Total_Occ_units_e = sum(OccHh_e, na.rm = TRUE),
                   E.N.Total_Occ_units_s = (sum(OccHh_s^2, na.rm = TRUE))^0.5,
                   E.N.Rented_units_e = sum(Rented_e, na.rm = TRUE),
                   E.N.Rented_units_s = (sum(Rentd_s^2, na.rm = TRUE))^0.5,
                   # PCT_rented_e = 100*sum(Rented_, na.rm = TRUE)/sum(OccHh_e, na.rm = TRUE)
  ) %>% 
  mutate(E.N.Pov_rate_e = 100*(E.N.Total_BelPov_e / E.N.Total_Pop_e),
         E.N.Pov_rate_s = 100*ifelse((E.N.Total_BelPov_e / E.N.Total_Pop_e) != 1,
                                   ifelse((E.N.Total_BelPov_s^2 - ((E.N.Total_BelPov_e / E.N.Total_Pop_e)^2 * E.N.Total_Pop_s^2)) > 0,
                                          (1 / E.N.Total_Pop_e) * (E.N.Total_BelPov_s^2 - ((E.N.Total_BelPov_e / E.N.Total_Pop_e)^2 * E.N.Total_Pop_s^2))^0.5,
                                          (1 / E.N.Total_Pop_e) * (E.N.Total_BelPov_s^2 + ((E.N.Total_BelPov_e / E.N.Total_Pop_e)^2 * E.N.Total_Pop_s^2))^0.5),
                                   E.N.Total_BelPov_s / E.N.Total_Pop_e),
         
         E.N.PCT_No_Car_e = 100*(E.N.HH_No_Car_e / E.N.Total_HH_e),
         E.N.PCT_No_Car_s = 100*ifelse((E.N.HH_No_Car_e / E.N.Total_HH_e) != 1,
                                     ifelse((E.N.HH_No_Car_s^2 - ((E.N.HH_No_Car_e / E.N.Total_HH_e)^2 * E.N.Total_HH_s^2)) > 0,
                                            (1 / E.N.Total_HH_e) * (E.N.HH_No_Car_s^2 - ((E.N.HH_No_Car_e / E.N.Total_HH_e)^2 * E.N.Total_HH_s^2))^0.5,
                                            (1 / E.N.Total_HH_e) * (E.N.HH_No_Car_s^2 + ((E.N.HH_No_Car_e / E.N.Total_HH_e)^2 * E.N.Total_HH_s^2))^0.5),
                                     E.N.HH_No_Car_s / E.N.Total_HH_e),
         
         E.N.PCT_rented_e = 100*(E.N.Rented_units_e / E.N.Total_Occ_units_e),
         E.N.PCT_rented_s = 100*ifelse((E.N.Rented_units_e / E.N.Total_Occ_units_e) != 1,
                                     ifelse((E.N.Rented_units_s^2 - ((E.N.Rented_units_e / E.N.Total_Occ_units_e)^2 * E.N.Total_Occ_units_s^2)) > 0,
                                            (1 / E.N.Total_Occ_units_e) * (E.N.Rented_units_s^2 - ((E.N.Rented_units_e / E.N.Total_Occ_units_e)^2 * E.N.Total_Occ_units_s^2))^0.5,
                                            (1 / E.N.Total_Occ_units_e) * (E.N.Rented_units_s^2 + ((E.N.Rented_units_e / E.N.Total_Occ_units_e)^2 * E.N.Total_Occ_units_s^2))^0.5),
                                     E.N.Rented_units_s / E.N.Total_Occ_units_e)
         
         
  )

final_table <- final_table %>%
  left_join(ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>% 
  left_join(M.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(M.N.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(E.ACS_demographic_summary, by = c("Geography" = "boro_cd")) %>%
  left_join(E.N.ACS_demographic_summary, by = c("Geography" = "boro_cd"))

# For the NYC row, lets calculate ACS variables first, then add them (so I dont go crazy)

## CityWide

NYC.BelPov_e <- sum(final_table[,"Total_BelPov_e"])
NYC.BelPov_s <- (sum(final_table[,"Total_BelPov_s"]^2))^0.5
NYC.TotPop_e <- sum(final_table[,"Total_Pop_e"])
NYC.TotPop_s <- (sum(final_table[,"Total_Pop_s"]^2))^0.5
NYC.TotHH_e <- sum(final_table[,"Total_HH_e"])
NYC.TotHH_s <- (sum(final_table[,"Total_HH_s"]^2))^0.5
NYC.HH_No_Car_e <- sum(final_table[,"HH_No_Car_e"])
NYC.HH_No_Car_s <- (sum(final_table[,"HH_No_Car_s"]^2))^0.5
NYC.Total_Occ_units_e <- sum(final_table[,"Total_Occ_units_e"])
NYC.Total_Occ_units_s <- (sum(final_table[,"Total_Occ_units_s"]^2))^0.5
NYC.Rented_units_e <- sum(final_table[,"Rented_units_e"])
NYC.Rented_units_s <- (sum(final_table[,"Rented_units_s"]^2))^0.5

NYC.Pov_rate_e <- 100*NYC.BelPov_e / NYC.TotPop_e
NYC.Pov_rate_s <- 100*ifelse(NYC.Pov_rate_e != 100,
                             ifelse((NYC.BelPov_s^2 - (NYC.Pov_rate_e^2 * NYC.TotPop_s^2)) > 0,
                                    (1 / NYC.TotPop_e) * ((NYC.BelPov_s^2 - (NYC.Pov_rate_e^2 * NYC.TotPop_s^2))^0.5),
                                    (1 / NYC.TotPop_e) * ((NYC.BelPov_s^2 + (NYC.Pov_rate_e^2 * NYC.TotPop_s^2))^0.5)),
                             NYC.BelPov_s / NYC.TotPop_e)

NYC.PCT_No_Car_e <- 100*NYC.HH_No_Car_e / NYC.TotHH_e
NYC.PCT_No_Car_s <- 100*ifelse(NYC.PCT_No_Car_e != 100,
                               ifelse((NYC.HH_No_Car_s^2 - (NYC.PCT_No_Car_e^2 * NYC.TotHH_s^2)) > 0,
                                      (1 / NYC.TotHH_e) * ((NYC.HH_No_Car_s^2 - (NYC.PCT_No_Car_e^2 * NYC.TotHH_s^2))^0.5),
                                      (1 / NYC.TotHH_e) * ((NYC.HH_No_Car_s^2 + (NYC.PCT_No_Car_e^2 * NYC.TotHH_s^2))^0.5)),
                               NYC.HH_No_Car_s / NYC.TotHH_e)

NYC.PCT_rented_e <- 100*NYC.Rented_units_e / NYC.Total_Occ_units_e
NYC.PCT_rented_s <- 100*ifelse(NYC.PCT_rented_e != 100,
                               ifelse((NYC.Rented_units_s^2 - (NYC.PCT_rented_e^2 * NYC.Total_Occ_units_s^2)) > 0,
                                      (1 / NYC.Total_Occ_units_e) * ((NYC.Rented_units_s^2 - (NYC.PCT_rented_e^2 * NYC.Total_Occ_units_s^2))^0.5),
                                      (1 / NYC.Total_Occ_units_e) * ((NYC.Rented_units_s^2 + (NYC.PCT_rented_e^2 * NYC.Total_Occ_units_s^2))^0.5)),
                               NYC.Rented_units_s / NYC.Total_Occ_units_e)


## Moderate CityWide

NYC.M.BelPov_e <- sum(final_table[,"M.Total_BelPov_e"])
NYC.M.BelPov_s <- (sum(final_table[,"M.Total_BelPov_s"]^2))^0.5
NYC.M.TotPop_e <- sum(final_table[,"M.Total_Pop_e"])
NYC.M.TotPop_s <- (sum(final_table[,"M.Total_Pop_s"]^2))^0.5
NYC.M.TotHH_e <- sum(final_table[,"M.Total_HH_e"])
NYC.M.TotHH_s <- (sum(final_table[,"M.Total_HH_s"]^2))^0.5
NYC.M.HH_No_Car_e <- sum(final_table[,"M.HH_No_Car_e"])
NYC.M.HH_No_Car_s <- (sum(final_table[,"M.HH_No_Car_s"]^2))^0.5
NYC.M.Total_Occ_units_e <- sum(final_table[,"M.Total_Occ_units_e"])
NYC.M.Total_Occ_units_s <- (sum(final_table[,"M.Total_Occ_units_s"]^2))^0.5
NYC.M.Rented_units_e <- sum(final_table[,"M.Rented_units_e"])
NYC.M.Rented_units_s <- (sum(final_table[,"M.Rented_units_s"]^2))^0.5

NYC.M.Pov_rate_e <- 100*NYC.M.BelPov_e / NYC.M.TotPop_e
NYC.M.Pov_rate_s <- 100*ifelse(NYC.M.Pov_rate_e != 100,
                             ifelse((NYC.M.BelPov_s^2 - (NYC.M.Pov_rate_e^2 * NYC.M.TotPop_s^2)) > 0,
                                    (1 / NYC.M.TotPop_e) * (NYC.M.BelPov_s^2 - (NYC.M.Pov_rate_e^2 * NYC.M.TotPop_s^2))^0.5,
                                    (1 / NYC.M.TotPop_e) * (NYC.M.BelPov_s^2 + (NYC.M.Pov_rate_e^2 * NYC.M.TotPop_s^2))^0.5),
                             NYC.M.BelPov_s / NYC.M.TotPop_e)

NYC.M.PCT_No_Car_e <- 100*NYC.M.HH_No_Car_e / NYC.M.TotHH_e
NYC.M.PCT_No_Car_s <- 100*ifelse(NYC.M.PCT_No_Car_e != 100,
                               ifelse((NYC.M.HH_No_Car_s^2 - (NYC.M.PCT_No_Car_e^2 * NYC.M.TotHH_s^2)) > 0,
                                      (1 / NYC.M.TotHH_e) * (NYC.M.HH_No_Car_s^2 - (NYC.M.PCT_No_Car_e^2 * NYC.M.TotHH_s^2))^0.5,
                                      (1 / NYC.M.TotHH_e) * (NYC.M.HH_No_Car_s^2 + (NYC.M.PCT_No_Car_e^2 * NYC.M.TotHH_s^2))^0.5),
                               NYC.M.HH_No_Car_s / NYC.M.TotHH_e)

NYC.M.PCT_rented_e <- 100*NYC.M.Rented_units_e / NYC.M.Total_Occ_units_e
NYC.M.PCT_rented_s <- 100*ifelse(NYC.M.PCT_rented_e != 100,
                               ifelse((NYC.M.Rented_units_s^2 - (NYC.M.PCT_rented_e^2 * NYC.M.Total_Occ_units_s^2)) > 0,
                                      (1 / NYC.M.Total_Occ_units_e) * (NYC.M.Rented_units_s^2 - (NYC.M.PCT_rented_e^2 * NYC.M.Total_Occ_units_s^2))^0.5,
                                      (1 / NYC.M.Total_Occ_units_e) * (NYC.M.Rented_units_s^2 + (NYC.M.PCT_rented_e^2 * NYC.M.Total_Occ_units_s^2))^0.5),
                               NYC.M.Rented_units_s / NYC.M.Total_Occ_units_e)

## Moderate CityWide Unexposed

NYC.M.N.BelPov_e <- sum(final_table[,"M.N.Total_BelPov_e"], na.rm=TRUE)
NYC.M.N.BelPov_s <- (sum(final_table[,"M.N.Total_BelPov_s"]^2, na.rm=TRUE))^0.5
NYC.M.N.TotPop_e <- sum(final_table[,"M.N.Total_Pop_e"], na.rm=TRUE)
NYC.M.N.TotPop_s <- (sum(final_table[,"M.N.Total_Pop_s"]^2, na.rm=TRUE))^0.5
NYC.M.N.TotHH_e <- sum(final_table[,"M.N.Total_HH_e"], na.rm=TRUE)
NYC.M.N.TotHH_s <- (sum(final_table[,"M.N.Total_HH_s"]^2, na.rm=TRUE))^0.5
NYC.M.N.HH_No_Car_e <- sum(final_table[,"M.N.HH_No_Car_e"], na.rm=TRUE)
NYC.M.N.HH_No_Car_s <- (sum(final_table[,"M.N.HH_No_Car_s"]^2, na.rm=TRUE))^0.5
NYC.M.N.Total_Occ_units_e <- sum(final_table[,"M.N.Total_Occ_units_e"], na.rm=TRUE)
NYC.M.N.Total_Occ_units_s <- (sum(final_table[,"M.N.Total_Occ_units_s"]^2, na.rm=TRUE))^0.5
NYC.M.N.Rented_units_e <- sum(final_table[,"M.N.Rented_units_e"], na.rm=TRUE)
NYC.M.N.Rented_units_s <- (sum(final_table[,"M.N.Rented_units_s"]^2, na.rm=TRUE))^0.5

NYC.M.N.Pov_rate_e <- 100*NYC.M.N.BelPov_e / NYC.M.N.TotPop_e
NYC.M.N.Pov_rate_s <- 100*ifelse(NYC.M.N.Pov_rate_e != 100,
                               ifelse((NYC.M.N.BelPov_s^2 - (NYC.M.N.Pov_rate_e^2 * NYC.M.N.TotPop_s^2)) > 0,
                                      (1 / NYC.M.N.TotPop_e) * (NYC.M.N.BelPov_s^2 - (NYC.M.N.Pov_rate_e^2 * NYC.M.N.TotPop_s^2))^0.5,
                                      (1 / NYC.M.N.TotPop_e) * (NYC.M.N.BelPov_s^2 + (NYC.M.N.Pov_rate_e^2 * NYC.M.N.TotPop_s^2))^0.5),
                               NYC.M.N.BelPov_s / NYC.M.N.TotPop_e)

NYC.M.N.PCT_No_Car_e <- 100*NYC.M.N.HH_No_Car_e / NYC.M.N.TotHH_e
NYC.M.N.PCT_No_Car_s <- 100*ifelse(NYC.M.N.PCT_No_Car_e != 100,
                                 ifelse((NYC.M.N.HH_No_Car_s^2 - (NYC.M.N.PCT_No_Car_e^2 * NYC.M.N.TotHH_s^2)) > 0,
                                        (1 / NYC.M.N.TotHH_e) * (NYC.M.N.HH_No_Car_s^2 - (NYC.M.N.PCT_No_Car_e^2 * NYC.M.N.TotHH_s^2))^0.5,
                                        (1 / NYC.M.N.TotHH_e) * (NYC.M.N.HH_No_Car_s^2 + (NYC.M.N.PCT_No_Car_e^2 * NYC.M.N.TotHH_s^2))^0.5),
                                 NYC.M.N.HH_No_Car_s / NYC.M.N.TotHH_e)

NYC.M.N.PCT_rented_e <- 100*NYC.M.N.Rented_units_e / NYC.M.N.Total_Occ_units_e
NYC.M.N.PCT_rented_s <- 100*ifelse(NYC.M.N.PCT_rented_e != 100,
                                 ifelse((NYC.M.N.Rented_units_s^2 - (NYC.M.N.PCT_rented_e^2 * NYC.M.N.Total_Occ_units_s^2)) > 0,
                                        (1 / NYC.M.N.Total_Occ_units_e) * (NYC.M.N.Rented_units_s^2 - (NYC.M.N.PCT_rented_e^2 * NYC.M.N.Total_Occ_units_s^2))^0.5,
                                        (1 / NYC.M.N.Total_Occ_units_e) * (NYC.M.N.Rented_units_s^2 + (NYC.M.N.PCT_rented_e^2 * NYC.M.N.Total_Occ_units_s^2))^0.5),
                                 NYC.M.N.Rented_units_s / NYC.M.N.Total_Occ_units_e)

## Extreme CityWide

NYC.E.BelPov_e <- sum(final_table[,"E.Total_BelPov_e"])
NYC.E.BelPov_s <- (sum(final_table[,"E.Total_BelPov_s"]^2))^0.5
NYC.E.TotPop_e <- sum(final_table[,"E.Total_Pop_e"])
NYC.E.TotPop_s <- (sum(final_table[,"E.Total_Pop_s"]^2))^0.5
NYC.E.TotHH_e <- sum(final_table[,"E.Total_HH_e"])
NYC.E.TotHH_s <- (sum(final_table[,"E.Total_HH_s"]^2))^0.5
NYC.E.HH_No_Car_e <- sum(final_table[,"E.HH_No_Car_e"])
NYC.E.HH_No_Car_s <- (sum(final_table[,"E.HH_No_Car_s"]^2))^0.5
NYC.E.Total_Occ_units_e <- sum(final_table[,"E.Total_Occ_units_e"])
NYC.E.Total_Occ_units_s <- (sum(final_table[,"E.Total_Occ_units_s"]^2))^0.5
NYC.E.Rented_units_e <- sum(final_table[,"E.Rented_units_e"])
NYC.E.Rented_units_s <- (sum(final_table[,"E.Rented_units_s"]^2))^0.5

NYC.E.Pov_rate_e <- 100*NYC.E.BelPov_e / NYC.E.TotPop_e
NYC.E.Pov_rate_s <- 100*ifelse(NYC.E.Pov_rate_e != 100,
                               ifelse((NYC.E.BelPov_s^2 - (NYC.E.Pov_rate_e^2 * NYC.E.TotPop_s^2)) > 0,
                                      (1 / NYC.E.TotPop_e) * (NYC.E.BelPov_s^2 - (NYC.E.Pov_rate_e^2 * NYC.E.TotPop_s^2))^0.5,
                                      (1 / NYC.E.TotPop_e) * (NYC.E.BelPov_s^2 + (NYC.E.Pov_rate_e^2 * NYC.E.TotPop_s^2))^0.5),
                               NYC.E.BelPov_s / NYC.E.TotPop_e)

NYC.E.PCT_No_Car_e <- 100*NYC.E.HH_No_Car_e / NYC.E.TotHH_e
NYC.E.PCT_No_Car_s <- 100*ifelse(NYC.E.PCT_No_Car_e != 100,
                                 ifelse((NYC.E.HH_No_Car_s^2 - (NYC.E.PCT_No_Car_e^2 * NYC.E.TotHH_s^2)) > 0,
                                        (1 / NYC.E.TotHH_e) * (NYC.E.HH_No_Car_s^2 - (NYC.E.PCT_No_Car_e^2 * NYC.E.TotHH_s^2))^0.5,
                                        (1 / NYC.E.TotHH_e) * (NYC.E.HH_No_Car_s^2 + (NYC.E.PCT_No_Car_e^2 * NYC.E.TotHH_s^2))^0.5),
                                 NYC.E.HH_No_Car_s / NYC.E.TotHH_e)

NYC.E.PCT_rented_e <- 100*NYC.E.Rented_units_e / NYC.E.Total_Occ_units_e
NYC.E.PCT_rented_s <- 100*ifelse(NYC.E.PCT_rented_e != 100,
                                 ifelse((NYC.E.Rented_units_s^2 - (NYC.E.PCT_rented_e^2 * NYC.E.Total_Occ_units_s^2)) > 0,
                                        (1 / NYC.E.Total_Occ_units_e) * (NYC.E.Rented_units_s^2 - (NYC.E.PCT_rented_e^2 * NYC.E.Total_Occ_units_s^2))^0.5,
                                        (1 / NYC.E.Total_Occ_units_e) * (NYC.E.Rented_units_s^2 + (NYC.E.PCT_rented_e^2 * NYC.E.Total_Occ_units_s^2))^0.5),
                                 NYC.E.Rented_units_s / NYC.E.Total_Occ_units_e)

## Extreme Citiwide Unexposed

NYC.E.N.BelPov_e <- sum(final_table[,"E.N.Total_BelPov_e"], na.rm=TRUE)
NYC.E.N.BelPov_s <- (sum(final_table[,"E.N.Total_BelPov_s"]^2, na.rm=TRUE))^0.5
NYC.E.N.TotPop_e <- sum(final_table[,"E.N.Total_Pop_e"], na.rm=TRUE)
NYC.E.N.TotPop_s <- (sum(final_table[,"E.N.Total_Pop_s"]^2, na.rm=TRUE))^0.5
NYC.E.N.TotHH_e <- sum(final_table[,"E.N.Total_HH_e"], na.rm=TRUE)
NYC.E.N.TotHH_s <- (sum(final_table[,"E.N.Total_HH_s"]^2, na.rm=TRUE))^0.5
NYC.E.N.HH_No_Car_e <- sum(final_table[,"E.N.HH_No_Car_e"], na.rm=TRUE)
NYC.E.N.HH_No_Car_s <- (sum(final_table[,"E.N.HH_No_Car_s"]^2, na.rm=TRUE))^0.5
NYC.E.N.Total_Occ_units_e <- sum(final_table[,"E.N.Total_Occ_units_e"], na.rm=TRUE)
NYC.E.N.Total_Occ_units_s <- (sum(final_table[,"E.N.Total_Occ_units_s"]^2, na.rm=TRUE))^0.5
NYC.E.N.Rented_units_e <- sum(final_table[,"E.N.Rented_units_e"], na.rm=TRUE)
NYC.E.N.Rented_units_s <- (sum(final_table[,"E.N.Rented_units_s"]^2, na.rm=TRUE))^0.5

NYC.E.N.Pov_rate_e <- 100*NYC.E.N.BelPov_e / NYC.E.N.TotPop_e
NYC.E.N.Pov_rate_s <- 100*ifelse(NYC.E.N.Pov_rate_e != 100,
                               ifelse((NYC.E.N.BelPov_s^2 - (NYC.E.N.Pov_rate_e^2 * NYC.E.N.TotPop_s^2)) > 0,
                                      (1 / NYC.E.N.TotPop_e) * (NYC.E.N.BelPov_s^2 - (NYC.E.N.Pov_rate_e^2 * NYC.E.N.TotPop_s^2))^0.5,
                                      (1 / NYC.E.N.TotPop_e) * (NYC.E.N.BelPov_s^2 + (NYC.E.N.Pov_rate_e^2 * NYC.E.N.TotPop_s^2))^0.5),
                               NYC.E.N.BelPov_s / NYC.E.N.TotPop_e)

NYC.E.N.PCT_No_Car_e <- 100*NYC.E.N.HH_No_Car_e / NYC.E.N.TotHH_e
NYC.E.N.PCT_No_Car_s <- 100*ifelse(NYC.E.N.PCT_No_Car_e != 100,
                                 ifelse((NYC.E.N.HH_No_Car_s^2 - (NYC.E.N.PCT_No_Car_e^2 * NYC.E.N.TotHH_s^2)) > 0,
                                        (1 / NYC.E.N.TotHH_e) * (NYC.E.N.HH_No_Car_s^2 - (NYC.E.N.PCT_No_Car_e^2 * NYC.E.N.TotHH_s^2))^0.5,
                                        (1 / NYC.E.N.TotHH_e) * (NYC.E.N.HH_No_Car_s^2 + (NYC.E.N.PCT_No_Car_e^2 * NYC.E.N.TotHH_s^2))^0.5),
                                 NYC.E.N.HH_No_Car_s / NYC.E.N.TotHH_e)

NYC.E.N.PCT_rented_e <- 100*NYC.E.N.Rented_units_e / NYC.E.N.Total_Occ_units_e
NYC.E.N.PCT_rented_s <- 100*ifelse(NYC.E.N.PCT_rented_e != 100,
                                 ifelse((NYC.E.N.Rented_units_s^2 - (NYC.E.N.PCT_rented_e^2 * NYC.E.N.Total_Occ_units_s^2)) > 0,
                                        (1 / NYC.E.N.Total_Occ_units_e) * (NYC.E.N.Rented_units_s^2 - (NYC.E.N.PCT_rented_e^2 * NYC.E.N.Total_Occ_units_s^2))^0.5,
                                        (1 / NYC.E.N.Total_Occ_units_e) * (NYC.E.N.Rented_units_s^2 + (NYC.E.N.PCT_rented_e^2 * NYC.E.N.Total_Occ_units_s^2))^0.5),
                                 NYC.E.N.Rented_units_s / NYC.E.N.Total_Occ_units_e)

# Now write final CityWide row

NYC_row_data <- list("NYC", 
                  sum(final_table[,"Total area"]),
                  sum(final_table[,"Moderate 4in area"]),
                  sum(final_table[,"Moderate 1ft area"]),
                  sum(final_table[,"Moderate total flood area"]),
                  100 * sum(final_table[,"Moderate 4in area"]) / sum(final_table[,"Total area"]),
                  100 * sum(final_table[,"Moderate 1ft area"]) / sum(final_table[,"Total area"]),
                  100 * sum(final_table[,"Moderate total flood area"]) / sum(final_table[,"Total area"]),
                  sum(final_table[,"Extreme 4in area"]),
                  sum(final_table[,"Extreme 1ft area"]),
                  sum(final_table[,"Extreme total flood area"]),
                  100 * sum(final_table[,"Extreme 4in area"]) / sum(final_table[,"Total area"]),
                  100 * sum(final_table[,"Extreme 1ft area"]) / sum(final_table[,"Total area"]),
                  100 * sum(final_table[,"Extreme total flood area"]) / sum(final_table[,"Total area"]),
                  sum(final_table[,"Total road area"]),
                  sum(final_table[,"M.Flooded road area"]),
                  sum(final_table[,"E.Flooded road area"]),
                  100 * sum(final_table[,"M.Flooded road area"] / sum(final_table[,"Total road area"])),
                  100 * sum(final_table[,"E.Flooded road area"] / sum(final_table[,"Total road area"])),
                  sum(final_table[,"Total population"]), 
                  sum(final_table[,"Total Black"]), 
                  sum(final_table[,"Total Hispanic"]),
                  sum(final_table[,"Total White"]),
                  100 * sum(final_table[,"Total Black"]) / sum(final_table[,"Total population"]),
                  100 * sum(final_table[,"Total Hispanic"]) / sum(final_table[,"Total population"]),
                  100 * sum(final_table[,"Total White"]) / sum(final_table[,"Total population"]),
                  sum(final_table[,"Total 0-5 children"]),
                  sum(final_table[,"Total +65 elder"]),
                  100 * sum(final_table[,"Total 0-5 children"]) / sum(final_table[,"Total population"]),
                  100 * sum(final_table[,"Total +65 elder"]) / sum(final_table[,"Total population"]),
                  sum(final_table[,"M.Flooded total population"]),
                  sum(final_table[,"M.Flooded total Black"]),
                  sum(final_table[,"M.Flooded total Hispanic"]),
                  sum(final_table[,"M.Flooded total White"]),
                  100 * sum(final_table[,"M.Flooded total Black"]) / sum(final_table[,"M.Flooded total population"]),
                  100 * sum(final_table[,"M.Flooded total Hispanic"]) / sum(final_table[,"M.Flooded total population"]),
                  100 * sum(final_table[,"M.Flooded total White"]) / sum(final_table[,"M.Flooded total population"]),
                  sum(final_table[,"M.Flooded total 0-5 children"]),
                  sum(final_table[,"M.Flooded total +65 elder"]),
                  100 * sum(final_table[,"M.Flooded total 0-5 children"]) / sum(final_table[,"M.Flooded total population"]),
                  100 * sum(final_table[,"M.Flooded total +65 elder"]) / sum(final_table[,"M.Flooded total population"]),
                  
                  sum(final_table[,"M.N.Flooded total population"]),
                  sum(final_table[,"M.N.Flooded total Black"]),
                  sum(final_table[,"M.N.Flooded total Hispanic"]),
                  sum(final_table[,"M.N.Flooded total White"]),
                  100 * sum(final_table[,"M.N.Flooded total Black"]) / sum(final_table[,"M.N.Flooded total population"]),
                  100 * sum(final_table[,"M.N.Flooded total Hispanic"]) / sum(final_table[,"M.N.Flooded total population"]),
                  100 * sum(final_table[,"M.N.Flooded total White"]) / sum(final_table[,"M.N.Flooded total population"]),
                  sum(final_table[,"M.N.Flooded total 0-5 children"]),
                  sum(final_table[,"M.N.Flooded total +65 elder"]),
                  100 * sum(final_table[,"M.N.Flooded total 0-5 children"]) / sum(final_table[,"M.N.Flooded total population"]),
                  100 * sum(final_table[,"M.N.Flooded total +65 elder"]) / sum(final_table[,"M.N.Flooded total population"]),
                  
                  sum(final_table[,"E.Flooded total population"]),
                  sum(final_table[,"E.Flooded total Black"]),
                  sum(final_table[,"E.Flooded total Hispanic"]),
                  sum(final_table[,"E.Flooded total White"]),
                  100 * sum(final_table[,"E.Flooded total Black"]) / sum(final_table[,"E.Flooded total population"]),
                  100 * sum(final_table[,"E.Flooded total Hispanic"]) / sum(final_table[,"E.Flooded total population"]),
                  100 * sum(final_table[,"E.Flooded total White"]) / sum(final_table[,"E.Flooded total population"]),
                  sum(final_table[,"E.Flooded total 0-5 children"]),
                  sum(final_table[,"E.Flooded total +65 elder"]),
                  100 * sum(final_table[,"E.Flooded total 0-5 children"]) / sum(final_table[,"E.Flooded total population"]),
                  100 * sum(final_table[,"E.Flooded total +65 elder"]) / sum(final_table[,"E.Flooded total population"]),
                  
                  sum(final_table[,"E.N.Flooded total population"]),
                  sum(final_table[,"E.N.Flooded total Black"]),
                  sum(final_table[,"E.N.Flooded total Hispanic"]),
                  sum(final_table[,"E.N.Flooded total White"]),
                  100 * sum(final_table[,"E.N.Flooded total Black"]) / sum(final_table[,"E.N.Flooded total population"]),
                  100 * sum(final_table[,"E.N.Flooded total Hispanic"]) / sum(final_table[,"E.N.Flooded total population"]),
                  100 * sum(final_table[,"E.N.Flooded total White"]) / sum(final_table[,"E.N.Flooded total population"]),
                  sum(final_table[,"E.N.Flooded total 0-5 children"]),
                  sum(final_table[,"E.N.Flooded total +65 elder"]),
                  100 * sum(final_table[,"E.N.Flooded total 0-5 children"]) / sum(final_table[,"E.N.Flooded total population"]),
                  100 * sum(final_table[,"E.N.Flooded total +65 elder"]) / sum(final_table[,"E.N.Flooded total population"]),
                  
                  sum(final_table[,"Total residential tax lots"]),
                  sum(final_table[,"M.Flooded total residential"]),
                  sum(final_table[,"E.Flooded total residential"]),
                  100 * sum(final_table[,"M.Flooded total residential"]) / sum(final_table[,"Total residential tax lots"]),
                  100 * sum(final_table[,"E.Flooded total residential"]) / sum(final_table[,"Total residential tax lots"]),
                  
                  sum(final_table[,"Total below grade basement residential"]),
                  sum(final_table[,"M.Flooded below grade basement residential"]),
                  sum(final_table[,"E.Flooded below grade basement residential"]),
                  100 * sum(final_table[,"M.Flooded below grade basement residential"]) / sum(final_table[,"Total below grade basement residential"]),
                  100 * sum(final_table[,"E.Flooded below grade basement residential"]) / sum(final_table[,"Total below grade basement residential"]),
                  
                  sum(final_table[,"Total mixed commercial & residential tax lots"]),
                  sum(final_table[,"M.Flooded total mixed commercial & residential"]),
                  sum(final_table[,"E.Flooded total mixed commercial & residential"]),
                  100 * sum(final_table[,"M.Flooded total mixed commercial & residential"]) / sum(final_table[,"Total mixed commercial & residential tax lots"]),
                  100 * sum(final_table[,"E.Flooded total mixed commercial & residential"]) / sum(final_table[,"Total mixed commercial & residential tax lots"]),
                  
                  sum(final_table[,"Total commercial tax lots"]),
                  sum(final_table[,"M.Flooded total commercial"]),
                  sum(final_table[,"E.Flooded total commercial"]),
                  100 * sum(final_table[,"M.Flooded total commercial"]) / sum(final_table[,"Total commercial tax lots"]),
                  100 * sum(final_table[,"E.Flooded total commercial"]) / sum(final_table[,"Total commercial tax lots"]),
                  
                  sum(final_table[,"Total industrial tax lots"]),
                  sum(final_table[,"M.Flooded total industrial"]),
                  sum(final_table[,"E.Flooded total industrial"]),
                  100 * sum(final_table[,"M.Flooded total industrial"]) / sum(final_table[,"Total industrial tax lots"]),
                  100 * sum(final_table[,"E.Flooded total industrial"]) / sum(final_table[,"Total industrial tax lots"]),
                  
                  sum(final_table[,"Total number of Human Welfare Services facilities"]),
                  sum(final_table[,"M.Flooded number of Human Welfare Services facilities"]),
                  sum(final_table[,"E.Flooded number of Human Welfare Services facilities"]),
                  100* sum(final_table[,"M.Flooded number of Human Welfare Services facilities"]) / sum(final_table[,"Total number of Human Welfare Services facilities"]),
                  100* sum(final_table[,"E.Flooded number of Human Welfare Services facilities"]) / sum(final_table[,"Total number of Human Welfare Services facilities"]),
                  
                  sum(final_table[,"Total number of Education facilities"]),
                  sum(final_table[,"M.Flooded number of Education facilities"]),
                  sum(final_table[,"E.Flooded number of Education facilities"]),
                  100* sum(final_table[,"M.Flooded number of Education facilities"]) / sum(final_table[,"Total number of Education facilities"]),
                  100* sum(final_table[,"E.Flooded number of Education facilities"]) / sum(final_table[,"Total number of Education facilities"]),
                  
                  sum(final_table[,"Total number of Healthcare facilities"]),
                  sum(final_table[,"M.Flooded number of Healthcare facilities"]),
                  sum(final_table[,"E.Flooded number of Healthcare facilities"]),
                  100* sum(final_table[,"M.Flooded number of Healthcare facilities"]) / sum(final_table[,"Total number of Healthcare facilities"]),
                  100* sum(final_table[,"E.Flooded number of Healthcare facilities"]) / sum(final_table[,"Total number of Healthcare facilities"]),
                  
                  sum(final_table[,"Total number of Public Safety facilities"]),
                  sum(final_table[,"M.Flooded number of Public Safety facilities"]),
                  sum(final_table[,"E.Flooded number of Public Safety facilities"]),
                  100* sum(final_table[,"M.Flooded number of Public Safety facilities"]) / sum(final_table[,"Total number of Public Safety facilities"]),
                  100* sum(final_table[,"E.Flooded number of Public Safety facilities"]) / sum(final_table[,"Total number of Public Safety facilities"]),
                  
                  sum(final_table[,"Total number of bus stops"]),
                  sum(final_table[,"M.Flooded number of bus stops"]),
                  sum(final_table[,"E.Flooded number of bus stops"]),
                  100* sum(final_table[,"M.Flooded number of bus stops"]) / sum(final_table[,"Total number of bus stops"]),
                  100* sum(final_table[,"E.Flooded number of bus stops"]) / sum(final_table[,"Total number of bus stops"]),
                  
                  sum(final_table[,"Total number of subway entrances"]),
                  sum(final_table[,"M.Flooded number of subway entrances"]),
                  sum(final_table[,"E.Flooded number of subway entrances"]),
                  100* sum(final_table[,"M.Flooded number of subway entrances"]) / sum(final_table[,"Total number of subway entrances"]),
                  100* sum(final_table[,"E.Flooded number of subway entrances"]) / sum(final_table[,"Total number of subway entrances"]),
                  
                  sum(final_table[,"Total number of bus routes"]),
                  sum(final_table[,"M.Flooded number of bus routes"]),
                  sum(final_table[,"E.Flooded number of bus routes"]),
                  100* sum(final_table[,"M.Flooded number of bus routes"]) / sum(final_table[,"Total number of bus routes"]),
                  100* sum(final_table[,"E.Flooded number of bus routes"]) / sum(final_table[,"Total number of bus routes"]),
                  
                  NYC.BelPov_e,
                  NYC.BelPov_s,
                  NYC.TotPop_e,
                  NYC.TotPop_s,
                  NYC.TotHH_e,
                  NYC.TotHH_s,
                  NYC.HH_No_Car_e,
                  NYC.HH_No_Car_s,
                  NYC.Total_Occ_units_e,
                  NYC.Total_Occ_units_s,
                  NYC.Rented_units_e,
                  NYC.Rented_units_s,
                  NYC.Pov_rate_e,
                  NYC.Pov_rate_s,
                  NYC.PCT_No_Car_e,
                  NYC.PCT_No_Car_s,
                  NYC.PCT_rented_e,
                  NYC.PCT_rented_s,
                  
                  NYC.M.BelPov_e,
                  NYC.M.BelPov_s,
                  NYC.M.TotPop_e,
                  NYC.M.TotPop_s,
                  NYC.M.TotHH_e,
                  NYC.M.TotHH_s,
                  NYC.M.HH_No_Car_e,
                  NYC.M.HH_No_Car_s,
                  NYC.M.Total_Occ_units_e,
                  NYC.M.Total_Occ_units_s,
                  NYC.M.Rented_units_e,
                  NYC.M.Rented_units_s,
                  NYC.M.Pov_rate_e,
                  NYC.M.Pov_rate_s,
                  NYC.M.PCT_No_Car_e,
                  NYC.M.PCT_No_Car_s,
                  NYC.M.PCT_rented_e,
                  NYC.M.PCT_rented_s,
                  
                  NYC.M.N.BelPov_e,
                  NYC.M.N.BelPov_s,
                  NYC.M.N.TotPop_e,
                  NYC.M.N.TotPop_s,
                  NYC.M.N.TotHH_e,
                  NYC.M.N.TotHH_s,
                  NYC.M.N.HH_No_Car_e,
                  NYC.M.N.HH_No_Car_s,
                  NYC.M.N.Total_Occ_units_e,
                  NYC.M.N.Total_Occ_units_s,
                  NYC.M.N.Rented_units_e,
                  NYC.M.N.Rented_units_s,
                  NYC.M.N.Pov_rate_e,
                  NYC.M.N.Pov_rate_s,
                  NYC.M.N.PCT_No_Car_e,
                  NYC.M.N.PCT_No_Car_s,
                  NYC.M.N.PCT_rented_e,
                  NYC.M.N.PCT_rented_s,
                  
                  NYC.E.BelPov_e,
                  NYC.E.BelPov_s,
                  NYC.E.TotPop_e,
                  NYC.E.TotPop_s,
                  NYC.E.TotHH_e,
                  NYC.E.TotHH_s,
                  NYC.E.HH_No_Car_e,
                  NYC.E.HH_No_Car_s,
                  NYC.E.Total_Occ_units_e,
                  NYC.E.Total_Occ_units_s,
                  NYC.E.Rented_units_e,
                  NYC.E.Rented_units_s,
                  NYC.E.Pov_rate_e,
                  NYC.E.Pov_rate_s,
                  NYC.E.PCT_No_Car_e,
                  NYC.E.PCT_No_Car_s,
                  NYC.E.PCT_rented_e,
                  NYC.E.PCT_rented_s,
                  
                  NYC.E.N.BelPov_e,
                  NYC.E.N.BelPov_s,
                  NYC.E.N.TotPop_e,
                  NYC.E.N.TotPop_s,
                  NYC.E.N.TotHH_e,
                  NYC.E.N.TotHH_s,
                  NYC.E.N.HH_No_Car_e,
                  NYC.E.N.HH_No_Car_s,
                  NYC.E.N.Total_Occ_units_e,
                  NYC.E.N.Total_Occ_units_s,
                  NYC.E.N.Rented_units_e,
                  NYC.E.N.Rented_units_s,
                  NYC.E.N.Pov_rate_e,
                  NYC.E.N.Pov_rate_s,
                  NYC.E.N.PCT_No_Car_e,
                  NYC.E.N.PCT_No_Car_s,
                  NYC.E.N.PCT_rented_e,
                  NYC.E.N.PCT_rented_s
                  
)

# lets break the final table, and save facility subgroup data separately. Otherwise my brain is exploding

final_table_facility_subgroups <- final_table[,c(1,120:194)]

write.csv(final_table_facility_subgroups,
          row.names = FALSE,
          "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/stormwater_analysis_final_database_facility_subgroups_NOSLR.csv")

final_table <- final_table[,-(120:194)]

final_table_NYC <- rbind(final_table, NYC_row_data)

write.csv(final_table_NYC,
          row.names = FALSE,
          "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/stormwater_analysis_final_database_NOSLR.csv")

rm(AOI, 
   demographic_summary, 
   E.demographic_summary, 
   M.demographic_summary, 
   E.N.demographic_summary, 
   M.N.demographic_summary, 
   ext.flood, mod.flood, 
   tax_lots_flooding, 
   sliced, 
   Flood_summary_all, 
   NYC_2010.temp, 
   NYC_2010, 
   roads, 
   roads_cd, 
   roads_cd.buf, 
   roads_cd.buf.int, 
   roads_cd.buf.int.union, 
   neighborhood, 
   m.exposed_population,
   m.unexposed_population, 
   facilities_data_points, 
   fips_codes, 
   e.exposed_population, 
   e.unexposed_population, 
   extreme_flooding.int.road, 
   moderate_flooding.int.road, 
   NYC_flooding_blocks.joined.CDs,
   tax_lots_flooding.centroids,
   tax_lots_flooding.centroids_NA,
   NYC.BelPov_e,
   NYC.BelPov_s,
   NYC.TotPop_e,
   NYC.TotPop_s,
   NYC.TotHH_e,
   NYC.TotHH_s,
   NYC.HH_No_Car_e,
   NYC.HH_No_Car_s,
   NYC.Total_Occ_units_e,
   NYC.Total_Occ_units_s,
   NYC.Rented_units_e,
   NYC.Rented_units_s,
   NYC.Pov_rate_e,
   NYC.Pov_rate_s,
   NYC.PCT_No_Car_e,
   NYC.PCT_No_Car_s,
   NYC.PCT_rented_e,
   NYC.PCT_rented_s,
   NYC.M.BelPov_e,
   NYC.M.BelPov_s,
   NYC.M.TotPop_e,
   NYC.M.TotPop_s,
   NYC.M.TotHH_e,
   NYC.M.TotHH_s,
   NYC.M.HH_No_Car_e,
   NYC.M.HH_No_Car_s,
   NYC.M.Total_Occ_units_e,
   NYC.M.Total_Occ_units_s,
   NYC.M.Rented_units_e,
   NYC.M.Rented_units_s,
   NYC.M.Pov_rate_e,
   NYC.M.Pov_rate_s,
   NYC.M.PCT_No_Car_e,
   NYC.M.PCT_No_Car_s,
   NYC.M.PCT_rented_e,
   NYC.M.PCT_rented_s,
   NYC.M.N.BelPov_e,
   NYC.M.N.BelPov_s,
   NYC.M.N.TotPop_e,
   NYC.M.N.TotPop_s,
   NYC.M.N.TotHH_e,
   NYC.M.N.TotHH_s,
   NYC.M.N.HH_No_Car_e,
   NYC.M.N.HH_No_Car_s,
   NYC.M.N.Total_Occ_units_e,
   NYC.M.N.Total_Occ_units_s,
   NYC.M.N.Rented_units_e,
   NYC.M.N.Rented_units_s,
   NYC.M.N.Pov_rate_e,
   NYC.M.N.Pov_rate_s,
   NYC.M.N.PCT_No_Car_e,
   NYC.M.N.PCT_No_Car_s,
   NYC.M.N.PCT_rented_e,
   NYC.M.N.PCT_rented_s,
   NYC.E.BelPov_e,
   NYC.E.BelPov_s,
   NYC.E.TotPop_e,
   NYC.E.TotPop_s,
   NYC.E.TotHH_e,
   NYC.E.TotHH_s,
   NYC.E.HH_No_Car_e,
   NYC.E.HH_No_Car_s,
   NYC.E.Total_Occ_units_e,
   NYC.E.Total_Occ_units_s,
   NYC.E.Rented_units_e,
   NYC.E.Rented_units_s,
   NYC.E.Pov_rate_e,
   NYC.E.Pov_rate_s,
   NYC.E.PCT_No_Car_e,
   NYC.E.PCT_No_Car_s,
   NYC.E.PCT_rented_e,
   NYC.E.PCT_rented_s,
   NYC.E.N.BelPov_e,
   NYC.E.N.BelPov_s,
   NYC.E.N.TotPop_e,
   NYC.E.N.TotPop_s,
   NYC.E.N.TotHH_e,
   NYC.E.N.TotHH_s,
   NYC.E.N.HH_No_Car_e,
   NYC.E.N.HH_No_Car_s,
   NYC.E.N.Total_Occ_units_e,
   NYC.E.N.Total_Occ_units_s,
   NYC.E.N.Rented_units_e,
   NYC.E.N.Rented_units_s,
   NYC.E.N.Pov_rate_e,
   NYC.E.N.Pov_rate_s,
   NYC.E.N.PCT_No_Car_e,
   NYC.E.N.PCT_No_Car_s,
   NYC.E.N.PCT_rented_e,
   NYC.E.N.PCT_rented_s,
   cd_code,
   columns,
   comdist,
   commdist,
   E.Flooded_nr_emergency_facilities,
   E.Flooded_nr_human_facilities,
   E.Flooded_PCT_human_facilities,
   E.Flooded_PCT_emergency_facilities,
   E.nr_commercial,
   E.nr_industrial,
   E.nr_mixed,
   E.nr_open,
   E.nr_parking,
   E.nr_publicfac,
   E.nr_res_bsmt,
   E.nr_residential,
   E.nr_transportation,
   E.nr_vacant,
   ext.1ft,
   ext.4.in,
   M.Flooded_nr_emergency_facilities,
   M.Flooded_nr_human_facilities,
   M.Flooded_PCT_human_facilities,
   M.Flooded_PCT_emergency_facilities,
   M.nr_commercial,
   M.nr_industrial,
   M.nr_mixed,
   M.nr_open,
   M.nr_parking,
   M.nr_publicfac,
   M.nr_res_bsmt,
   M.nr_residential,
   M.nr_transportation,
   M.nr_vacant,
   mod.1ft,
   mod.4.in,
   nr_commercial,
   nr_industrial,
   nr_mixed,
   nr_open,
   nr_parking,
   nr_publicfac,
   nr_res_bsmt,
   nr_residential,
   nr_transportation,
   nr_vacant,
   total_nr_emergency_facilities,
   total_nr_human_facilities,
   NYC_row_data,
   SOVI_data,
   SOVI_data.CD,
   ACS_demographic_summary,
   E.ACS_demographic_summary,
   M.ACS_demographic_summary,
   
   E.N.ACS_demographic_summary,
   M.N.ACS_demographic_summary,
   
   E.Flooded_nr_BusStops,
   E.Flooded_nr_facility,
   E.Flooded_nr_routes,
   E.Flooded_nr_SubwayEntrances,
   E.Flooded_PCT_BusStops,
   E.Flooded_PCT_facility,
   E.Flooded_PCT_routes,
   E.Flooded_PCT_SubwayEntrances,
   facility_cat,
   M.Flooded_nr_BusStops,
   M.Flooded_nr_facility,
   M.Flooded_nr_routes,
   M.Flooded_nr_SubwayEntrances,
   M.Flooded_PCT_BusStops,
   M.Flooded_PCT_facility,
   M.Flooded_PCT_routes,
   M.Flooded_PCT_SubwayEntrances,
   total_nr_BusStops,
   total_nr_facility,
   total_nr_routes,
   total_nr_SubwayEntrances,
   bus_routes,
   bus_stops,
   bus_stops_CD,
   cd_routes,
   CD_with_parks,
   E.Flooded_routes,
   M.Flooded_routes,
   subway_entrances,
   subway_entrances_CD,
   E.Flooded_nr_subgrp,
   M.Flooded_nr_subgrp,
   facility_categories,
   facility_subgroups,
   facility_subgrp,
   total_nr_subgrp)


gc()

### Prepare summary NYC table for paper

data_NYC <- read_csv("C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/stormwater_analysis_final_database_NOSLR.csv") %>%
  filter(Geography == "NYC") %>%
  mutate(total.bipoc = data_NYC$`Total population` - data_NYC$`Total White`,
         total.pct.bipoc = 100 - data_NYC$`PCT White`, 
         M.pct_TotPop = 100 * data_NYC$`M.Flooded total population` / data_NYC$`Total population`,
         M.total.bipoc = data_NYC$`M.Flooded total population` - data_NYC$`M.Flooded total White`,
         M.pct_BIPOC = 100 - data_NYC$`M.Flooded PCT White`,
         E.pct_TotPop = 100 * data_NYC$`E.Flooded total population` / data_NYC$`Total population`,
         E.total.bipoc = data_NYC$`E.Flooded total population` - data_NYC$`E.Flooded total White`,
         E.pct_BIPOC = 100 - data_NYC$`E.Flooded PCT White`)

df_summary_NYC <- data.frame(category = c("Area Flooded (km2)",
                                          "Population",
                                          "Road area (km2)", 
                                          "Bus stops", 
                                          "Bus routes",
                                          "Subway entrances",
                                          "Residential lots affected without a full basement",
                                          "Residential lots with a full basement",
                                          "Mixed Commercial and Residential Lots",
                                          "Commercial Lots",
                                          "Industrial Lots",
                                          "Black, Indigenous, People of Color (BIPOC)",
                                          "Children population",
                                          "Elderly population",
                                          "Poverty",
                                          "Households without a car",
                                          "Renter-occupied households"),
                             M.absolute = c(data_NYC$`Moderate total flood area`/1000000,
                                            data_NYC$`M.Flooded total population`,
                                            data_NYC$`M.Flooded road area`/1000000,
                                            data_NYC$`M.Flooded number of bus stops`,
                                            data_NYC$`M.Flooded number of bus routes`,
                                            data_NYC$`M.Flooded number of subway entrances`,
                                            data_NYC$`M.Flooded total residential`,
                                            data_NYC$`M.Flooded below grade basement residential`,
                                            data_NYC$`M.Flooded total mixed commercial & residential`,
                                            data_NYC$`M.Flooded total commercial`,
                                            data_NYC$`M.Flooded total industrial`,
                                            data_NYC$M.total.bipoc,
                                            data_NYC$`M.Flooded PCT 0-5 children`,
                                            data_NYC$`M.Flooded PCT +65 elder`,
                                            data_NYC$M.Total_BelPov_e,
                                            data_NYC$M.HH_No_Car_e,
                                            data_NYC$M.Rented_units_e),
                             M.pct = c(data_NYC$`Moderate total pct flood`,
                                       data_NYC$M.pct_TotPop,
                                       data_NYC$`M.Flooded PCT road`,
                                       data_NYC$`M.Flooded PCT bus stops`,
                                       data_NYC$`M.Flooded PCT bus routes`,
                                       data_NYC$`M.Flooded PCT subway entrances`,
                                       data_NYC$`M.Flooded PCT residential`,
                                       data_NYC$`M.Flooded PCT below grade basement residential`,
                                       data_NYC$`M.Flooded PCT mixed commercial & residential`,
                                       data_NYC$`M.Flooded PCT commercial`,
                                       data_NYC$`M.Flooded PCT industrial`,
                                       data_NYC$M.pct_BIPOC,
                                       data_NYC$`M.Flooded PCT 0-5 children`,
                                       data_NYC$`M.Flooded PCT +65 elder`,
                                       data_NYC$M.Pov_rate_e,
                                       data_NYC$M.PCT_No_Car_e,
                                       data_NYC$M.PCT_rented_e),
                             E.absolute = c(data_NYC$`Extreme total flood area`/1000000,
                                            data_NYC$`E.Flooded total population`,
                                            data_NYC$`E.Flooded road area`/1000000,
                                            data_NYC$`E.Flooded number of bus stops`,
                                            data_NYC$`E.Flooded number of bus routes`,
                                            data_NYC$`E.Flooded number of subway entrances`,
                                            data_NYC$`E.Flooded total residential`,
                                            data_NYC$`E.Flooded below grade basement residential`,
                                            data_NYC$`E.Flooded total mixed commercial & residential`,
                                            data_NYC$`E.Flooded total commercial`,
                                            data_NYC$`E.Flooded total industrial`,
                                            data_NYC$E.total.bipoc,
                                            data_NYC$`E.Flooded PCT 0-5 children`,
                                            data_NYC$`E.Flooded PCT +65 elder`,
                                            data_NYC$E.Total_BelPov_e,
                                            data_NYC$E.HH_No_Car_e,
                                            data_NYC$E.Rented_units_e),
                             E.pct = c(data_NYC$`Extreme total pct flood`,
                                       data_NYC$E.pct_TotPop,
                                       data_NYC$`E.Flooded PCT road`,
                                       data_NYC$`E.Flooded PCT bus stops`,
                                       data_NYC$`E.Flooded PCT bus routes`,
                                       data_NYC$`E.Flooded PCT subway entrances`,
                                       data_NYC$`E.Flooded PCT residential`,
                                       data_NYC$`E.Flooded PCT below grade basement residential`,
                                       data_NYC$`E.Flooded PCT mixed commercial & residential`,
                                       data_NYC$`E.Flooded PCT commercial`,
                                       data_NYC$`E.Flooded PCT industrial`,
                                       data_NYC$E.pct_BIPOC,
                                       data_NYC$`E.Flooded PCT 0-5 children`,
                                       data_NYC$`E.Flooded PCT +65 elder`,
                                       data_NYC$E.Pov_rate_e,
                                       data_NYC$E.PCT_No_Car_e,
                                       data_NYC$E.PCT_rented_e),
                             citywide.absolute = c(data_NYC$`Total area`/1000000,
                                                   data_NYC$`Total population`,
                                                   data_NYC$`Total road area`/1000000,
                                                   data_NYC$`Total number of bus stops`,
                                                   data_NYC$`Total number of bus routes`,
                                                   data_NYC$`Total number of subway entrances`,
                                                   data_NYC$`Total residential tax lots`,
                                                   data_NYC$`Total below grade basement residential`,
                                                   data_NYC$`Total mixed commercial & residential tax lots`,
                                                   data_NYC$`Total commercial tax lots`,
                                                   data_NYC$`Total industrial tax lots`,
                                                   data_NYC$total.bipoc,
                                                   data_NYC$`Total 0-5 children`,
                                                   data_NYC$`Total +65 elder`,
                                                   data_NYC$Total_BelPov_e,
                                                   data_NYC$HH_No_Car_e,
                                                   data_NYC$Rented_units_e),
                             citywide.pct = c(100,
                                              100,
                                              100,
                                              100,
                                              100,
                                              100,
                                              100,
                                              100,
                                              100,
                                              100,
                                              100,
                                              data_NYC$total.pct.bipoc,
                                              data_NYC$`PCT 0-5 children`,
                                              data_NYC$`PCT +65 elder`,
                                              data_NYC$Pov_rate_e,
                                              data_NYC$PCT_No_Car_e,
                                              data_NYC$PCT_rented_e))

write.csv(df_summary_NYC,
          row.names = FALSE,
          "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/stormwater_analysis_final_NYC_summary_NOSLR.csv")

### Maps per CD per indicator with quintile classification

# create a sf with final table x CD and plot MODERATE scenario because it makes a higher difference and it is more urgent


Map.P_Fac_Public_Safety <- ggplot() +
  geom_sf(data = final_table_sf, aes(fill = factor(quintile_label(final_table_sf, "M.Flooded PCT of Public Safety facilities")))) +
  theme_map() +
  theme(legend.position = "none",
        title = element_text(size = 18)) + 
  labs(title = "% Public Safety Facilities") +
  scale_fill_manual(values = c("#CCE0EB", "#3485AD", "#016699"), name= "Quintile")

Map.P_Fac_Healthcare <- ggplot() +
  geom_sf(data = final_table_sf, aes(fill = factor(quintile_label(final_table_sf, "M.Flooded PCT of Healthcare facilities")))) +
  theme_map() +
  theme(legend.position = "none",
        title = element_text(size = 18)) + 
  labs(title = "% Healthcare Facilities") +
  scale_fill_manual(values = c("#CCE0EB", "#99C2D6", "#67A3C2","#3485AD", "#016699"), name= "Quintile")

Map.P_Fac_Education <- ggplot() +
  geom_sf(data = final_table_sf, aes(fill = factor(quintile_label(final_table_sf, "M.Flooded PCT of Education facilities")))) +
  theme_map() +
  theme(legend.position = "none",
        title = element_text(size = 18)) + 
  labs(title = "% Education Facilities") +
  scale_fill_manual(values = c("#CCE0EB", "#99C2D6", "#67A3C2","#3485AD", "#016699"), name= "Quintile")

Map.P_Fac_Welfare <- ggplot() +
  geom_sf(data = final_table_sf, aes(fill = factor(quintile_label(final_table_sf, "M.Flooded PCT of Human Welfare Services facilities")))) +
  theme_map() +
  theme(legend.position = "none",
        title = element_text(size = 18)) + 
  labs(title = "% Subway Entrances Impacted") +
  scale_fill_manual(values = c("#CCE0EB", "#99C2D6", "#67A3C2","#3485AD", "#016699"), name= "Quintile")

frame <- ggarrange(Map.P_Fac_Public_Safety,
                   Map.P_Fac_Healthcare,
                   Map.P_Fac_Education,
                   Map.P_Fac_Welfare,
                   Map.Legend,
                   nrow = 3, ncol = 2) %>%
  annotate_figure(top = text_grob("\n Critical Facilities Exposure Indicators Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 25))


frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_Facilities_exposure.png", width = 850, height = 1300) 

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


### barplots for the city

data_plot_NYC <- read_xlsx("C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/PostProcess_stormwater_analysis_final_database_NOSLR.xlsx", sheet = "ParaPlotear") %>%
  rename_all(make.names) %>% filter(CD == "NYC") %>% filter(Scenario != "Total")

data_plot_NYC$Scenario <- factor(data_plot_NYC$Scenario, levels = c("Extreme", "Moderate"))

plot.P_Area_Flooded <- ggplot(data_plot_NYC, aes(fill = Scenario, y = PCT_Area_Flooded, x = Scenario)) +
  geom_bar(position="dodge", stat="identity", show.legend = FALSE, width = 0.7) +
  geom_text(aes(label=paste0(round((Area_Flooded/10000),1), "ha")), size = 7, position=position_dodge(width=0.9), hjust=-.1) +
  scale_fill_manual(values=c('#004466','#3485AD', "#777678")) + 
  labs( y = "% Total Area Flooded") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18),
        plot.margin = margin(1, 3.5, 1, 1, "cm"),
        aspect.ratio = 1/2) +
  coord_flip(clip = "off")

ggsave(plot.P_Area_Flooded, filename = paste0("C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/Citywide_barchart_Area", ".png"),
       dpi = 900,
       width = 10,
       height = 10,
       units = "in",
       bg = "transparent")

plot.P_TotPop_Flooded <- ggplot(data_plot_NYC, aes(fill = Scenario, y = PCT_Total_Population, x = Scenario)) +
  geom_bar(position="dodge", stat="identity", show.legend = FALSE, width = 0.7) +
  geom_text(aes(label=Total_Population), size = 7, position=position_dodge(width=0.9), hjust=-.1) +
  scale_fill_manual(values=c('#004466', '#3485AD',"#777678")) + 
  labs(y = "% Total Population Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18),
        plot.margin = margin(1, 3.5, 1, 1, "cm"),
        aspect.ratio = 1/2) +
  coord_flip(clip = "off")

ggsave(plot.P_TotPop_Flooded, filename = paste0("C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/Citywide_barchart_Population", ".png"),
       dpi = 900,
       width = 10,
       height = 10,
       units = "in",
       bg = "transparent")

