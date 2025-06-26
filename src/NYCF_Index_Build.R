# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL CREATE A SERIES OF INDICES THAT AGGREGATE THEMATIC INDICATORS DEVELOPED IN THE "NYCF_report_CD_impacts.R" SCRIPT.

## we begin loading the libraries, functions, and variables that we will recurrently use

source("src/NYCF_housekeeping_GIS_vars.R")

## and the final table that we will use to compute the indices at the CD level

final_table <- read_csv("data/3_output/stormwater_analysis_final_database.csv") %>%
  filter(Geography != "NYC") %>%                # we dont need the citywide row here, as we are interested in finding risk hotstpots at the CD level
  mutate(Geography = as.numeric(Geography))

## we bring back the facilities database to have the fuller set of indicators

facilities <- read_csv("data/3_output/stormwater_analysis_final_database_facility_subgroups.csv") %>%
  mutate(Geography = as.numeric(Geography))

facilities[is.na(facilities)] <- 0

final_table <- inner_join(final_table, facilities, by = "Geography")

## we will develop Hotspot Scores depicting exposures and vulnerability under each of the two scenarios considered. 
## Hotspot Scores are first calculated at the indicator level by ranking CDs in quintiles
## then, quintiles are summed and divided by the maximum possible value (5 x number of aggregated quintile scores).
## the result is the Hotspot Score for a given risk category (see equation 1 in the manuscript's methods)

## MODERATE SCENARIO

## Risk Attribute 1: Population Exposure 

### prefix M.HS" implies that the new field generated corresponds to the Hotspot Score (quintile ranking) of the given indicator under the moderate scenario
### in this case, it means that for each CD we calculated the quintile score for "% population exposed under the moderate scenario"

final_table <- final_table %>%
  mutate("M.HS.PCT_Total_population" = quintile_label(final_table, "M.PCT_Total_population", drop_geom = FALSE)) %>% 
  mutate(M.HS.Agg_population = M.HS.PCT_Total_population/5)

## Risk Attribute 2: Critical Infrastructures and Services
## This risk attribute counts with several categories, so we must first aggregate them separately and then run a re-aggregation to generate a final index

## Risk Category 2.1: land use

final_table <- final_table %>%
  mutate("M.HS.PCT_residential_lots" = quintile_label(final_table, "M.PCT_residential_lots", drop_geom = FALSE),
         "M.HS.PCT_basement_residential_lots" = quintile_label(final_table, "M.PCT_basement_residential_lots", drop_geom = FALSE),
         "M.HS.PCT_mixed_comres_lots" = quintile_label(final_table, "M.PCT_mixed_comres_lots", drop_geom = FALSE),
         "M.HS.PCT_commercial_lots" = quintile_label(final_table, "M.PCT_commercial_lots", drop_geom = FALSE),
         "M.HS.PCT_industrial_lots" = quintile_label(final_table, "M.PCT_industrial_lots", drop_geom = FALSE)
  ) %>% 
  
  mutate(M.HS.Agg_landuse = (M.HS.PCT_residential_lots +
                             M.HS.PCT_basement_residential_lots  +
                             M.HS.PCT_mixed_comres_lots +
                             M.HS.PCT_commercial_lots +
                             M.HS.PCT_industrial_lots)/25)

## Risk Category 2.2: transportation

final_table <- final_table %>%
  mutate("M.HS.PCT_road_area" = quintile_label(final_table, "M.PCT_road_area", drop_geom = FALSE),
         "M.HS.PCT_bus_stops" = quintile_label(final_table, "M.PCT_bus_stops", drop_geom = FALSE),
         "M.HS.PCT_bus_routes" = quintile_label(final_table, "M.PCT_bus_routes", drop_geom = FALSE),
         "M.HS.PCT.PCT_subway_stops" = quintile_label(final_table, "M.PCT_subway_stops", drop_geom = FALSE)
  ) %>% 
  mutate(M.HS.Agg_transport = (M.HS.PCT_road_area +
                               M.HS.PCT_bus_stops  +
                               M.HS.PCT_bus_routes +
                               M.HS.PCT.PCT_subway_stops)/20)

## Risk Category 2.3: public safety

final_table <- final_table %>%
  mutate("M.HS.PCT_POLICE_SERVICES_facilities" = quintile_label(final_table, "M.PCT_POLICE_SERVICES_facilities", drop_geom = FALSE),
         "M.HS.PCT_FIRE_SERVICES_facilities" = quintile_label(final_table, "M.PCT_FIRE_SERVICES_facilities", drop_geom = FALSE),
         "M.HS.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities" = quintile_label(final_table, "M.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities", drop_geom = FALSE)
  ) %>% 
  mutate(M.HS.Agg_psafety = (M.HS.PCT_POLICE_SERVICES_facilities +
                               M.HS.PCT_FIRE_SERVICES_facilities  +
                               M.HS.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities)/15)


## Risk Category 2.4: healthcare

final_table <- final_table %>%
  mutate("M.HS.PCT_HOSPITALS_AND_CLINICS_facilities" = quintile_label(final_table, "M.PCT_HOSPITALS_AND_CLINICS_facilities" , drop_geom = FALSE),
         "M.HS.PCT_RESIDENTIAL_HEALTH_CARE_facilities" = quintile_label(final_table, "M.PCT_RESIDENTIAL_HEALTH_CARE_facilities" , drop_geom = FALSE),
         "M.HS.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities" = quintile_label(final_table, "M.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities" , drop_geom = FALSE),
         "M.HS.PCT_OTHER_HEALTH_CARE_facilities" = quintile_label(final_table, "M.PCT_OTHER_HEALTH_CARE_facilities" , drop_geom = FALSE)
  ) %>% 
  mutate(M.HS.Agg_healthcare = (M.HS.PCT_HOSPITALS_AND_CLINICS_facilities +
                                  M.HS.PCT_RESIDENTIAL_HEALTH_CARE_facilities  +
                                  M.HS.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities +
                                  M.HS.PCT_OTHER_HEALTH_CARE_facilities)/20)


## Risk Category 2.5: welfare services

final_table <- final_table %>%
  mutate("M.HS.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities" = quintile_label(final_table, "M.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities" , drop_geom = FALSE),
         "M.HS.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities" = quintile_label(final_table, "M.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities" , drop_geom = FALSE),
         "M.HS.PCT_HOUSING_AND_FOOD_SERVICES_facilities" = quintile_label(final_table, "M.PCT_HOUSING_AND_FOOD_SERVICES_facilities" , drop_geom = FALSE),
         "M.HS.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities" = quintile_label(final_table, "M.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities" , drop_geom = FALSE)
  ) %>% 
  mutate(M.HS.Agg_welfare = (M.HS.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities +
                               M.HS.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities  +
                               M.HS.PCT_HOUSING_AND_FOOD_SERVICES_facilities +
                               M.HS.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities)/20)

## Risk Category 2.6: education

final_table <- final_table %>%
  mutate("M.HS.PCT_K12_EDUCATION_SERVICES_facilities" = quintile_label(final_table, "M.PCT_K12_EDUCATION_SERVICES_facilities", drop_geom = FALSE),
         "M.HS.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities" = quintile_label(final_table, "M.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities", drop_geom = FALSE),
         "M.HS.PCT_COLLEGES_OR_UNIVERSITIES_facilities" = quintile_label(final_table, "M.PCT_COLLEGES_OR_UNIVERSITIES_facilities", drop_geom = FALSE),
         "M.HS.PCT_PROPRIETARY_SCHOOLS_facilities" = quintile_label(final_table, "M.PCT_PROPRIETARY_SCHOOLS_facilities", drop_geom = FALSE)
  ) %>% 
  mutate(M.HS.Agg_education = (M.HS.PCT_K12_EDUCATION_SERVICES_facilities +
                                 M.HS.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities  +
                                 M.HS.PCT_COLLEGES_OR_UNIVERSITIES_facilities +
                                 M.HS.PCT_PROPRIETARY_SCHOOLS_facilities)/20)


## now we can aggregate the hotspot scores of the different risk categories under Risk Attribute - Critical Infrastructures and Services
## to generate a final exposure index
## we do so by summing all hotspot scores and normalizing the result to a 0-1 range

final_table$M.Infra_hotspots_sum_n <- round(normalize(final_table$M.HS.Agg_landuse +
                                                        final_table$M.HS.Agg_transport +
                                                        final_table$M.HS.Agg_education +
                                                        final_table$M.HS.Agg_welfare +
                                                        final_table$M.HS.Agg_psafety +
                                                        final_table$M.HS.Agg_healthcare), 2)



### Risk Attribute 3: Social Vulnerability

final_table <- final_table %>%
  mutate("M.HS.PCT_above65_population" = quintile_label(final_table, "M.PCT_above65_population", drop_geom = FALSE),
         "M.HS.PCT_below5_population" = quintile_label(final_table, "M.PCT_below5_population", drop_geom = FALSE),
         "M.HS.PCT_bipoc_population" = quintile_label(final_table, "M.PCT_bipoc_population", drop_geom = FALSE),
         "M.HS.PCT_Belpov_e" = quintile_label(final_table, "M.PCT_Belpov_e", drop_geom = FALSE),
         "M.HS.PCT_Disability_e" = quintile_label(final_table, "M.PCT_Disability_e", drop_geom = FALSE),
         "M.HS.PCT_HH_Income_Bel75K_e" = quintile_label(final_table, "M.PCT_HH_Income_Bel75K_e", drop_geom = FALSE),
         "M.HS.PCT_RB_e" = quintile_label(final_table, "M.PCT_RB_e", drop_geom = FALSE),
         "M.HS.PCT_CB_e" = quintile_label(final_table, "M.PCT_CB_e", drop_geom = FALSE)
  ) %>% 
  mutate(M.HS.Agg_sovi = (M.HS.PCT_above65_population +
                          M.HS.PCT_below5_population  +
                          M.HS.PCT_bipoc_population +
                          M.HS.PCT_Belpov_e +
                          M.HS.PCT_Disability_e +
                          M.HS.PCT_HH_Income_Bel75K_e +
                          M.HS.PCT_RB_e +
                          M.HS.PCT_CB_e)/40)

## EXTREME SCENARIO

## Risk Attribute 1: Population Exposure 

final_table <- final_table %>%
  mutate("E.HS.PCT_Total_population" = quintile_label(final_table, "E.PCT_Total_population", drop_geom = FALSE)) %>% 
  mutate(E.HS.Agg_population = E.HS.PCT_Total_population/5)

## Risk Attribute 2: Critical Infrastructures and Services
## This risk attribute counts with several categories, so we must first aggregate them separately and then run a re-aggregation to generate a final index

## Risk Category 2.1: land use

final_table <- final_table %>%
  mutate("E.HS.PCT_residential_lots" = quintile_label(final_table, "E.PCT_residential_lots", drop_geom = FALSE),
         "E.HS.PCT_basement_residential_lots" = quintile_label(final_table, "E.PCT_basement_residential_lots", drop_geom = FALSE),
         "E.HS.PCT_mixed_comres_lots" = quintile_label(final_table, "E.PCT_mixed_comres_lots", drop_geom = FALSE),
         "E.HS.PCT_commercial_lots" = quintile_label(final_table, "E.PCT_commercial_lots", drop_geom = FALSE),
         "E.HS.PCT_industrial_lots" = quintile_label(final_table, "E.PCT_industrial_lots", drop_geom = FALSE)
  ) %>% 
  
  mutate(E.HS.Agg_landuse = (E.HS.PCT_residential_lots +
                               E.HS.PCT_basement_residential_lots  +
                               E.HS.PCT_mixed_comres_lots +
                               E.HS.PCT_commercial_lots +
                               E.HS.PCT_industrial_lots)/25)

## Risk Category 2.2: transportation

final_table <- final_table %>%
  mutate("E.HS.PCT_road_area" = quintile_label(final_table, "E.PCT_road_area", drop_geom = FALSE),
         "E.HS.PCT_bus_stops" = quintile_label(final_table, "E.PCT_bus_stops", drop_geom = FALSE),
         "E.HS.PCT_bus_routes" = quintile_label(final_table, "E.PCT_bus_routes", drop_geom = FALSE),
         "E.HS.PCT.PCT_subway_stops" = quintile_label(final_table, "E.PCT_subway_stops", drop_geom = FALSE)
  ) %>% 
  mutate(E.HS.Agg_transport = (E.HS.PCT_road_area +
                                 E.HS.PCT_bus_stops  +
                                 E.HS.PCT_bus_routes +
                                 E.HS.PCT.PCT_subway_stops)/20)

## Risk Category 2.3: public safety

final_table <- final_table %>%
  mutate("E.HS.PCT_POLICE_SERVICES_facilities" = quintile_label(final_table, "E.PCT_POLICE_SERVICES_facilities", drop_geom = FALSE),
         "E.HS.PCT_FIRE_SERVICES_facilities" = quintile_label(final_table, "E.PCT_FIRE_SERVICES_facilities", drop_geom = FALSE),
         "E.HS.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities" = quintile_label(final_table, "E.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities", drop_geom = FALSE)
  ) %>% 
  mutate(E.HS.Agg_psafety = (E.HS.PCT_POLICE_SERVICES_facilities +
                               E.HS.PCT_FIRE_SERVICES_facilities  +
                               E.HS.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities)/15)


## Risk Category 2.4: healthcare

final_table <- final_table %>%
  mutate("E.HS.PCT_HOSPITALS_AND_CLINICS_facilities" = quintile_label(final_table, "E.PCT_HOSPITALS_AND_CLINICS_facilities" , drop_geom = FALSE),
         "E.HS.PCT_RESIDENTIAL_HEALTH_CARE_facilities" = quintile_label(final_table, "E.PCT_RESIDENTIAL_HEALTH_CARE_facilities" , drop_geom = FALSE),
         "E.HS.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities" = quintile_label(final_table, "E.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities" , drop_geom = FALSE),
         "E.HS.PCT_OTHER_HEALTH_CARE_facilities" = quintile_label(final_table, "E.PCT_OTHER_HEALTH_CARE_facilities" , drop_geom = FALSE)
  ) %>% 
  mutate(E.HS.Agg_healthcare = (E.HS.PCT_HOSPITALS_AND_CLINICS_facilities +
                                  E.HS.PCT_RESIDENTIAL_HEALTH_CARE_facilities  +
                                  E.HS.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities +
                                  E.HS.PCT_OTHER_HEALTH_CARE_facilities)/20)


## Risk Category 2.5: welfare services

final_table <- final_table %>%
  mutate("E.HS.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities" = quintile_label(final_table, "E.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities" , drop_geom = FALSE),
         "E.HS.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities" = quintile_label(final_table, "E.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities" , drop_geom = FALSE),
         "E.HS.PCT_HOUSING_AND_FOOD_SERVICES_facilities" = quintile_label(final_table, "E.PCT_HOUSING_AND_FOOD_SERVICES_facilities" , drop_geom = FALSE),
         "E.HS.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities" = quintile_label(final_table, "E.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities" , drop_geom = FALSE)
  ) %>% 
  mutate(E.HS.Agg_welfare = (E.HS.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities +
                               E.HS.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities  +
                               E.HS.PCT_HOUSING_AND_FOOD_SERVICES_facilities +
                               E.HS.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities)/20)

## Risk Category 2.6: education

final_table <- final_table %>%
  mutate("E.HS.PCT_K12_EDUCATION_SERVICES_facilities" = quintile_label(final_table, "E.PCT_K12_EDUCATION_SERVICES_facilities", drop_geom = FALSE),
         "E.HS.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities" = quintile_label(final_table, "E.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities", drop_geom = FALSE),
         "E.HS.PCT_COLLEGES_OR_UNIVERSITIES_facilities" = quintile_label(final_table, "E.PCT_COLLEGES_OR_UNIVERSITIES_facilities", drop_geom = FALSE),
         "E.HS.PCT_PROPRIETARY_SCHOOLS_facilities" = quintile_label(final_table, "E.PCT_PROPRIETARY_SCHOOLS_facilities", drop_geom = FALSE)
  ) %>% 
  mutate(E.HS.Agg_education = (E.HS.PCT_K12_EDUCATION_SERVICES_facilities +
                                 E.HS.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities  +
                                 E.HS.PCT_COLLEGES_OR_UNIVERSITIES_facilities +
                                 E.HS.PCT_PROPRIETARY_SCHOOLS_facilities)/20)


## now we can aggregate the hotspot scores of the different risk categories under Risk Attribute - Critical Infrastructures and Services
## to generate a final exposure index
## we do so by summing all hotspot scores and normalizing the result to a 0-1 range

final_table$E.Infra_hotspots_sum_n <- round(normalize(final_table$E.HS.Agg_landuse +
                                                        final_table$E.HS.Agg_transport +
                                                        final_table$E.HS.Agg_education +
                                                        final_table$E.HS.Agg_welfare +
                                                        final_table$E.HS.Agg_psafety +
                                                        final_table$E.HS.Agg_healthcare), 2)



### Risk Attribute 3: Social Vulnerability

final_table <- final_table %>%
  mutate("E.HS.PCT_above65_population" = quintile_label(final_table, "E.PCT_above65_population", drop_geom = FALSE),
         "E.HS.PCT_below5_population" = quintile_label(final_table, "E.PCT_below5_population", drop_geom = FALSE),
         "E.HS.PCT_bipoc_population" = quintile_label(final_table, "E.PCT_bipoc_population", drop_geom = FALSE),
         "E.HS.PCT_Belpov_e" = quintile_label(final_table, "E.PCT_Belpov_e", drop_geom = FALSE),
         "E.HS.PCT_Disability_e" = quintile_label(final_table, "E.PCT_Disability_e", drop_geom = FALSE),
         "E.HS.PCT_HH_Income_Bel75K_e" = quintile_label(final_table, "E.PCT_HH_Income_Bel75K_e", drop_geom = FALSE),
         "E.HS.PCT_RB_e" = quintile_label(final_table, "E.PCT_RB_e", drop_geom = FALSE),
         "E.HS.PCT_CB_e" = quintile_label(final_table, "E.PCT_CB_e", drop_geom = FALSE)
  ) %>% 
  mutate(E.HS.Agg_sovi = (E.HS.PCT_above65_population +
                            E.HS.PCT_below5_population  +
                            E.HS.PCT_bipoc_population +
                            E.HS.PCT_Belpov_e +
                            E.HS.PCT_Disability_e +
                            E.HS.PCT_HH_Income_Bel75K_e +
                            E.HS.PCT_RB_e +
                            E.HS.PCT_CB_e)/40)

write_csv(final_table, "data/3_output/stormwater_analysis_final_database_impact_indices.csv")


## we can use the hotspot scores developed to identify CDs ranking in the top of each Risk Attribute

M.CDs_HS_pop <- filter(final_table, M.HS.Agg_population >= 0.75)$Geography
M.CDs_HS_sovi <- filter(final_table, M.HS.Agg_sovi >= 0.75)$Geography
M.CDs_HS_infra <- filter(final_table, M.Infra_hotspots_sum_n >= 0.75)$Geography

E.CDs_HS_pop <- filter(final_table, E.HS.Agg_population >= 0.75)$Geography
E.CDs_HS_sovi <- filter(final_table, E.HS.Agg_sovi >= 0.75)$Geography
E.CDs_HS_infra <- filter(final_table, E.Infra_hotspots_sum_n >= 0.75)$Geography

table(c(M.CDs_HS_pop, M.CDs_HS_sovi, M.CDs_HS_infra)) # what CD appears 2 or 3 times? (meaning that it is in the top quartile at for 2 or 3 of the risk attributes)
table(c(E.CDs_HS_pop, E.CDs_HS_sovi, E.CDs_HS_infra))
