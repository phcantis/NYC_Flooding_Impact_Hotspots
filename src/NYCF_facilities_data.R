# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL MEASURE THE MINIMUM DISTANCE TO FLOODING OF NEW YORK CITY'S CRITICAL INFRASTRUCTURES AND SERVICES.

## this script is meant to be sourced directly from the script "NYCF_report_CD_impacts.R"
## due to datasets and functions being loaded in the parent script, running this file independently will require line-by-line checks for missing datasets

## we load the input data - NYC's facilities database
## we will also filter to keep those facility groups and subgroups that are relevant to the analysis

facilities_data_points <- st_read("data/1_raw/Facilities_20210811.shp") %>%
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
  dplyr::select(FACNAME,BBL, boro_cd = CD,BOROCODE,FACSUBGRP,FACGROUP) %>%
  st_transform(UTM_18N_meter) %>% 
  mutate(FACSUBGRP = case_when(FACSUBGRP %in% c("OTHER PUBLIC SAFETY",
                                                "OTHER EMERGENCY SERVICES") ~ "OTHER_SAFETY_AND_EMERGENCY_FACILITIES",
                               FACSUBGRP %in% c("MENTAL HEALTH",
                                                "CHEMICAL DEPENDENCY",
                                                "HEALTH PROMOTION AND DISEASE PREVENTION") ~ "MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES",
                               FACSUBGRP %in% c("COMMUNITY CENTERS AND COMMUNITY SCHOOL PROGRAMS",
                                                "FINANCIAL ASSISTANCE AND SOCIAL SERVICES",
                                                "WORKFORCE DEVELOPMENT",
                                                "LEGAL AND INTERVENTION SERVICES",
                                                "YOUTH CENTERS, LITERACY PROGRAMS, AND JOB TRAINING SERVICES") ~ "HUMAN_DEVELOPMENT_SERVICES",
                               FACSUBGRP %in% c("SENIOR SERVICES",
                                                "PROGRAMS FOR PEOPLE WITH DISABILITIES") ~ "SENIOR_AND_DISSABILITY_SERVICES",
                               FACSUBGRP %in% c("PERMANENT SUPPORTIVE SRO HOUSING",
                                                "NON-RESIDENTIAL HOUSING AND HOMELESS SERVICES",
                                                "SOUP KITCHENS AND FOOD PANTRIES") ~ "HOUSING_AND_FOOD_SERVICES",
                               FACSUBGRP %in% c("FOSTER CARE SERVICES AND RESIDENTIAL CARE",
                                                "CHILD NUTRITION") ~ "CHILD_SERVICES_AND_WELFARE_SERVICES",
                               FACSUBGRP %in% c("PUBLIC K-12 SCHOOLS",
                                                "CHARTER K-12 SCHOOLS",
                                                "NON-PUBLIC K-12 SCHOOLS",
                                                "PUBLIC AND PRIVATE SPECIAL EDUCATION SCHOOLS",
                                                "GED AND ALTERNATIVE HIGH SCHOOL EQUIVALENCY") ~ "K12_EDUCATION_SERVICES",
                               FACSUBGRP %in% c("DOE UNIVERSAL PRE-KINDERGARTEN",
                                                "DAY CARE",
                                                "PRESCHOOLS FOR STUDENTS WITH DISABILITIES",
                                                "DUAL CHILD CARE AND UNIVERSAL PRE-K") ~ "DAY_CARE_AND_PREKINDERGARTEN",
                               TRUE ~ paste0(.$FACSUBGRP, ""))) %>% 
  mutate(CatFac = case_when(FACSUBGRP %in% c("POLICE SERVICES", 
                                             "FIRE SERVICES",
                                             "OTHER_SAFETY_AND_EMERGENCY_FACILITIES") ~ "Public_Safety",
                            FACSUBGRP %in% c("HOSPITALS AND CLINICS", 
                                             "RESIDENTIAL HEALTH CARE",
                                             "MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES",
                                             "OTHER HEALTH CARE") ~ "Healthcare",
                            FACSUBGRP %in% c("HUMAN_DEVELOPMENT_SERVICES",
                                             "SENIOR_AND_DISSABILITY_SERVICES",
                                             "HOUSING_AND_FOOD_SERVICES",
                                             "CHILD_SERVICES_AND_WELFARE_SERVICES") ~ "Human_Welfare_Services",
                            FACSUBGRP %in% c("K12_EDUCATION_SERVICES",
                                             "DAY_CARE_AND_PREKINDERGARTEN",
                                             "COLLEGES OR UNIVERSITIES",
                                             "PROPRIETARY SCHOOLS") ~ "Education"
  )
  ) %>% 
  mutate(FACSUBGRP = str_replace_all(FACSUBGRP, ",", "")) %>% # remove commas and blank spaces to prevent errors when parsing string data
  mutate(FACSUBGRP = str_replace_all(FACSUBGRP, " ", "_"))

## the lines below will write a new column in the dataset indicating the distance to the closest flooding 
## the new column called "m_d_f" refers to exposure under the moderate scenario, while "e_d_f" does for the extreme scenario

facilities_data_points <- write_closest_flooding(facilities_data_points, 
                                                 moderate_flooding,
                                                 "m_d_f")

facilities_data_points <- write_closest_flooding(facilities_data_points, 
                                                 extreme_flooding,
                                                 "e_d_f")

st_write(facilities_data_points, "data/2_intermediate/facilities_20210811_flooding.shp", delete_dsn = TRUE)
rm(facilities_data_points)
