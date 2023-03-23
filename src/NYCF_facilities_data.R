# source(".src/NYCF_housekeeping_GIS_vars.R")

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
  mutate(FACSUBGRP = str_replace_all(FACSUBGRP, ",", "")) %>% 
  mutate(FACSUBGRP = str_replace_all(FACSUBGRP, " ", "_"))


facilities_data_points <- write_closest_flooding(facilities_data_points, 
                                                 moderate_flooding,
                                                 "m_d_f",
                                                 centroids = FALSE)

facilities_data_points <- write_closest_flooding(facilities_data_points, 
                                                 extreme_flooding,
                                                 "e_d_f",
                                                 centroids = FALSE)

st_write(facilities_data_points, "data/2_intermediate/facilities_20210811_flooding.shp", delete_dsn = TRUE)
rm(facilities_data_points)
