# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL GENERATE MAPS THAT SHOW THE DISTRIBUTION OF RISK ATTRIBUTES, CATEGORIES, AND INDIVIDUAL INDICATORS ACROSS NYC'S CDs

## we begin loading the libraries, functions, and variables that we will recurrently use

source("src/NYCF_housekeeping_GIS_vars.R")

CD <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd) %>%
  filter(boro_cd %nin% c(164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

## we load the dataset that was saved in the script "NYCF_Index_Build.R"
## this is an extended risk database with added hotspot scores at theindicator level (quintile ranking), risk category level, and risk attribute

final_table <- read_csv("data/3_output/stormwater_analysis_final_database_impact_indices.csv") %>% 
  mutate(Geography = as.numeric(Geography))

final_table_sf <- inner_join(CD, final_table, by=c("boro_cd" ="Geography"))

## HOTSPOTS PER RISK ATTRIBUTE (population exposure; critical infrastructure exposure, and social vulnerability) - for Moderate and Extreme scenarios
## the result will be 3 risk attributes x 2 flood hazard scenarios = 6 maps that will be in the manuscript's main text.
## each map shows the risk hotspots at the CD level for each risk attribute, under a given scenario

### POPULATION HOTSPOTS (Risk Attribute 1: Population Exposure)

### moderate 

ggplot() +
  geom_sf(data = final_table_sf, aes_string(fill = "M.HS.Agg_population")) +
  geom_sf_label(data = final_table_sf, 
                aes(label = round(M.HS.Agg_population,1)), 
                fill="white", 
                color="black", label.size  = NA, alpha = 0.7,
                label.padding = unit(0.1, "lines",),
                fun.geometry = sf::st_centroid) +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 25),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(1, "cm")) + 
  labs(title = "A)") + 
  scale_fill_gradientn(colours = c("#F3935F", "#E54787", "#4B1D91"), 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       name= "",
                       limits = c(0,1))
  
ggsave(filename = "data/4_display/maps/M.HS.Agg_population.png", 
       dpi = 400,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

### extreme 

ggplot() +
  geom_sf(data = final_table_sf, aes_string(fill = "E.HS.Agg_population")) +
  geom_sf_label(data = final_table_sf, 
                aes(label = round(E.HS.Agg_population,1)), 
                fill="white", 
                color="black", label.size  = NA, alpha = 0.7,
                label.padding = unit(0.1, "lines",),
                fun.geometry = sf::st_centroid) +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 25),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(1, "cm")) + 
  labs(title = "A)") +
  scale_fill_gradientn(colours = c("#F3935F", "#E54787", "#4B1D91"), 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       name= "",
                       limits = c(0,1))

ggsave(filename = "data/4_display/maps/E.HS.Agg_population.png", 
       dpi = 400,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

### CRITICAL INFRASTRUCTURES AND SERVICES HOTSPOTS (Risk Attribute 2)

ggplot() +
  geom_sf(data = final_table_sf, aes_string(fill = "M.Infra_hotspots_sum_n")) +
  geom_sf_label(data = final_table_sf, 
                aes(label = round(M.Infra_hotspots_sum_n,1)), 
                fill="white", 
                color="black", label.size  = NA, alpha = 0.7,
                label.padding = unit(0.1, "lines",),
                fun.geometry = sf::st_centroid) +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 25),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(1, "cm")) + 
  labs(title = "B)") +
  scale_fill_gradientn(colours = c("#EDEF5C", "#17A77E", "#255668"), 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       name= "",
                       limits = c(0,1))

ggsave(filename = "data/4_display/maps/M.Infra_hotspots_sum_n.png", 
       dpi = 400,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

ggplot() +
  geom_sf(data = final_table_sf, aes_string(fill = "E.Infra_hotspots_sum_n")) +
  geom_sf_label(data = final_table_sf, 
                aes(label = round(E.Infra_hotspots_sum_n,1)), 
                fill="white", 
                color="black", label.size  = NA, alpha = 0.7,
                label.padding = unit(0.1, "lines",),
                fun.geometry = sf::st_centroid) +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 25),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(1, "cm")) + 
  labs(title = "B)") +
  scale_fill_gradientn(colours = c("#EDEF5C", "#17A77E", "#255668"), 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       name= "",
                       limits = c(0,1))

ggsave(filename = "data/4_display/maps/E.Infra_hotspots_sum_n.png", 
       dpi = 400,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

### SOCIAL VULNERABILITY HOTSPOTS (Risk Attribute 3)

ggplot() +
  geom_sf(data = final_table_sf, aes_string(fill = "M.HS.Agg_sovi")) +
  geom_sf_label(data = final_table_sf, 
                aes(label = round(M.HS.Agg_sovi,1)), 
                fill="white", 
                color="black", label.size  = NA, alpha = 0.7,
                label.padding = unit(0.1, "lines",),
                fun.geometry = sf::st_centroid) +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 25),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(1, "cm")) + 
  labs(title = "C)") +
  scale_fill_gradientn(colours = sequential_hcl(5, "Lajolla"), 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       name= "",
                       limits = c(0,1))

ggsave(filename = "data/4_display/maps/M.HS.Agg_sovi.png", 
       dpi = 400,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

ggplot() +
  geom_sf(data = final_table_sf, aes_string(fill = "E.HS.Agg_sovi")) +
  geom_sf_label(data = final_table_sf, 
                aes(label = round(E.HS.Agg_sovi,1)), 
                fill="white", 
                color="black", label.size  = NA, alpha = 0.7,
                label.padding = unit(0.1, "lines",),
                fun.geometry = sf::st_centroid) +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 25),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(1, "cm")) + 
  labs(title = "C)") +
  scale_fill_gradientn(colours = sequential_hcl(5, "Lajolla"), 
                       breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       labels = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                       name= "",
                       limits = c(0,1))

ggsave(filename = "data/4_display/maps/E.HS.Agg_sovi.png", 
       dpi = 400,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

## besides the 3 risk attributes, we want to generate maps for each risk category and individual risk indicators
## these maps will be provided as supplementary materials in the form of panels. 
## Each panel will show the mapped quintile ranks of individual indicators under each risk category

##we can start setting up handy legends that we will use consistently across the maps
Map.Legend_LUSE <- mapper_legender_frame(data_df = final_table_sf, fieldname = "M.PCT_industrial_lots", legend_position = "top", color_scheme = "greens")
Map.Legend_LEFT <- mapper_legender_frame(data_df = final_table_sf, fieldname = "M.PCT_road_area", legend_position = c(0.1,0.5), key_size_cm = 3, text_title_size = 50, text_size = 40, color_scheme = "greens")


## Starting with the MODERATE scenario

## Risk Attribute 1: Population Exposure 

M.Map.P_population <- mapper_function_quintile(final_table_sf, "M.PCT_Total_population", "% Population Impacted", "none", color_scheme = "purples")

## Risk Attribute 2: Critical Infrastructures and Services

## Risk Category 2.1: land use

M.Map.P_Res_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_residential_lots", "% Residential Lots Impacted", "none", color_scheme = "greens")

M.Map.P_ResBsmnt_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_basement_residential_lots", "% Residential Lots with a \n Basement Impacted", "none", color_scheme = "greens")

M.Map.P_Mixed_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_mixed_comres_lots", "% Mixed Residential/Commercial \n Lots Impacted", "none", color_scheme = "greens")

M.Map.P_Commercial_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_commercial_lots", "% Commercial Lots Impacted", "none", color_scheme = "greens")

M.Map.P_Industrial_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_industrial_lots", "% Industrial Lots Impacted", "none", color_scheme = "greens")

frame <- ggarrange(M.Map.P_Res_Flooded,
                   M.Map.P_ResBsmnt_Flooded, 
                   M.Map.P_Mixed_Flooded,
                   M.Map.P_Commercial_Flooded,
                   M.Map.P_Industrial_Flooded,
                   Map.Legend_LEFT,
                   nrow = 3, ncol = 2, 
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Land Use Exposure Indicators Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/moderate/maps_LU_exposure.png", 
                   width = 1800, 
                   height = 2400) 

## Risk Category 2.2: transportation

Map.P_Road_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_road_area", "% Road Area Flooded", "none", color_scheme = "greens")
Map.P_Bus_Stops_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_bus_stops", "% Bus Stops Impacted", "none", color_scheme = "greens")
Map.P_Bus_Routes_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_bus_routes", "% Bus Routes Impacted", "none", color_scheme = "greens")
Map.P_Subway_Entrances_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_subway_stops", "% Subway Entrances Impacted", "none", color_scheme = "greens")


frame <- ggarrange(Map.P_Road_Flooded,
                   Map.P_Bus_Stops_Flooded, 
                   Map.P_Bus_Routes_Flooded,
                   Map.P_Subway_Entrances_Flooded,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2, 
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Transportation Exposure Indicators Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/moderate/maps_Transport_exposure.png", 
                   width = 2200, 
                   height = 3200) 

## Risk Category 2.3: public safety

Map.P_POLICE <- mapper_function_quintile(final_table_sf, "M.PCT_POLICE_SERVICES_facilities", "% Police Services", "none", color_scheme = "greens")
Map.P_FIRE <- mapper_function_quintile(final_table_sf, "M.PCT_FIRE_SERVICES_facilities", "% Fire Services", "none", color_scheme = "greens")
Map.P_OTHER_EMERGENCY <- mapper_function_quintile(final_table_sf, "M.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities", "% Other Safety And Emergency \nmanagement Services", "none", color_scheme = "greens")


frame <- ggarrange(Map.P_POLICE, 
                   Map.P_FIRE,
                   Map.P_OTHER_EMERGENCY,
                   Map.Legend_LEFT,
                   nrow = 2, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Public Safety Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/moderate/maps_PS.png", 
                   width = 1800, 
                   height = 2700) 

## Risk Category 2.4: healthcare

Map.P_HospitalsClinics <- mapper_function_quintile(final_table_sf, "M.PCT_HOSPITALS_AND_CLINICS_facilities", "% Hospitals and Clinics", "none", color_scheme = "greens")
Map.P_ResidentialHCare <- mapper_function_quintile(final_table_sf, "M.PCT_RESIDENTIAL_HEALTH_CARE_facilities", "% Residential Healtchare Facilities", "none", color_scheme = "greens")
Map.P_MentalHealth <- mapper_function_quintile(final_table_sf, "M.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities", "% Mental Health and \nChemical Dependency Facilities", "none", color_scheme = "greens")
Map.P_OtherHealthCare <- mapper_function_quintile(final_table_sf, "M.PCT_OTHER_HEALTH_CARE_facilities", "% Other Healthcare Facilities", "none", color_scheme = "greens")


frame <- ggarrange(Map.P_HospitalsClinics, 
                   Map.P_ResidentialHCare,
                   Map.P_MentalHealth,
                   Map.P_OtherHealthCare,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Healthcare Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/moderate/maps_HC.png", 
                   width = 1800, 
                   height = 3600) 

## Risk Category 2.5: welfare services

Map.P_HumanDev <- mapper_function_quintile(final_table_sf, "M.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities", "% Human Development Facilities", "none", color_scheme = "greens")
Map.P_Senior <- mapper_function_quintile(final_table_sf, "M.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities", "% Senior and Disability Services", "none", color_scheme = "greens")
Map.P_HousingFood <- mapper_function_quintile(final_table_sf, "M.PCT_HOUSING_AND_FOOD_SERVICES_facilities", "% Housing and Food Services", "none", color_scheme = "greens")
Map.P_ChildWelfare <- mapper_function_quintile(final_table_sf, "M.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities", "% Child Services \nand Welfare Facilities", "none", color_scheme = "greens")



frame <- ggarrange(Map.P_HumanDev, 
                   Map.P_Senior,
                   Map.P_HousingFood,
                   Map.P_ChildWelfare,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Welfare Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/moderate/maps_WF.png", 
                   width = 1800, 
                   height = 2700)

## Risk Category 2.6: education

Map.P_K12 <- mapper_function_quintile(final_table_sf, "M.PCT_K12_EDUCATION_SERVICES_facilities", "% K-12 Education Facilities", "none", color_scheme = "greens")
Map.P_DayCare <- mapper_function_quintile(final_table_sf, "M.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities", "% Pre-K / Day Care \nEducation Facilities", "none", color_scheme = "greens")
Map.P_Higher <- mapper_function_quintile(final_table_sf, "M.PCT_COLLEGES_OR_UNIVERSITIES_facilities", "% Higher Education Facilities", "none", color_scheme = "greens")
Map.P_Proprietary <- mapper_function_quintile(final_table_sf, "M.PCT_PROPRIETARY_SCHOOLS_facilities", "% Proprietary Education Facilities", "none", color_scheme = "greens")


frame <- ggarrange(Map.P_K12, 
                   Map.P_DayCare,
                   Map.P_Higher,
                   Map.P_Proprietary,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Education Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/moderate/maps_ED.png", 
                   width = 1800, 
                   height = 2700)

## And we can generate a panel with the aggregated hotspot scores for each risk category that is part of risk attribute 3

M.AGGREGATE_LUSE <- mapper_function_equal_breaks(final_table_sf, fieldname = "M.HS.Agg_landuse", map_title = "Land Use \nHotspot Score", legend_position = "none", n_breaks = 5, color_scheme = "greens")
M.AGGREGATE_TRANSPORT <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_transport", "Transport \nHotspot Score", "right", color_scheme = "greens")
M.AGGREGATE_EDUCATION <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_education", "Education Facilities \nIndex", "right", color_scheme = "greens")
M.AGGREGATE_HEALTHCARE <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_healthcare", "Healthcare Facilities \nHotspot Score", "right", color_scheme = "greens")
M.AGGREGATE_WELFARE <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_welfare", "Welfare Facilities \nHotspot Score", "right", color_scheme = "greens")
M.AGGREGATE_PSAFETY <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_psafety", "Public Safety Facilities \nHotspot Score", "right", color_scheme = "greens")


frame <- ggarrange(M.AGGREGATE_LUSE,
                   M.AGGREGATE_TRANSPORT,
                   M.AGGREGATE_EDUCATION,
                   M.AGGREGATE_HEALTHCARE,
                   M.AGGREGATE_WELFARE,
                   M.AGGREGATE_PSAFETY,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Hotspot Scores Per Infrastructure Risk Category Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 70))

frame %>% ggexport(filename = "data/4_display/maps/moderate/infra_maps_ALL_HOTSPOTS.png", 
                   width = 2800, 
                   height = 3600)

### Risk Attribute 3: Social Vulnerability

Map.P_bel_5 <- mapper_function_quintile(final_table_sf, "M.PCT_below5_population", "% Below 5 Years Old", "none", color_scheme = "browns")
Map.P_above_65 <- mapper_function_quintile(final_table_sf, "M.PCT_above65_population", "% Above 65 Years Old", "none", color_scheme = "browns")
Map.P_BIPOC <- mapper_function_quintile(final_table_sf, "M.PCT_bipoc_population", "% BIPOC", "none", color_scheme = "browns")
Map.P_below_poverty <- mapper_function_quintile(final_table_sf, "M.PCT_Belpov_e", "% Below the Poverty Line", "none", color_scheme = "browns")
Map.P_disability <- mapper_function_quintile(final_table_sf, "M.PCT_Disability_e", "% With a Disability", "none", color_scheme = "browns")
Map.P_Income75K <- mapper_function_quintile(final_table_sf, "M.PCT_HH_Income_Bel75K_e", "% Household Income Below $75K", "none", color_scheme = "browns")
Map.P_cost_burdened <- mapper_function_quintile(final_table_sf, "M.PCT_CB_e", "% Cost-Burdened Households", "none", color_scheme = "browns")
Map.P_rent_burdened <- mapper_function_quintile(final_table_sf, "M.PCT_RB_e", "% Rent-Burdened Households", "none", color_scheme = "browns")

frame <- ggarrange(Map.P_BIPOC, 
                   Map.P_bel_5,
                   Map.P_above_65,
                   Map.P_below_poverty,
                   Map.P_disability,
                   Map.P_Income75K,
                   Map.P_cost_burdened,
                   Map.P_rent_burdened,
                   mapper_legender_frame(data_df = final_table_sf, fieldname = "M.PCT_industrial_lots", legend_position = "top", color_scheme = "browns"),
                   nrow = 5, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Social Vulnerability Indicators Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/moderate/maps_SV.png", 
                   width = 1800, 
                   height = 3600) 

### We repeat the operation with the EXTREME scenario, replacing the prefix "M." for "E." in every call for indicators 

## Risk Attribute 1: Population Exposure 

E.Map.P_population <- mapper_function_quintile(final_table_sf, "E.PCT_Total_population", "% Population Impacted", "none", color_scheme = "purples")

## Risk Attribute 2: Critical Infrastructures and Services

## Risk Category 2.1: land use

E.Map.P_Res_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_residential_lots", "% Residential Lots Impacted", "none", color_scheme = "greens")

E.Map.P_ResBsmnt_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_basement_residential_lots", "% Residential Lots with a \n Basement Impacted", "none", color_scheme = "greens")

E.Map.P_Mixed_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_mixed_comres_lots", "% Mixed Residential/Commercial \n Lots Impacted", "none", color_scheme = "greens")

E.Map.P_Commercial_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_commercial_lots", "% Commercial Lots Impacted", "none", color_scheme = "greens")

E.Map.P_Industrial_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_industrial_lots", "% Industrial Lots Impacted", "none", color_scheme = "greens")

frame <- ggarrange(E.Map.P_Res_Flooded,
                   E.Map.P_ResBsmnt_Flooded, 
                   E.Map.P_Mixed_Flooded,
                   E.Map.P_Commercial_Flooded,
                   E.Map.P_Industrial_Flooded,
                   Map.Legend_LEFT,
                   nrow = 3, ncol = 2, 
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Land Use Exposure Indicators Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/extreme/maps_LU_exposure.png", 
                   width = 1800, 
                   height = 2400) 

## Risk Category 2.2: transportation

Map.P_Road_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_road_area", "% Road Area Flooded", "none", color_scheme = "greens")
Map.P_Bus_Stops_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_bus_stops", "% Bus Stops Impacted", "none", color_scheme = "greens")
Map.P_Bus_Routes_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_bus_routes", "% Bus Routes Impacted", "none", color_scheme = "greens")
Map.P_Subway_Entrances_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_subway_stops", "% Subway Entrances Impacted", "none", color_scheme = "greens")


frame <- ggarrange(Map.P_Road_Flooded,
                   Map.P_Bus_Stops_Flooded, 
                   Map.P_Bus_Routes_Flooded,
                   Map.P_Subway_Entrances_Flooded,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2, 
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Transportation Exposure Indicators Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/extreme/maps_Transport_exposure.png", 
                   width = 2200, 
                   height = 3200) 

## Risk Category 2.3: public safety

Map.P_POLICE <- mapper_function_quintile(final_table_sf, "E.PCT_POLICE_SERVICES_facilities", "% Police Services", "none", color_scheme = "greens")
Map.P_FIRE <- mapper_function_quintile(final_table_sf, "E.PCT_FIRE_SERVICES_facilities", "% Fire Services", "none", color_scheme = "greens")
Map.P_OTHER_EMERGENCY <- mapper_function_quintile(final_table_sf, "E.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities", "% Other Safety And Emergency \nmanagement Services", "none", color_scheme = "greens")


frame <- ggarrange(Map.P_POLICE, 
                   Map.P_FIRE,
                   Map.P_OTHER_EMERGENCY,
                   Map.Legend_LEFT,
                   nrow = 2, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Public Safety Facilities Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/extreme/maps_PS.png", 
                   width = 1800, 
                   height = 2700) 

## Risk Category 2.4: healthcare

Map.P_HospitalsClinics <- mapper_function_quintile(final_table_sf, "E.PCT_HOSPITALS_AND_CLINICS_facilities", "% Hospitals and Clinics", "none", color_scheme = "greens")
Map.P_ResidentialHCare <- mapper_function_quintile(final_table_sf, "E.PCT_RESIDENTIAL_HEALTH_CARE_facilities", "% Residential Healtchare Facilities", "none", color_scheme = "greens")
Map.P_MentalHealth <- mapper_function_quintile(final_table_sf, "E.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities", "% Mental Health and \nChemical Dependency Facilities", "none", color_scheme = "greens")
Map.P_OtherHealthCare <- mapper_function_quintile(final_table_sf, "E.PCT_OTHER_HEALTH_CARE_facilities", "% Other Healthcare Facilities", "none", color_scheme = "greens")


frame <- ggarrange(Map.P_HospitalsClinics, 
                   Map.P_ResidentialHCare,
                   Map.P_MentalHealth,
                   Map.P_OtherHealthCare,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Healthcare Facilities Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/extreme/maps_HC.png", 
                   width = 1800, 
                   height = 3600) 

## Risk Category 2.5: welfare services

Map.P_HumanDev <- mapper_function_quintile(final_table_sf, "E.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities", "% Human Development Facilities", "none", color_scheme = "greens")
Map.P_Senior <- mapper_function_quintile(final_table_sf, "E.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities", "% Senior and Disability Services", "none", color_scheme = "greens")
Map.P_HousingFood <- mapper_function_quintile(final_table_sf, "E.PCT_HOUSING_AND_FOOD_SERVICES_facilities", "% Housing and Food Services", "none", color_scheme = "greens")
Map.P_ChildWelfare <- mapper_function_quintile(final_table_sf, "E.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities", "% Child Services \nand Welfare Facilities", "none", color_scheme = "greens")

frame <- ggarrange(Map.P_HumanDev, 
                   Map.P_Senior,
                   Map.P_HousingFood,
                   Map.P_ChildWelfare,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Welfare Facilities Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/extreme/maps_WF.png", 
                   width = 1800, 
                   height = 2700)

## Risk Category 2.6: education

Map.P_K12 <- mapper_function_quintile(final_table_sf, "E.PCT_K12_EDUCATION_SERVICES_facilities", "% K-12 Education Facilities", "none", color_scheme = "greens")
Map.P_DayCare <- mapper_function_quintile(final_table_sf, "E.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities", "% Pre-K / Day Care \nEducation Facilities", "none", color_scheme = "greens")
Map.P_Higher <- mapper_function_quintile(final_table_sf, "E.PCT_COLLEGES_OR_UNIVERSITIES_facilities", "% Higher Education Facilities", "none", color_scheme = "greens")
Map.P_Proprietary <- mapper_function_quintile(final_table_sf, "E.PCT_PROPRIETARY_SCHOOLS_facilities", "% Proprietary Education Facilities", "none", color_scheme = "greens")

frame <- ggarrange(Map.P_K12, 
                   Map.P_DayCare,
                   Map.P_Higher,
                   Map.P_Proprietary,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Education Facilities Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/extreme/maps_ED.png", 
                   width = 1800, 
                   height = 2700)

## And we can generate a panel with the aggregated hotspot scores for each risk category that is part of risk attribute 3

E.AGGREGATE_LUSE <- mapper_function_equal_breaks(final_table_sf, fieldname = "E.HS.Agg_landuse", map_title = "Land Use \nHotspot Score", legend_position = "none", n_breaks = 5, color_scheme = "greens")
E.AGGREGATE_TRANSPORT <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_transport", "Transport \nHotspot Score", "right", color_scheme = "greens")
E.AGGREGATE_EDUCATION <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_education", "Education Facilities \nIndex", "right", color_scheme = "greens")
E.AGGREGATE_HEALTHCARE <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_healthcare", "Healthcare Facilities \nHotspot Score", "right", color_scheme = "greens")
E.AGGREGATE_WELFARE <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_welfare", "Welfare Facilities \nHotspot Score", "right", color_scheme = "greens")
E.AGGREGATE_PSAFETY <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_psafety", "Public Safety Facilities \nHotspot Score", "right", color_scheme = "greens")

frame <- ggarrange(E.AGGREGATE_LUSE,
                   E.AGGREGATE_TRANSPORT,
                   E.AGGREGATE_EDUCATION,
                   E.AGGREGATE_HEALTHCARE,
                   E.AGGREGATE_WELFARE,
                   E.AGGREGATE_PSAFETY,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Hotspot Scores Per Infrastructure Risk Category Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 70))

frame %>% ggexport(filename = "data/4_display/maps/extreme/infra_maps_ALL_HOTSPOTS.png", 
                   width = 2800, 
                   height = 3600)

### Risk Attribute 3: Social Vulnerability

Map.P_bel_5 <- mapper_function_quintile(final_table_sf, "E.PCT_below5_population", "% Below 5 Years Old", "none", color_scheme = "browns")
Map.P_above_65 <- mapper_function_quintile(final_table_sf, "E.PCT_above65_population", "% Above 65 Years Old", "none", color_scheme = "browns")
Map.P_BIPOC <- mapper_function_quintile(final_table_sf, "E.PCT_bipoc_population", "% BIPOC", "none", color_scheme = "browns")
Map.P_below_poverty <- mapper_function_quintile(final_table_sf, "E.PCT_Belpov_e", "% Below the Poverty Line", "none", color_scheme = "browns")
Map.P_disability <- mapper_function_quintile(final_table_sf, "E.PCT_Disability_e", "% With a Disability", "none", color_scheme = "browns")
Map.P_Income75K <- mapper_function_quintile(final_table_sf, "E.PCT_HH_Income_Bel75K_e", "% Household Income Below $75K", "none", color_scheme = "browns")
Map.P_cost_burdened <- mapper_function_quintile(final_table_sf, "E.PCT_CB_e", "% Cost-Burdened Households", "none", color_scheme = "browns")
Map.P_rent_burdened <- mapper_function_quintile(final_table_sf, "E.PCT_RB_e", "% Rent-Burdened Households", "none", color_scheme = "browns")

frame <- ggarrange(Map.P_BIPOC, 
                   Map.P_bel_5,
                   Map.P_above_65,
                   Map.P_below_poverty,
                   Map.P_disability,
                   Map.P_Income75K,
                   Map.P_cost_burdened,
                   Map.P_rent_burdened,
                   mapper_legender_frame(data_df = final_table_sf, fieldname = "E.PCT_industrial_lots", legend_position = "top", color_scheme = "browns"),
                   nrow = 5, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Social Vulnerability Indicators Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/extreme/maps_SV.png", 
                   width = 1800, 
                   height = 3600)