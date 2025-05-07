source("src/NYCF_housekeeping_GIS_vars.R")

CD <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd) %>%
  filter(boro_cd %nin% c(164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

final_table <- read_csv("DATA/3_output/stormwater_analysis_final_database_impact_indices.csv") %>%
  mutate(Geography = as.numeric(Geography))

final_table_sf <- inner_join(CD, final_table, by=c("boro_cd" ="Geography"))

### HOTSPOTS PER CATEGORY - M and E scenarios

### POPULATION HOTSPOTS

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

### INFRA HOTSPOTS

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

### SOVI HOTSPOTS

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




### CD level MAPS per categories 

Map.Legend_LUSE <- mapper_legender_frame(data_df = final_table_sf, fieldname = "M.PCT_industrial_lots", legend_position = "top", color_scheme = "greens")
Map.Legend_LEFT <- mapper_legender_frame(data_df = final_table_sf, fieldname = "M.PCT_road_area", legend_position = c(0.1,0.5), key_size_cm = 3, text_title_size = 50, text_size = 40, color_scheme = "greens")

## Population & LAND USE 
M.Map.P_population <- mapper_function_quintile(final_table_sf, "M.PCT_Total_population", "% Population Impacted", "none", color_scheme = "purples")

M.Map.P_Res_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_residential_lots", "% Residential Lots Impacted", "none", color_scheme = "greens")

M.Map.P_ResBsmnt_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_basement_residential_lots", "% Residential Lots with a \n Basement Impacted", "none", color_scheme = "greens")

M.Map.P_Mixed_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_mixed_comres_lots", "% Mixed Residential/Commercial \n Lots Impacted", "none", color_scheme = "greens")

M.Map.P_Commercial_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_commercial_lots", "% Commercial Lots Impacted", "none", color_scheme = "greens")

M.Map.P_Industrial_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_industrial_lots", "% Industrial Lots Impacted", "none", color_scheme = "greens")

M.AGGREGATE_LUSE <- mapper_function_equal_breaks(final_table_sf, fieldname = "M.HS.Agg_landuse", map_title = "Land Use \nHotspot Score", legend_position = "none", n_breaks = 5, color_scheme = "greens")
M.AGGREGATE_POP <- mapper_function_equal_breaks(final_table_sf, fieldname = "M.HS.Agg_population", map_title = "Population \nHotspot Score", legend_position = c(0.99,0.2), n_breaks = 5, color_scheme = "purples")

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

## Transport

Map.P_Road_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_road_area", "% Road Area Flooded", "none", color_scheme = "greens")
Map.P_Bus_Stops_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_bus_stops", "% Bus Stops Impacted", "none", color_scheme = "greens")
Map.P_Bus_Routes_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_bus_routes", "% Bus Routes Impacted", "none", color_scheme = "greens")
Map.P_Subway_Entrances_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_subway_stops", "% Subway Entrances Impacted", "none", color_scheme = "greens")

M.AGGREGATE_TRANSPORT <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_transport", "Transport \nHotspot Score", "right", color_scheme = "greens")

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

## SOVI
Map.P_bel_5 <- mapper_function_quintile(final_table_sf, "M.PCT_below5_population", "% Below 5 Years Old", "none", color_scheme = "browns")
Map.P_above_65 <- mapper_function_quintile(final_table_sf, "M.PCT_above65_population", "% Above 65 Years Old", "none", color_scheme = "browns")
Map.P_BIPOC <- mapper_function_quintile(final_table_sf, "M.PCT_bipoc_population", "% BIPOC", "none", color_scheme = "browns")
Map.P_below_poverty <- mapper_function_quintile(final_table_sf, "M.PCT_Belpov_e", "% Below the Poverty Line", "none", color_scheme = "browns")
Map.P_disability <- mapper_function_quintile(final_table_sf, "M.PCT_Disability_e", "% With a Disability", "none", color_scheme = "browns")
Map.P_Income75K <- mapper_function_quintile(final_table_sf, "M.PCT_HH_Income_Bel75K_e", "% Household Income Below $75K", "none", color_scheme = "browns")
Map.P_cost_burdened <- mapper_function_quintile(final_table_sf, "M.PCT_CB_e", "% Cost-Burdened Households", "none", color_scheme = "browns")
Map.P_rent_burdened <- mapper_function_quintile(final_table_sf, "M.PCT_RB_e", "% Rent-Burdened Households", "none", color_scheme = "browns")


M.AGGREGATE_SOVI <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_sovi", "Social Vulnerability \nHotspot Score", "right", color_scheme = "browns")


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

### PUBLIC SAFETY

Map.P_POLICE <- mapper_function_quintile(final_table_sf, "M.PCT_POLICE_SERVICES_facilities", "% Police Services", "none", color_scheme = "greens")
Map.P_FIRE <- mapper_function_quintile(final_table_sf, "M.PCT_FIRE_SERVICES_facilities", "% Fire Services", "none", color_scheme = "greens")
Map.P_OTHER_EMERGENCY <- mapper_function_quintile(final_table_sf, "M.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities", "% Other Safety And Emergency \nmanagement Services", "none", color_scheme = "greens")

M.AGGREGATE_PSAFETY <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_psafety", "Public Safety Facilities \nHotspot Score", "right", color_scheme = "greens")

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

### HEALTHCARE

Map.P_HospitalsClinics <- mapper_function_quintile(final_table_sf, "M.PCT_HOSPITALS_AND_CLINICS_facilities", "% Hospitals and Clinics", "none", color_scheme = "greens")
Map.P_ResidentialHCare <- mapper_function_quintile(final_table_sf, "M.PCT_RESIDENTIAL_HEALTH_CARE_facilities", "% Residential Healtchare Facilities", "none", color_scheme = "greens")
Map.P_MentalHealth <- mapper_function_quintile(final_table_sf, "M.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities", "% Mental Health and \nChemical Dependency Facilities", "none", color_scheme = "greens")
Map.P_OtherHealthCare <- mapper_function_quintile(final_table_sf, "M.PCT_OTHER_HEALTH_CARE_facilities", "% Other Healthcare Facilities", "none", color_scheme = "greens")



M.AGGREGATE_HEALTHCARE <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_healthcare", "Healthcare Facilities \nHotspot Score", "right", color_scheme = "greens")

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

### WELFARE

Map.P_HumanDev <- mapper_function_quintile(final_table_sf, "M.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities", "% Human Development Facilities", "none", color_scheme = "greens")
Map.P_Senior <- mapper_function_quintile(final_table_sf, "M.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities", "% Senior and Disability Services", "none", color_scheme = "greens")
Map.P_HousingFood <- mapper_function_quintile(final_table_sf, "M.PCT_HOUSING_AND_FOOD_SERVICES_facilities", "% Housing and Food Services", "none", color_scheme = "greens")
Map.P_ChildWelfare <- mapper_function_quintile(final_table_sf, "M.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities", "% Child Services \nand Welfare Facilities", "none", color_scheme = "greens")


M.AGGREGATE_WELFARE <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_welfare", "Welfare Facilities \nHotspot Score", "right", color_scheme = "greens")

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

### EDUCATION

Map.P_K12 <- mapper_function_quintile(final_table_sf, "M.PCT_K12_EDUCATION_SERVICES_facilities", "% K-12 Education Facilities", "none", color_scheme = "greens")
Map.P_DayCare <- mapper_function_quintile(final_table_sf, "M.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities", "% Pre-K / Day Care \nEducation Facilities", "none", color_scheme = "greens")
Map.P_Higher <- mapper_function_quintile(final_table_sf, "M.PCT_COLLEGES_OR_UNIVERSITIES_facilities", "% Higher Education Facilities", "none", color_scheme = "greens")
Map.P_Proprietary <- mapper_function_quintile(final_table_sf, "M.PCT_PROPRIETARY_SCHOOLS_facilities", "% Proprietary Education Facilities", "none", color_scheme = "greens")

M.AGGREGATE_EDUCATION <- mapper_function_equal_breaks(final_table_sf, "M.HS.Agg_education", "Education Facilities \nIndex", "right", color_scheme = "greens")

frame <- ggarrange(Map.P_K12, 
                   Map.P_DayCare,
                   Map.P_Higher,
                   Map.P_Proprietary,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Welfare Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/moderate/maps_ED.png", 
                   width = 1800, 
                   height = 2700)

final_table_sf$infra_hotspots_sum <- round(final_table_sf$M.HS.Agg_landuse +
                                             final_table_sf$M.HS.Agg_transport +
                                             final_table_sf$M.HS.Agg_education +
                                             final_table_sf$M.HS.Agg_welfare +
                                             final_table_sf$M.HS.Agg_psafety +
                                             final_table_sf$M.HS.Agg_healthcare,2)

final_table_sf$infra_hotspots_sum_n <- normalize(final_table_sf$infra_hotspots_sum) %>%
  round(2)

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

### LETS DO IT WITH THE EXTREME SCENARIO NOW

## Population & LAND USE 
E.Map.P_population <- mapper_function_quintile(final_table_sf, "E.PCT_Total_population", "% Population Impacted", "none", color_scheme = "purples")

E.Map.P_Res_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_residential_lots", "% Residential Lots Impacted", "none", color_scheme = "greens")

E.Map.P_ResBsmnt_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_basement_residential_lots", "% Residential Lots with a \n Basement Impacted", "none", color_scheme = "greens")

E.Map.P_Mixed_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_mixed_comres_lots", "% Mixed Residential/Commercial \n Lots Impacted", "none", color_scheme = "greens")

E.Map.P_Commercial_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_commercial_lots", "% Commercial Lots Impacted", "none", color_scheme = "greens")

E.Map.P_Industrial_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_industrial_lots", "% Industrial Lots Impacted", "none", color_scheme = "greens")

E.AGGREGATE_LUSE <- mapper_function_equal_breaks(final_table_sf, fieldname = "E.HS.Agg_landuse", map_title = "Land Use \nHotspot Score", legend_position = "none", n_breaks = 5, color_scheme = "greens")
E.AGGREGATE_POP <- mapper_function_equal_breaks(final_table_sf, fieldname = "E.HS.Agg_population", map_title = "Population \nHotspot Score", legend_position = c(0.99,0.2), n_breaks = 5, color_scheme = "purples")

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

## Transport

Map.P_Road_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_road_area", "% Road Area Flooded", "none", color_scheme = "greens")
Map.P_Bus_Stops_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_bus_stops", "% Bus Stops Impacted", "none", color_scheme = "greens")
Map.P_Bus_Routes_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_bus_routes", "% Bus Routes Impacted", "none", color_scheme = "greens")
Map.P_Subway_Entrances_Flooded <- mapper_function_quintile(final_table_sf, "E.PCT_subway_stops", "% Subway Entrances Impacted", "none", color_scheme = "greens")

E.AGGREGATE_TRANSPORT <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_transport", "Transport \nHotspot Score", "right", color_scheme = "greens")

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

## SOVI
Map.P_bel_5 <- mapper_function_quintile(final_table_sf, "E.PCT_below5_population", "% Below 5 Years Old", "none", color_scheme = "browns")
Map.P_above_65 <- mapper_function_quintile(final_table_sf, "E.PCT_above65_population", "% Above 65 Years Old", "none", color_scheme = "browns")
Map.P_BIPOC <- mapper_function_quintile(final_table_sf, "E.PCT_bipoc_population", "% BIPOC", "none", color_scheme = "browns")
Map.P_below_poverty <- mapper_function_quintile(final_table_sf, "E.PCT_Belpov_e", "% Below the Poverty Line", "none", color_scheme = "browns")
Map.P_disability <- mapper_function_quintile(final_table_sf, "E.PCT_Disability_e", "% With a Disability", "none", color_scheme = "browns")
Map.P_Income75K <- mapper_function_quintile(final_table_sf, "E.PCT_HH_Income_Bel75K_e", "% Household Income Below $75K", "none", color_scheme = "browns")
Map.P_cost_burdened <- mapper_function_quintile(final_table_sf, "E.PCT_CB_e", "% Cost-Burdened Households", "none", color_scheme = "browns")
Map.P_rent_burdened <- mapper_function_quintile(final_table_sf, "E.PCT_RB_e", "% Rent-Burdened Households", "none", color_scheme = "browns")

E.AGGREGATE_SOVI <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_sovi", "Social Vulnerability \nHotspot Score", "right", color_scheme = "browns")

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

### PUBLIC SAFETY

Map.P_POLICE <- mapper_function_quintile(final_table_sf, "E.PCT_POLICE_SERVICES_facilities", "% Police Services", "none", color_scheme = "greens")
Map.P_FIRE <- mapper_function_quintile(final_table_sf, "E.PCT_FIRE_SERVICES_facilities", "% Fire Services", "none", color_scheme = "greens")
Map.P_OTHER_EMERGENCY <- mapper_function_quintile(final_table_sf, "E.PCT_OTHER_SAFETY_AND_EMERGENCY_FACILITIES_facilities", "% Other Safety And Emergency \nmanagement Services", "none", color_scheme = "greens")

E.AGGREGATE_PSAFETY <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_psafety", "Public Safety Facilities \nHotspot Score", "right", color_scheme = "greens")

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

### HEALTHCARE

Map.P_HospitalsClinics <- mapper_function_quintile(final_table_sf, "E.PCT_HOSPITALS_AND_CLINICS_facilities", "% Hospitals and Clinics", "none", color_scheme = "greens")
Map.P_ResidentialHCare <- mapper_function_quintile(final_table_sf, "E.PCT_RESIDENTIAL_HEALTH_CARE_facilities", "% Residential Healtchare Facilities", "none", color_scheme = "greens")
Map.P_MentalHealth <- mapper_function_quintile(final_table_sf, "E.PCT_MENTAL_HEALTH_HEALTH_PROMOTION_AND_CHEMICAL_DEPENDENCY_SERVICES_facilities", "% Mental Health and \nChemical Dependency Facilities", "none", color_scheme = "greens")
Map.P_OtherHealthCare <- mapper_function_quintile(final_table_sf, "E.PCT_OTHER_HEALTH_CARE_facilities", "% Other Healthcare Facilities", "none", color_scheme = "greens")



E.AGGREGATE_HEALTHCARE <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_healthcare", "Healthcare Facilities \nHotspot Score", "right", color_scheme = "greens")

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

### WELFARE

Map.P_HumanDev <- mapper_function_quintile(final_table_sf, "E.PCT_HUMAN_DEVELOPMENT_SERVICES_facilities", "% Human Development Facilities", "none", color_scheme = "greens")
Map.P_Senior <- mapper_function_quintile(final_table_sf, "E.PCT_SENIOR_AND_DISSABILITY_SERVICES_facilities", "% Senior and Disability Services", "none", color_scheme = "greens")
Map.P_HousingFood <- mapper_function_quintile(final_table_sf, "E.PCT_HOUSING_AND_FOOD_SERVICES_facilities", "% Housing and Food Services", "none", color_scheme = "greens")
Map.P_ChildWelfare <- mapper_function_quintile(final_table_sf, "E.PCT_CHILD_SERVICES_AND_WELFARE_SERVICES_facilities", "% Child Services \nand Welfare Facilities", "none", color_scheme = "greens")


E.AGGREGATE_WELFARE <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_welfare", "Welfare Facilities \nHotspot Score", "right", color_scheme = "greens")

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

### EDUCATION

Map.P_K12 <- mapper_function_quintile(final_table_sf, "E.PCT_K12_EDUCATION_SERVICES_facilities", "% K-12 Education Facilities", "none", color_scheme = "greens")
Map.P_DayCare <- mapper_function_quintile(final_table_sf, "E.PCT_DAY_CARE_AND_PREKINDERGARTEN_facilities", "% Pre-K / Day Care \nEducation Facilities", "none", color_scheme = "greens")
Map.P_Higher <- mapper_function_quintile(final_table_sf, "E.PCT_COLLEGES_OR_UNIVERSITIES_facilities", "% Higher Education Facilities", "none", color_scheme = "greens")
Map.P_Proprietary <- mapper_function_quintile(final_table_sf, "E.PCT_PROPRIETARY_SCHOOLS_facilities", "% Proprietary Education Facilities", "none", color_scheme = "greens")

E.AGGREGATE_EDUCATION <- mapper_function_equal_breaks(final_table_sf, "E.HS.Agg_education", "Education Facilities \nIndex", "right", color_scheme = "greens")

frame <- ggarrange(Map.P_K12, 
                   Map.P_DayCare,
                   Map.P_Higher,
                   Map.P_Proprietary,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Welfare Facilities Per Community District \n Extreme Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/extreme/maps_ED.png", 
                   width = 1800, 
                   height = 2700)

final_table_sf$infra_hotspots_sum <- round(final_table_sf$E.HS.Agg_landuse +
                                             final_table_sf$E.HS.Agg_transport +
                                             final_table_sf$E.HS.Agg_education +
                                             final_table_sf$E.HS.Agg_welfare +
                                             final_table_sf$E.HS.Agg_psafety +
                                             final_table_sf$E.HS.Agg_healthcare,2)

final_table_sf$infra_hotspots_sum_n <- normalize(final_table_sf$infra_hotspots_sum) %>%
  round(2)

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

### Differences in SV between exposed - non exposed populations

template <- data.frame(CD = final_table$Geography)

# moderate

moderate <- template %>%
  mutate(Scenario = "Mod",
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
  mutate(Scenario = "ModUn",
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
# 
# # extreme
# 
# extreme <- template %>%
#   mutate(Scenario = "Ext",
#          PCT_Children = final_table$`E.Flooded PCT 0-5 children`,
#          PCT_Elderly = final_table$`E.Flooded PCT +65 elder`,
#          PCT_African_American = final_table$`E.Flooded PCT Black`,
#          PCT_Hispanic_Latinx = final_table$`E.Flooded PCT Hispanic`,
#          PCT_White = final_table$`E.Flooded PCT White`,
#          PovRate_e = final_table$E.Pov_rate_e,
#          PovRate_s = final_table$E.Pov_rate_s,
#          PCT_NoCar_e = final_table$E.PCT_No_Car_e,
#          PCT_NoCar_s = final_table$E.PCT_No_Car_s,
#          PCT_Rented_e = final_table$E.PCT_rented_e,
#          PCT_Rented_s = final_table$E.PCT_rented_s)
# 
# extreme_unexposed <- template %>%
#   mutate(Scenario = "ExtUn",
#          PCT_Children = final_table$`E.N.Flooded PCT 0-5 children`,
#          PCT_Elderly = final_table$`E.N.Flooded PCT +65 elder`,
#          PCT_African_American = final_table$`E.N.Flooded PCT Black`,
#          PCT_Hispanic_Latinx = final_table$`E.N.Flooded PCT Hispanic`,
#          PCT_White = final_table$`E.N.Flooded PCT White`,
#          PovRate_e = final_table$E.N.Pov_rate_e,
#          PovRate_s = final_table$E.N.Pov_rate_s,
#          PCT_NoCar_e = final_table$E.N.PCT_No_Car_e,
#          PCT_NoCar_s = final_table$E.N.PCT_No_Car_s,
#          PCT_Rented_e = final_table$E.N.PCT_rented_e,
#          PCT_Rented_s = final_table$E.N.PCT_rented_s)
# 
# # total
# 
# total <- template %>%
#   mutate(Scenario = "Total",
#          PCT_Children = final_table$`PCT 0-5 children`,
#          PCT_Elderly = final_table$`PCT +65 elder`,
#          PCT_African_American = final_table$`PCT Black`,
#          PCT_Hispanic_Latinx = final_table$`PCT Hispanic`,
#          PCT_White = final_table$`PCT White`,
#          PovRate_e = final_table$Pov_rate_e,
#          PovRate_s = final_table$Pov_rate_s,
#          PCT_NoCar_e = final_table$PCT_No_Car_e,
#          PCT_NoCar_s = final_table$PCT_No_Car_s,
#          PCT_Rented_e = final_table$PCT_rented_e,
#          PCT_Rented_s = final_table$PCT_rented_s)

SOVI_plot_m <- rbind(moderate, moderate_unexposed)

SOVI_plot_m$Scenario <- factor(SOVI_plot_m$Scenario, levels = c("Mod", 
                                                                "ModUn", 
                                                                "Ext",
                                                                "ExtUn",
                                                                "Total"))


df_sig_test <- data.frame(CD = unique(SOVI_plot_m$CD))

Mod_EvN <- c("Mod", "ModUn")

for(CD in unique(df_sig_test$CD)){
  
  for(estimate in names(SOVI_plot_m[grepl("_e", names(SOVI_plot_m))])){
    
    estimate_2 <- str_remove(estimate, "_e")
    
    data_estimate <- cbind(select(SOVI_plot_m, c(CD, Scenario)), SOVI_plot_m[, grepl(estimate_2, names(SOVI_plot_m))])
    
    A <- as.numeric(SOVI_plot_m[SOVI_plot_m$CD == CD & SOVI_plot_m$Scenario == Mod_EvN[1], estimate])
    B <- as.numeric(SOVI_plot_m[SOVI_plot_m$CD == CD & SOVI_plot_m$Scenario == Mod_EvN[2], estimate])
    SE_A <- as.numeric(SOVI_plot_m[SOVI_plot_m$CD == CD & SOVI_plot_m$Scenario == Mod_EvN[1], paste0(estimate_2, "_s")])
    SE_B <- as.numeric(SOVI_plot_m[SOVI_plot_m$CD == CD & SOVI_plot_m$Scenario == Mod_EvN[2], paste0(estimate_2, "_s")])
    
    Z_Mod_EvN <- (A-B)/((SE_A^2 + SE_B^2)^0.5)
    
    Delta_Mod_EvN <- A-B
    
    df_sig_test[df_sig_test$CD == CD, paste0("diff_", estimate_2, "_", Mod_EvN[1],"_", Mod_EvN[2])] <- Delta_Mod_EvN
    df_sig_test[df_sig_test$CD == CD, paste0("sig_",estimate_2, "_", Mod_EvN[1],"_", Mod_EvN[2])] <- Z_Mod_EvN
  }
  
}

df_sig_test["diff_PCT_b5_Mod_ModUn"] <- final_table_sf$`M.Flooded PCT 0-5 children` - final_table_sf$`M.N.Flooded PCT 0-5 children`
df_sig_test["diff_PCT_a65_Mod_ModUn"] <- final_table_sf$`M.Flooded PCT +65 elder` - final_table_sf$`M.N.Flooded PCT +65 elder`
df_sig_test["diff_PCT_AfAm_Mod_ModUn"] <- final_table_sf$`M.Flooded PCT Black` - final_table_sf$`M.N.Flooded PCT Black`
df_sig_test["diff_PCT_Hisp_Mod_ModUn"] <- final_table_sf$`M.Flooded PCT Hispanic` - final_table_sf$`M.N.Flooded PCT Hispanic`
df_sig_test["diff_PCT_White_Mod_ModUn"] <- final_table_sf$`M.Flooded PCT White` - final_table_sf$`M.N.Flooded PCT White`

final_table_sf_justice_added <- inner_join(final_table_sf, df_sig_test, by=c("boro_cd" = "CD"))


ggplot() +
  geom_sf(data = final_table_sf_justice_added, aes(fill = diff_PCT_b5_Mod_ModUn)) +
  geom_sf_text(data = final_table_sf_justice_added, aes(label = round(diff_PCT_b5_Mod_ModUn,2)), colour="black") +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(2, "cm")) + 
  labs(title = "SV Difference \nExposed VS Unexposed Populations \nModerate Scenario") +
  scale_fill_gradient2(midpoint = 0,
                       low = "#5d7a00",
                       mid = "#FFEBCC",
                       high = "#9e1405",
                       breaks = c(range(final_table_sf_justice_added$diff_PCT_b5_Mod_ModUn, na.rm = TRUE)[1],
                                  0, 
                                  range(final_table_sf_justice_added$diff_PCT_b5_Mod_ModUn, na.rm = TRUE)[2]),
                       labels = c(round(range(final_table_sf_justice_added$diff_PCT_b5_Mod_ModUn, na.rm = TRUE)[1],2),
                                  0, 
                                  round(range(final_table_sf_justice_added$diff_PCT_b5_Mod_ModUn, na.rm = TRUE)[2],2)),
                       name= "% Children")

ggsave(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_SV_diff_children.png", 
       dpi = 1000,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

ggplot() +
  geom_sf(data = final_table_sf_justice_added, aes(fill = diff_PCT_a65_Mod_ModUn)) +
  geom_sf_text(data = final_table_sf_justice_added, aes(label = round(diff_PCT_a65_Mod_ModUn,2)), colour="black") +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(2, "cm")) + 
  labs(title = "SV Difference \nExposed VS Unexposed Populations \nModerate Scenario") +
  scale_fill_gradient2(midpoint = 0,
                       low = "#5d7a00",
                       mid = "#FFEBCC",
                       high = "#9e1405",
                       breaks = c(range(final_table_sf_justice_added$diff_PCT_a65_Mod_ModUn, na.rm = TRUE)[1],
                                  0, 
                                  range(final_table_sf_justice_added$diff_PCT_a65_Mod_ModUn, na.rm = TRUE)[2]),
                       labels = c(round(range(final_table_sf_justice_added$diff_PCT_a65_Mod_ModUn, na.rm = TRUE)[1],2),
                                  0, 
                                  round(range(final_table_sf_justice_added$diff_PCT_a65_Mod_ModUn, na.rm = TRUE)[2],2)),
                       name= "% Elderly")

ggsave(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_SV_diff_elderly.png", 
       dpi = 1000,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

ggplot() +
  geom_sf(data = final_table_sf_justice_added, aes(fill = diff_PCT_AfAm_Mod_ModUn)) +
  geom_sf_text(data = final_table_sf_justice_added, aes(label = round(diff_PCT_AfAm_Mod_ModUn,2)), colour="black") +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(2, "cm")) + 
  labs(title = "SV Difference \nExposed VS Unexposed Populations \nModerate Scenario") +
  scale_fill_gradient2(midpoint = 0,
                       low = "#5d7a00",
                       mid = "#FFEBCC",
                       high = "#9e1405",
                       breaks = c(range(final_table_sf_justice_added$diff_PCT_AfAm_Mod_ModUn, na.rm = TRUE)[1],
                                  0, 
                                  range(final_table_sf_justice_added$diff_PCT_AfAm_Mod_ModUn, na.rm = TRUE)[2]),
                       labels = c(round(range(final_table_sf_justice_added$diff_PCT_AfAm_Mod_ModUn, na.rm = TRUE)[1],2),
                                  0, 
                                  round(range(final_table_sf_justice_added$diff_PCT_AfAm_Mod_ModUn, na.rm = TRUE)[2],2)),
                       name= "% African American")

ggsave(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_SV_diff_AfAm.png", 
       dpi = 1000,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

ggplot() +
  geom_sf(data = final_table_sf_justice_added, aes(fill = diff_PCT_Hisp_Mod_ModUn)) +
  geom_sf_text(data = final_table_sf_justice_added, aes(label = round(diff_PCT_Hisp_Mod_ModUn,2)), colour="black") +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(2, "cm")) + 
  labs(title = "SV Difference \nExposed VS Unexposed Populations \nModerate Scenario") +
  scale_fill_gradient2(midpoint = 0,
                       low = "#5d7a00",
                       mid = "#FFEBCC",
                       high = "#9e1405",
                       breaks = c(range(final_table_sf_justice_added$diff_PCT_Hisp_Mod_ModUn, na.rm = TRUE)[1],
                                  0, 
                                  range(final_table_sf_justice_added$diff_PCT_Hisp_Mod_ModUn, na.rm = TRUE)[2]),
                       labels = c(round(range(final_table_sf_justice_added$diff_PCT_Hisp_Mod_ModUn, na.rm = TRUE)[1],2),
                                  0, 
                                  round(range(final_table_sf_justice_added$diff_PCT_Hisp_Mod_ModUn, na.rm = TRUE)[2],2)),
                       name= "% Hispanic")

ggsave(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_SV_diff_Hisp.png", 
       dpi = 1000,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

ggplot() +
  geom_sf(data = final_table_sf_justice_added, aes(fill = diff_PCT_White_Mod_ModUn)) +
  geom_sf_text(data = final_table_sf_justice_added, aes(label = round(diff_PCT_White_Mod_ModUn,2)), colour="black") +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(2, "cm")) + 
  labs(title = "SV Difference \nExposed VS Unexposed Populations \nModerate Scenario") +
  scale_fill_gradient2(midpoint = 0,
                       low = "#5d7a00",
                       mid = "#FFEBCC",
                       high = "#9e1405",
                       breaks = c(range(final_table_sf_justice_added$diff_PCT_White_Mod_ModUn, na.rm = TRUE)[1],
                                  0, 
                                  range(final_table_sf_justice_added$diff_PCT_White_Mod_ModUn, na.rm = TRUE)[2]),
                       labels = c(round(range(final_table_sf_justice_added$diff_PCT_White_Mod_ModUn, na.rm = TRUE)[1],2),
                                  0, 
                                  round(range(final_table_sf_justice_added$diff_PCT_White_Mod_ModUn, na.rm = TRUE)[2],2)),
                       name= "% White")

ggsave(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_SV_diff_White.png", 
       dpi = 1000,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

ggplot() +
  geom_sf(data = final_table_sf_justice_added, aes(fill = diff_PovRate_Mod_ModUn)) +
  geom_sf_text(data = final_table_sf_justice_added, aes(label = round(diff_PovRate_Mod_ModUn,2)), colour="black") +
  theme_map() +
  theme(title = element_text(size = 36),
        legend.position = "right",
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 30),
        legend.spacing.x = unit(0.2, "cm"),
        legend.key.size = unit(2, "cm")) + 
  labs(title = "SV Difference \nExposed VS Unexposed Populations \nModerate Scenario") +
  scale_fill_gradient2(midpoint = 0,
                       low = "#5d7a00",
                       mid = "#FFEBCC",
                       high = "#9e1405",
                       breaks = c(range(final_table_sf_justice_added$diff_PovRate_Mod_ModUn, na.rm = TRUE)[1],
                                  0, 
                                  range(final_table_sf_justice_added$diff_PovRate_Mod_ModUn, na.rm = TRUE)[2]),
                       labels = c(round(range(final_table_sf_justice_added$diff_PovRate_Mod_ModUn, na.rm = TRUE)[1],2),
                                  0, 
                                  round(range(final_table_sf_justice_added$diff_PovRate_Mod_ModUn, na.rm = TRUE)[2],2)),
                       name= "% Poverty")

ggsave(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_SV_diff_PovRate.png", 
       dpi = 1000,
       width = 30,
       height = 30,
       units = "cm",
       limitsize = FALSE)

### GRAVEYARD

# final_table_sf <- final_table_sf %>%
#   mutate("M.pct_road_n" = normalize(final_table_sf$`M.Flooded PCT road`),
#          "M.PCT.busstops_n" = normalize(final_table_sf$`M.Flooded PCT bus stops`),
#          "M.PCT.busroutes_n" = normalize(final_table_sf$`M.Flooded PCT bus routes`),
#          "M.PCT.subentrances_n" = normalize(final_table_sf$`M.Flooded PCT subway entrances`)
#   ) %>% 
#   mutate(agg_norm_index_transport = M.pct_road_n +
#            M.PCT.busstops_n  +
#            M.PCT.busroutes_n +
#            M.PCT.subentrances_n)
# 
# 
# AGGREGATE_TRANSPORT <- mapper_function_quintile(final_table_sf, "agg_norm_index_transport", "Transport Infrastructure \nExposure Index", "none")
