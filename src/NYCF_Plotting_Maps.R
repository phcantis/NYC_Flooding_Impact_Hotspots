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

Map.Legend_LUSE <- mapper_legender_frame(data_df = final_table_sf, fieldname = "M.PCT_industrial_lots", legend_position = "top")
Map.Legend_LEFT <- mapper_legender_frame(data_df = final_table_sf, fieldname = "M.PCT_road_area", legend_position = c(0.1,0.5), key_size_cm = 3, text_title_size = 50, text_size = 40)

## Population & LAND USE 
M.Map.P_population <- mapper_function_quintile(final_table_sf, "M.PCT_Total_population", "% Population Impacted", "none")

M.Map.P_Res_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_residential_lots", "% Residential Lots Impacted", "none")

M.Map.P_ResBsmnt_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_basement_residential_lots", "% Residential Lots with a \n Basement Impacted", "none")

M.Map.P_Mixed_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_mixed_comres_lots", "% Mixed Residential/Commercial \n Lots Impacted", "none")

M.Map.P_Commercial_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_commercial_lots", "% Commercial Lots Impacted", "none")

M.Map.P_Industrial_Flooded <- mapper_function_quintile(final_table_sf, "M.PCT_industrial_lots", "% Industrial Lots Impacted", "none")

M.AGGREGATE_LUSE <- mapper_function_equal_breaks(final_table_sf, fieldname = "M.HS.Agg_landuse", map_title = "Land Use \nHotspot Score", legend_position = "none", n_breaks = 5)
M.AGGREGATE_POP <- mapper_function_equal_breaks(final_table_sf, fieldname = "M.HS.Agg_population", map_title = "Population \nHotspot Score", legend_position = c(0.99,0.2), n_breaks = 5)

frame <- ggarrange(M.Map.P_Res_Flooded,
                   M.Map.P_ResBsmnt_Flooded, 
                   M.Map.P_Mixed_Flooded,
                   M.Map.P_Commercial_Flooded,
                   M.Map.P_Industrial_Flooded,
                   Map.Legend_LEFT,
                   nrow = 3, ncol = 2, 
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Land Use Exposure and Population Indicators Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "data/4_display/maps/maps_LU_exposure.png", 
                   width = 1800, 
                   height = 2400) 

## Transport

Map.P_Road_Flooded <- mapper_function_quintile(final_table_sf, "M.Flooded PCT road", "% Road Area Flooded", "none")
Map.P_Bus_Stops_Flooded <- mapper_function_quintile(final_table_sf, "M.Flooded PCT bus stops", "% Bus Stops Impacted", "none")
Map.P_Bus_Routes_Flooded <- mapper_function_quintile(final_table_sf, "M.Flooded PCT bus routes", "% Bus Routes Impacted", "none")
Map.P_Subway_Entrances_Flooded <- mapper_function_quintile(final_table_sf, "M.Flooded PCT subway entrances", "% Subway Entrances Impacted", "none")

AGGREGATE_TRANSPORT <- mapper_function_equal_breaks(final_table_sf, "agg_hs_transport", "Transport \nHotspot Score", "right")

frame <- ggarrange(Map.P_Road_Flooded,
                   Map.P_Bus_Stops_Flooded, 
                   Map.P_Bus_Routes_Flooded,
                   Map.P_Subway_Entrances_Flooded,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2, 
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Transportation Exposure Indicators Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_Transport_exposure.png", 
                   width = 2200, 
                   height = 3200) 

## SOVI
Map.P_bel_5 <- mapper_function_quintile(final_table_sf, "M.Flooded PCT 0-5 children", "% Below 5 Years Old", "none")
Map.P_above_65 <- mapper_function_quintile(final_table_sf, "M.Flooded PCT +65 elder", "% Above 65 Years Old", "none")
Map.P_BIPOC <- mapper_function_quintile(final_table_sf, "M.pct_BIPOC", "% BIPOC", "none")
Map.P_below_poverty <- mapper_function_quintile(final_table_sf, "M.Pov_rate_e", "% Below the Poverty Line", "none")
Map.P_no_car <- mapper_function_quintile(final_table_sf, "M.PCT_No_Car_e", "% Households Without a Car", "none")
Map.P_rented_hh <- mapper_function_quintile(final_table_sf, "M.PCT_rented_e", "% Rented Households", "none")

AGGREGATE_SOVI <- mapper_function_equal_breaks(final_table_sf, "agg_hs_sovi", "Social Vulnerability \nHotspot Score", "right")


frame <- ggarrange(Map.P_BIPOC, 
                   Map.P_bel_5,
                   Map.P_above_65,
                   Map.P_below_poverty,
                   Map.P_no_car,
                   Map.P_rented_hh,
                   Map.Legend_LUSE,
                   nrow = 4, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Social Vulnerability Indicators Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_SV.png", 
                   width = 1800, 
                   height = 3600) 

### PUBLIC SAFETY

Map.P_POLICE <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of POLICE SERVICES facilities", "% Police Services", "none")
Map.P_FIRE <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of FIRE SERVICES facilities", "% Fire Services", "none")
Map.P_OTHER_EMERGENCY <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of OTHER SAFETY AND EMERGENCY FACILITIES facilities", "% Other Safety And Emergency \nmanagement Services", "none")


AGGREGATE_PSAFETY <- mapper_function_equal_breaks(final_table_sf, "agg_hs_psafety", "Public Safety Facilities \nHotspot Score", "right")

frame <- ggarrange(Map.P_POLICE, 
                   Map.P_FIRE,
                   Map.P_OTHER_EMERGENCY,
                   Map.Legend_TRANSPORT,
                   nrow = 2, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Public Safety Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_PS.png", 
                   width = 1800, 
                   height = 2700) 

### HEALTHCARE

Map.P_HospitalsClinics <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of HOSPITALS AND CLINICS facilities", "% Hospitals and Clinics", "none")
Map.P_ResidentialHCare <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of RESIDENTIAL HEALTH CARE facilities", "% Residential Healtchare Facilities", "none")
Map.P_MentalHealth <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of MENTAL HEALTH, HEALTH PROMOTION, AND CHEMICAL DEPENDENCY SERVICES facilities", "% Mental Health and \nChemical Dependency Facilities", "none")
Map.P_OtherHealthCare <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of OTHER HEALTH CARE facilities", "% Other Healthcare Facilities", "none")



AGGREGATE_HEALTHCARE <- mapper_function_equal_breaks(final_table_sf, "agg_hs_healthcare", "Healthcare Facilities \nHotspot Score", "right")

frame <- ggarrange(Map.P_HospitalsClinics, 
                   Map.P_ResidentialHCare,
                   Map.P_MentalHealth,
                   Map.P_OtherHealthCare,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Healthcare Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_HC.png", 
                   width = 1800, 
                   height = 3600) 

### WELFARE

Map.P_HumanDev <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of HUMAN DEVELOPMENT SERVICES facilities", "% Human Development Facilities", "none")
Map.P_Senior <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of SENIOR AND DISSABILITY SERVICES facilities", "% Senior and Disability Services", "none")
Map.P_HousingFood <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of HOUSING AND FOOD SERVICES facilities", "% Housing and Food Services", "none")
Map.P_ChildWelfare <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of CHILD SERVICES AND WELFARE SERVICES facilities", "% Child Services \nand Welfare Facilities", "none")


AGGREGATE_WELFARE <- mapper_function_equal_breaks(final_table_sf, "agg_hs_welfare", "Welfare Facilities \nHotspot Score", "right")

frame <- ggarrange(Map.P_HumanDev, 
                   Map.P_Senior,
                   Map.P_HousingFood,
                   Map.P_ChildWelfare,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Welfare Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_WF.png", 
                   width = 1800, 
                   height = 2700)

### EDUCATION

Map.P_K12 <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of K-12 EDUCATION SERVICES facilities", "% K-12 Education Facilities", "none")
Map.P_DayCare <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of DAY CARE AND PRE-KINDERGARTEN facilities", "% Pre-K / Day Care \nEducation Facilities", "none")
Map.P_Higher <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of COLLEGES OR UNIVERSITIES facilities", "% Higher Education Facilities", "none")
Map.P_Proprietary <- mapper_function_quintile(final_table_sf, "M.Flooded PCT of PROPRIETARY SCHOOLS facilities", "% Proprietary Education Facilities", "none")

AGGREGATE_EDUCATION <- mapper_function_equal_breaks(final_table_sf, "agg_hs_education", "Education Facilities \nIndex", "right")

frame <- ggarrange(Map.P_K12, 
                   Map.P_DayCare,
                   Map.P_Higher,
                   Map.P_Proprietary,
                   Map.Legend_LUSE,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Impact of Flooding on Welfare Facilities Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 50))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/maps_ED.png", 
                   width = 1800, 
                   height = 2700)

final_table_sf$infra_hotspots_sum <- round(final_table_sf$agg_hs_landuse +
                                             final_table_sf$agg_hs_transport +
                                             final_table_sf$agg_hs_education +
                                             final_table_sf$agg_hs_welfare +
                                             final_table_sf$agg_hs_psafety +
                                             final_table_sf$agg_hs_healthcare,2)

final_table_sf$infra_hotspots_sum_n <- normalize(final_table_sf$infra_hotspots_sum) %>%
  round(2)

frame <- ggarrange(AGGREGATE_LUSE,
                   AGGREGATE_TRANSPORT,
                   AGGREGATE_EDUCATION,
                   AGGREGATE_HEALTHCARE,
                   AGGREGATE_WELFARE,
                   AGGREGATE_PSAFETY,
                   nrow = 3, ncol = 2,
                   common.legend = FALSE) %>%
  annotate_figure(top = text_grob("\n Hotspot Scores Per Infrastructure Risk Category Per Community District \n Moderate Scenario \n", 
                                  color = "black", face = "bold", size = 70))

frame %>% ggexport(filename = "C:/Users/herrerop/Desktop/GIS/NYC_STORMWATER/StormWater_project/report_redo/display/infra_maps_ALL_HOTSPOTS.png", 
                   width = 3600, 
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
