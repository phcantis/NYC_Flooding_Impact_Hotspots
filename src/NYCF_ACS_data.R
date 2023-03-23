# source("src/NYCF_housekeeping_GIS_vars.R")

# vars_ACS18 <- load_variables(2018, "acs5", cache = TRUE)

# Poverty

vi_total_incomeratio <- c("C17002_001", "incrat_total")
vi_total_incomeratiomin50 <- c("C17002_002", "incrat_50")
vi_total_incomeratio50_99 <- c("C17002_003", "incrat_99")

# households with income below 75K

vi_total_HH_income <- c("B19001_001", "HH_Income_total")
vi_HH_income_0_10 <- c("B19001_002", "HH_Income_0_10")
vi_HH_income_10_14 <- c("B19001_003", "HH_Income_10_14")
vi_HH_income_15_19 <- c("B19001_004", "HH_Income_15_19")
vi_HH_income_20_24 <- c("B19001_005", "HH_Income_20_24")
vi__HH_income_25_29 <- c("B19001_006", "HH_Income_25_29")
vi__HH_income_30_34 <- c("B19001_007", "HH_Income_30_34")
vi__HH_income_35_39 <- c("B19001_008", "HH_Income_35_39")
vi__HH_income_40_44 <- c("B19001_009", "HH_Income_40_44")
vi__HH_income_45_49 <- c("B19001_010", "HH_Income_45_49")
vi__HH_income_50_59 <- c("B19001_011", "HH_Income_50_59")
vi__HH_income_60_74 <- c("B19001_012", "HH_Income_60_74")

# cost-burdened owner occ-households

vi_total_cost_burdened <- c("B25091_001", "HH_CB_total")
vi_cost_burdened_m_30_34 <- c("B25091_008", "HH_CB_m_30_34")
vi_cost_burdened_m_35_39 <- c("B25091_009", "HH_CB_m_35_39")
vi_cost_burdened_m_40_49 <- c("B25091_010", "HH_CB_m_40_49")
vi_cost_burdened_m_50 <- c("B25091_011", "HH_CB_m_50")

vi_cost_burdened_nm_30_34 <- c("B25091_019", "HH_CB_nm_30_34")
vi_cost_burdened_nm_35_39 <- c("B25091_020", "HH_CB_nm_35_39")
vi_cost_burdened_nm_40_49 <- c("B25091_021", "HH_CB_nm_40_49")
vi_cost_burdened_nm_50 <- c("B25091_022", "HH_CB_nm_50")

# rent burdened households

vi_total_rent_burdened <- c("B25070_001", "HH_RB_total")
vi_rent_burdened_30_34 <- c("B25070_007", "HH_RB_30_34")
vi_rent_burdened_35_39 <- c("B25070_008", "HH_RB_35_39")
vi_rent_burdened_40_49 <- c("B25070_009", "HH_RB_40_49")
vi_rent_burdened_50 <- c("B25070_010", "HH_RB_50")

acs_vars <- list(vi_total_incomeratiomin50,
                 vi_total_incomeratio50_99,
                 vi_total_HH_income,
                 
                 vi_HH_income_0_10,
                 vi_HH_income_10_14,
                 vi_HH_income_15_19,
                 vi_HH_income_20_24,
                 vi__HH_income_25_29,
                 vi__HH_income_30_34,
                 vi__HH_income_35_39,
                 vi__HH_income_40_44,
                 vi__HH_income_45_49,
                 vi__HH_income_50_59,
                 vi__HH_income_60_74,
                 
                 vi_total_cost_burdened,
                 vi_cost_burdened_m_30_34,
                 vi_cost_burdened_m_35_39,
                 vi_cost_burdened_m_40_49,
                 vi_cost_burdened_m_50,
                 vi_cost_burdened_nm_30_34, 
                 vi_cost_burdened_nm_35_39,
                 vi_cost_burdened_nm_40_49,
                 vi_cost_burdened_nm_50,
                 
                 vi_total_rent_burdened,
                 vi_rent_burdened_30_34,
                 vi_rent_burdened_35_39,
                 vi_rent_burdened_40_49,
                 vi_rent_burdened_50)

NYC_ACS2018 <- get_acs(geography = "block group", variables = vi_total_incomeratio[1],
                       keep_geo_vars = TRUE,
                       state = "NY", county = c("005", "047", "061", "081", "085"), 
                       year = 2018,
                       survey = "acs5",
                       geometry = TRUE) %>% 
  select(-c(variable, NAME.y, NAME.x, TRACTCE)) %>% 
  rename (!!paste0(vi_total_incomeratio[2], "_e") := estimate, !!paste0(vi_total_incomeratio[2], "_s") := moe) %>%
  mutate(!!paste0(vi_total_incomeratio[2], "_s") := .data[[!!paste0(vi_total_incomeratio[2], "_s")]] / 1.645) %>% 
  filter(ALAND > 10)

for(var in acs_vars){
  
  print(var[2])
  
  NYC_ACS2018.temp <- get_acs(geography = "block group", variables = var[1],
                              state = "NY", county = c("005", "047", "061", "081", "085"),
                              year = 2018,
                              survey = "acs5",
                              geometry = FALSE) %>% 
    select(-c(variable, NAME)) %>% 
    rename(!!paste0(var[2], "_e") := estimate, !!paste0(var[2], "_s") := moe) %>%
    mutate(!!paste0(var[2], "_s") := .data[[!!paste0(var[2], "_s")]]/1.645)
  
  NYC_ACS2018 <- left_join(NYC_ACS2018, NYC_ACS2018.temp, by = "GEOID")
  rm(NYC_ACS2018.temp)
}

# Now we add disability data, which we have from previous work. The raw data has not been committed or pushed due to size,
# but is available upon request. the commented code below serves to keep track of pre-processing done to the data.


# SOVI_data_2018 <- st_read(path) %>%
#   filter(STATEFP == 36) %>%
#   filter(COUNTYF %in% c("005", "047", "061", "081", "085")) %>%
#   st_transform(UTM_18N_meter) %>%
#   st_make_valid() %>%
#   select(GEOID, TotHH_e, TotHH_s, Disability_e = Disblty_, Disability_s = Dsblty_s, pct_disability_e = P_Dsabl_, pct_disability_s = P_Dsbl_s) %>%
#   st_drop_geometry()
# 
# write_csv(SOVI_data_2018, "data/1_raw/ACS18_BG_disability_data.csv")

SOVI_data_2018 <- read_csv("data/1_raw/ACS18_BG_disability_data.csv") %>%
  mutate(GEOID = as.character(GEOID))

NYC_ACS2018 <- NYC_ACS2018 %>%
  left_join(SOVI_data_2018, by = "GEOID") %>%
  st_transform(UTM_18N_meter)

CD_with_parks <- st_read("data/1_raw/Community_Districts.shp") %>% 
  select(boro_cd) %>%
  st_transform(UTM_18N_meter) %>% 
  arrange(boro_cd)

NYC_ACS2018 <- st_join(NYC_ACS2018, CD_with_parks, largest = TRUE)

NYC_ACS2018 <- write_closest_flooding(NYC_ACS2018, moderate_flooding, "m_d_f")

NYC_ACS2018 <- write_closest_flooding(NYC_ACS2018, extreme_flooding, "e_d_f")

NYC_ACS2018 <- NYC_ACS2018 %>% 
  st_drop_geometry() %>%
  mutate(
    Total_Belpov_e = incrat_total_e,
    Total_Belpov_s = incrat_total_s,
    Belpov_e = incrat_50_e + incrat_99_e,
    Belpov_s = (incrat_50_s^2 + incrat_99_s^2)^0.5,
    
    Total_HH_Income_e = HH_Income_total_e,
    Total_HH_Income_s = HH_Income_total_s,
    HH_Income_Bel75K_e = HH_Income_0_10_e + HH_Income_10_14_e + HH_Income_15_19_e + HH_Income_20_24_e + HH_Income_25_29_e + HH_Income_30_34_e + HH_Income_35_39_e + HH_Income_40_44_e + HH_Income_45_49_e + HH_Income_50_59_e + HH_Income_60_74_e,
    HH_Income_Bel75K_s = (HH_Income_0_10_s^2 + HH_Income_10_14_s^2 + HH_Income_15_19_s^2 + HH_Income_20_24_s^2 + HH_Income_25_29_s^2 + HH_Income_30_34_s^2 + HH_Income_35_39_s^2 + HH_Income_40_44_s^2 + HH_Income_45_49_s^2 + HH_Income_50_59_s^2 + HH_Income_60_74_s^2)^0.5,
    
    Total_HH_Dis_e = TotHH_e,
    Total_HH_Dis_s = TotHH_s,
    HH_Disability_e = Disability_e,
    HH_Disability_s = Disability_s,
    
    Total_Rent_HH_e = HH_RB_total_e,
    Total_Rent_HH_s = HH_RB_total_s,
    RB_HH_e = HH_RB_30_34_e + HH_RB_35_39_e + HH_RB_40_49_e + HH_RB_50_e,
    RB_HH_s = (HH_RB_30_34_s^2 + HH_RB_35_39_s^2 + HH_RB_40_49_s^2 + HH_RB_50_s^2)^0.5,
    
    Total_Owned_HH_e = HH_CB_total_e,
    Total_Owned_HH_s = HH_CB_total_s,
    CB_HH_e = HH_CB_m_30_34_e + HH_CB_m_35_39_e + HH_CB_m_40_49_e + HH_CB_m_50_e + HH_CB_nm_30_34_e + HH_CB_nm_35_39_e + HH_CB_nm_40_49_e + HH_CB_nm_50_e,
    CB_HH_s = (HH_CB_m_30_34_s^2 + HH_CB_m_35_39_s^2 + HH_CB_m_40_49_s^2 + HH_CB_m_50_s^2 + HH_CB_nm_30_34_s^2 + HH_CB_nm_35_39_s^2 + HH_CB_nm_40_49_s^2 + HH_CB_nm_50_s^2)^0.5
  ) %>%
  select(GEOID, ALAND, boro_cd, 
         Total_Belpov_e, Total_Belpov_s, Belpov_e, Belpov_s, 
         Total_HH_Income_e, Total_HH_Income_s, HH_Income_Bel75K_e, HH_Income_Bel75K_s, 
         Total_HH_Dis_e, Total_HH_Dis_s, HH_Disability_e, HH_Disability_s, 
         Total_Rent_HH_e, Total_Rent_HH_s, RB_HH_e, RB_HH_s,
         Total_Owned_HH_e, Total_Owned_HH_s, CB_HH_e, CB_HH_s,
         m_d_f, e_d_f)

write_csv(NYC_ACS2018, "data/2_intermediate/NYC_ACS_2018.csv")

# Some code to check collinearity

# NYC_ACS2018 <- NYC_ACS2018 %>%
#   mutate(
#     pct_belpov = case_when(
#       incrat_total_e != 0 ~ 100 * ((incrat_50_e + incrat_99_e) / incrat_total_e),
#       incrat_total_e == 0 ~ 0),
#     pct_bel_75k = case_when(
#       HH_Income_total_e != 0 ~ 100 * ((HH_Income_0_10_e + HH_Income_10_14_e + HH_Income_15_19_e + HH_Income_20_24_e + HH_Income_25_29_e + HH_Income_30_34_e + HH_Income_35_39_e + HH_Income_40_44_e + HH_Income_45_49_e + HH_Income_50_59_e + HH_Income_60_74_e) / HH_Income_total_e),
#       HH_Income_total_e == 0 ~ 0),
#     pct_cb = case_when(
#       HH_CB_total_e != 0 ~ 100 * ((HH_CB_m_30_34_e + HH_CB_m_35_39_e + HH_CB_m_40_49_e + HH_CB_m_50_e + HH_CB_nm_30_34_e + HH_CB_nm_35_39_e + HH_CB_nm_40_49_e + HH_CB_nm_50_e)/HH_CB_total_e),
#       HH_CB_total_e == 0 ~ 0),
#     pct_rb = case_when(
#       HH_RB_total_e != 0 ~ 100 * ((HH_RB_30_34_e + HH_RB_35_39_e + HH_RB_40_49_e + HH_RB_50_e) / HH_RB_total_e),
#       HH_RB_total_e == 0 ~ 0)
#   )
# 
# NYC_ACS2018_corr_explore <- NYC_ACS2018 %>% select(pct_belpov,
#                                                    pct_bel_75k,
#                                                    pct_cb,
#                                                    pct_rb) %>%
#   st_drop_geometry()
# 
# graf <- ggpairs(NYC_ACS2018_corr_explore, upper = list(continuous = wrap("cor", size = 2.5)))
# 
# ggplotly(graf)
# 
# usdm::vif(NYC_ACS2018_corr_explore)


