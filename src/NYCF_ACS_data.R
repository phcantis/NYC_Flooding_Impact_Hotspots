# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL MEASURE THE MINIMUM DISTANCE TO FLOODING OF NEW YORK CITY'S 2018-2022 CENSUS BLOCK GROUPS.

## this script is meant to be sourced directly from the script "NYCF_report_CD_impacts.R"
## due to datasets and functions being loaded in the parent script, running this file independently will require line-by-line checks for missing datasets

## we load the input data - American Community Survey 5-year estimates (2018-2022) at the census block group level

NYC_ACS2018 <- st_read("data/1_raw/NYC_2018_ACS.geojson") %>%
  filter(ALAND > 0) %>% # we filter census block groups that are 100% water, since we will not care about their exposure to flooding
  st_transform(UTM_18N_meter)

## the lines below will write a new column in the dataset indicating the distance to the closest flooding 
## the new column called "m_d_f" refers to exposure under the moderate scenario, while "e_d_f" does for the extreme scenario

NYC_ACS2018 <- write_closest_flooding(NYC_ACS2018, 
                                      moderate_flooding, 
                                      "m_d_f")

NYC_ACS2018 <- write_closest_flooding(NYC_ACS2018, 
                                      extreme_flooding, 
                                      "e_d_f")

## in order to aggregate exposure statistics to the CD level, we need to spatially join census block groups to CDs

NYC_ACS2018 <- st_join(NYC_ACS2018, CD_with_parks, largest = TRUE)

## sourced ACS data requires some processing to aggregate native variables into the target indicators. 
## because ACS data comes as estimates, they also include margins of error (here already converted to standard error in columns ending with "_s")
## for instance the indicator "people below the poverty line" (belpov_e) requires aggregating the native variables providing "people living below 1/2 the poverty line" and "people living between 1/2 and the poverty line"
## estimates are aggregated, and error propagation is considered using the formulas indicated by the Census Bureau. See https://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2020_Instructions_for_Stat_Testing_ACS.pdf

## below, a dataset with the final indicators is prepared based on the native sourced variables.

NYC_ACS2018 <- NYC_ACS2018 %>% 
  st_drop_geometry() %>%          # as exposure has been assessed and CDs have been assigned to each CBG, geometry is no longer needed. we remove it and treat the data as a data frame
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

rm(NYC_ACS2018)

## because we use so many economy-related indicators, this code below is used to check for multi collinearity using the Variance Inflation Factor

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


