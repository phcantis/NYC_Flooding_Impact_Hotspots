# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL MEASURE THE MINIMUM DISTANCE TO FLOODING OF NEW YORK CITY'S TAX LOTS.

## this script is meant to be sourced directly from the script "NYCF_report_CD_impacts.R"
## due to datasets and functions being loaded in the parent script, running this file independently will require line-by-line checks for missing datasets

## we load the input data - MapPLUTO's database for 2020

tax_lots_flooding <- st_read("data/1_raw/MapPLUTO.shp") %>%
  select(boro_cd = CD, LandUse, BBL, UnitsRes, BsmtCode) %>%
  st_transform(crs = UTM_18N_meter) %>%
  st_make_valid() %>%
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595)) # remove tax lots present in parks due to a lack of hazard and demographic data on them

## the lines below will write a new column in the dataset indicating the distance to the closest flooding 
## the new column called "m_d_f" refers to exposure under the moderate scenario, while "e_d_f" does for the extreme scenario

tax_lots_flooding <- write_closest_flooding(tax_lots_flooding,
                                               moderate_flooding,
                                               "m_d_f")

tax_lots_flooding <- write_closest_flooding(tax_lots_flooding,
                                               extreme_flooding,
                                               "e_d_f")

## land use categories are coded in the original dataset. we create a new column describing the land use with textto make interpretation easier

tax_lots_flooding <- tax_lots_flooding %>%
  mutate(LUseCat = case_when(as.integer(LandUse) %in% c(1,2,3) ~ "Residential",
                             as.integer(LandUse) == 4 ~ "Mixed residential and commercial",
                             as.integer(LandUse) == 5 ~ "Commercial",
                             as.integer(LandUse) == 6 ~ "Industrial & manufacturing",
                             as.integer(LandUse) == 7 ~ "Trasportation and utility",
                             as.integer(LandUse) == 8 ~ "Public facilities and institutions",
                             as.integer(LandUse) == 9 ~ "Open space and outdoor recreation",
                             as.integer(LandUse) == 10 ~ "Parking facilities",
                             as.integer(LandUse) == 11 ~ "Vacant land"),
         BSMT_RES = case_when(as.integer(LandUse) %in% c(1,2,3) & BsmtCode %in% c(1,2,3,4) & UnitsRes %in% c(1,2,3) ~ "Basement in residential lot",
                              TRUE ~ "No basement / Non residential lot"))

st_write(tax_lots_flooding, "data/2_intermediate/tax_lots_CD_flood.shp", delete_dsn = TRUE)
rm(tax_lots_flooding)
