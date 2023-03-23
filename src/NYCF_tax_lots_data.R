### tax lots --> calculate min distance to each flooding scenario

tax_lots_flooding <- st_read("data/1_raw/MapPLUTO.shp") %>%
  select(boro_cd = CD, LandUse, BBL, UnitsRes, BsmtCode) %>%
  st_transform(crs = UTM_18N_meter) %>%
  st_make_valid() %>%
  filter(boro_cd %nin% c(0, 164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595))

# shortest distance from tax lots CENTROIDS to flooding

tax_lots_flooding <- write_closest_flooding(tax_lots_flooding,
                                               moderate_flooding,
                                               "m_d_f",
                                               centroids = FALSE)

tax_lots_flooding <- write_closest_flooding(tax_lots_flooding,
                                               extreme_flooding,
                                               "e_d_f",
                                               centroids = FALSE)

st_write(tax_lots_flooding, "data/2_intermediate/tax_lots_CD_flood.shp", delete_dsn = TRUE)

tax_lots_flooding <- st_read("data/2_intermediate/tax_lots_CD_flood.shp")

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
