# source("src/NYCF_housekeeping_GIS_vars.R")

### download census geoms and variables

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
# get your own census api key here: https://api.census.gov/data/key_signup.html

# load in list of fips codes from the tidycensus package which is only for state and county codes
# data(fips_codes)
# names(fips_codes)

fips_codes_NYS <- filter(fips_codes, state_code == "36")

#total population per NYC county
vi_Total <- c("P009001", "Total")
vi_White <- c("P009005", "White")
vi_Black <- c("P009006", "Black")
vi_Black_two_or_more <- c("P009021", "Black_x2")
vi_Latinx <- c("P009002", "Latinx")
vi_Asian <- c("P009008", "Asian")
vi_malemin5 <- c("P012003", "m_0_5")
vi_femalemin5 <- c("P012027", "f_0_5")

vi_male65.66 <- c("P012020", "m_65_66")
vi_male67.69 <- c("P012021", "m_67_69")
vi_male70.74 <- c("P012022", "m_70_74")
vi_male75.79 <- c("P012023", "m_75_79")
vi_male80.84 <- c("P012024", "m_80_84")
vi_male85 <- c("P012025", "m_85plus")

vi_female65.66 <- c("P012044", "f_65_66")
vi_female67.69 <- c("P012045", "f_67_69")
vi_female70.74 <- c("P012046", "f_70_74")
vi_female75.79 <- c("P012047", "f_75_79")
vi_female80.84 <- c("P012048", "f_80_84")
vi_female85 <- c("P012049", "f_85plus")

variables_query <- list(vi_White,
                        vi_Black,
                        vi_Black_two_or_more,
                        vi_Latinx,
                        vi_Asian,
                        vi_malemin5,
                        vi_femalemin5,
                        vi_male65.66,
                        vi_male67.69,
                        vi_male70.74,
                        vi_male75.79,
                        vi_male80.84,
                        vi_male85,
                        vi_female65.66,
                        vi_female67.69,
                        vi_female70.74,
                        vi_female75.79,
                        vi_female80.84,
                        vi_female85)

NYC_2010 <- get_decennial(geography = "block", variables = vi_Total[1],
                          keep_geo_vars = TRUE,
                          state = "36", county = c("005", "047", "061", "081", "085"), year = 2010,
                          geometry = TRUE) %>% select(-variable, -NAME) %>% rename (!!vi_Total[2] := value)

for(var in variables_query){
  
  print(var[2])
  
  NYC_2010.temp <- get_decennial(geography = "block", variables = var[1],
                                 state = "36", county = c("005", "047", "061", "081", "085"), year = 2010) %>%
    select(-variable, -NAME) %>%
    rename (!!var[2] := value)
  
  NYC_2010 <- left_join(NYC_2010, NYC_2010.temp)
  
}

NYC_2010$b5 <- (NYC_2010$m_0_5 + NYC_2010$f_0_5)
NYC_2010$a65 <- (NYC_2010$m_65_66 +
                   NYC_2010$m_67_69 +
                   NYC_2010$m_70_74 +
                   NYC_2010$m_75_79 +
                   NYC_2010$m_80_84 +
                   NYC_2010$m_85plus +
                   NYC_2010$f_65_66 +
                   NYC_2010$f_67_69 +
                   NYC_2010$f_70_74 +
                   NYC_2010$f_75_79 +
                   NYC_2010$f_80_84 +
                   NYC_2010$f_85plus)

NYC_2010$PCT_b5 <- 100 * (NYC_2010$m_0_5 + NYC_2010$f_0_5) / NYC_2010$Total
NYC_2010$PCT_a65 <- 100 * (NYC_2010$m_65_66 +
                             NYC_2010$m_67_69 +
                             NYC_2010$m_70_74 +
                             NYC_2010$m_75_79 +
                             NYC_2010$m_80_84 +
                             NYC_2010$m_85plus +
                             NYC_2010$f_65_66 +
                             NYC_2010$f_67_69 +
                             NYC_2010$f_70_74 +
                             NYC_2010$f_75_79 +
                             NYC_2010$f_80_84 +
                             NYC_2010$f_85plus) / NYC_2010$Total

NYC_2010$AfAm <- NYC_2010$Black + NYC_2010$Black_x2

NYC_2010$PCT_White <- 100 * (NYC_2010$White) / NYC_2010$Total
NYC_2010$PCT_Afam <- 100 * (NYC_2010$Black + NYC_2010$Black_x2) / NYC_2010$Total
NYC_2010$PCT_Hisp <- 100 * (NYC_2010$Latinx) / NYC_2010$Total
NYC_2010$PC_Asian <- 100 * (NYC_2010$Asian) / NYC_2010$Total

NYC_flooding_blocks <- filter(NYC_2010, ALAND10 > 10) %>%
  st_transform(UTM_18N_meter)

## demographic assessment of flooded census blocks

NYC_flooding_blocks <- write_closest_flooding(NYC_flooding_blocks,
                                              moderate_flooding,
                                              "m_d_f")

NYC_flooding_blocks <- write_closest_flooding(NYC_flooding_blocks,
                                              extreme_flooding,
                                              "e_d_f")


st_write(NYC_flooding_blocks, "data/2_intermediate/NYC_blocks_CD_demo_flood.shp", delete_dsn = TRUE)

NYC_flooding_blocks <- st_read("data/2_intermediate/NYC_blocks_CD_demo_flood.shp")

# ### join NYC blocks to CDs

NYC_flooding_blocks.joined.CDs <- st_join(NYC_flooding_blocks, CD_with_parks, largest = TRUE) %>%
  filter(boro_cd %nin% c(164, 226, 227, 228, 355, 356, 480, 481, 482, 483, 484, 595)) %>%
  drop_na(boro_cd)

st_write(NYC_flooding_blocks.joined.CDs, "data/2_intermediate/NYC_blocks_CD_demo_flood.shp", delete_dsn = TRUE)

rm(fips_codes_NYS,
   variables_query,
   vi_Total,
   vi_White,
   vi_Black,
   vi_Black_two_or_more,
   vi_Latinx,
   vi_Asian,
   vi_malemin5,
   vi_femalemin5,
   vi_male65.66,
   vi_male67.69,
   vi_male70.74, 
   vi_male75.79, 
   vi_male80.84, 
   vi_male85,
   vi_female65.66, 
   vi_female67.69, 
   vi_female70.74, 
   vi_female75.79,  
   vi_female80.84, 
   vi_female85,
   NYC_2010,
   NYC_flooding_blocks)
