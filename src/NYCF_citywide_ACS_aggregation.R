# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL AGGREGATE ALL OUR DATA ORIGINALLY LINKED TO THE AMERICAN COMMUNITY SURVEY ESTIMATES. AGGREGATED DATA WILL BE STORED AS VARIABLES TO BE WRITTEN IN THE FINAL TABLE

## this script was created to save space in its parent code, and is meant to be sourced directly from the script "NYCF_report_CD_impacts.R"
## do not run separately - unless you run all lines above the "source(NYCF_citywide_ACS_aggregation)" command in the parent script "NYCF_report_CD_impacts.R" 

## we will aggregate social vulnerability indicators for NYC's total population, as well as for the exposed / unexposed populations under each scenario
## our starting point is the CDs final table dataset - we will sum total values, recomupute standard errors, and recalculate percentages.

NYC.Total_Belpov_e <- sum(final_table[,"Total_Belpov_e"])
NYC.Total_Belpov_s <- (sum(final_table[,"Total_Belpov_s"]^2))^0.5
NYC.Belpov_e <- sum(final_table[,"Belpov_e"])
NYC.Belpov_s <- (sum(final_table[,"Belpov_s"]^2))^0.5

NYC.Total_HH_Income_e <- sum(final_table[,"Total_HH_Income_e"])
NYC.Total_HH_Income_s <- (sum(final_table[,"Total_HH_Income_s"]^2))^0.5
NYC.HH_Income_Bel75K_e <- sum(final_table[,"HH_Income_Bel75K_e"])
NYC.HH_Income_Bel75K_s <- (sum(final_table[,"HH_Income_Bel75K_s"]^2))^0.5

NYC.Total_HH_Dis_e <- sum(final_table[,"Total_HH_Dis_e"])
NYC.Total_HH_Dis_s <- (sum(final_table[,"Total_HH_Dis_s"]^2))^0.5
NYC.HH_Disability_e <- sum(final_table[,"HH_Disability_e"])
NYC.HH_Disability_s <- (sum(final_table[,"HH_Disability_s"]^2))^0.5

NYC.Total_Rent_HH_e <- sum(final_table[,"Total_Rent_HH_e"])
NYC.Total_Rent_HH_s <- (sum(final_table[,"Total_Rent_HH_s"]^2))^0.5
NYC.RB_HH_e <- sum(final_table[,"RB_HH_e"])
NYC.RB_HH_s <- (sum(final_table[,"RB_HH_s"]^2))^0.5

NYC.Total_Owned_HH_e <- sum(final_table[,"Total_Owned_HH_e"])
NYC.Total_Owned_HH_s <- (sum(final_table[,"Total_Owned_HH_s"]^2))^0.5
NYC.CB_HH_e <- sum(final_table[,"CB_HH_e"])
NYC.CB_HH_s <- (sum(final_table[,"CB_HH_s"]^2))^0.5

NYC.PCT_Belpov_e <- 100 * NYC.Belpov_e / NYC.Total_Belpov_e
NYC.PCT_Belpov_s <- 100 * ifelse(NYC.PCT_Belpov_e != 100,
                                 ifelse((NYC.Belpov_s^2 - ((NYC.Belpov_e / NYC.Total_Belpov_e)^2 * NYC.Total_Belpov_s^2)) > 0,
                                        (1 / NYC.Total_Belpov_e) * ((NYC.Belpov_s^2 - ((NYC.Belpov_e / NYC.Total_Belpov_e)^2 * NYC.Total_Belpov_s^2))^0.5),
                                        (1 / NYC.Total_Belpov_e) * ((NYC.Belpov_s^2 + ((NYC.Belpov_e / NYC.Total_Belpov_e)^2 * NYC.Total_Belpov_s^2))^0.5)),
                                 NYC.Belpov_s / NYC.Total_Belpov_e)

NYC.PCT_HH_Income_Bel75K_e <- 100 * NYC.HH_Income_Bel75K_e / NYC.Total_HH_Income_e
NYC.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.PCT_HH_Income_Bel75K_e != 100,
                                           ifelse((NYC.HH_Income_Bel75K_s^2 - ((NYC.HH_Income_Bel75K_e / NYC.Total_HH_Income_e)^2 * NYC.Total_HH_Income_s^2)) > 0,
                                                  (1 / NYC.Total_HH_Income_e) * ((NYC.HH_Income_Bel75K_s^2 - ((NYC.HH_Income_Bel75K_e / NYC.Total_HH_Income_e)^2 * NYC.Total_HH_Income_s^2))^0.5),
                                                  (1 / NYC.Total_HH_Income_e) * ((NYC.HH_Income_Bel75K_s^2 + ((NYC.HH_Income_Bel75K_e / NYC.Total_HH_Income_e)^2 * NYC.Total_HH_Income_s^2))^0.5)),
                                           NYC.HH_Income_Bel75K_s / NYC.Total_HH_Income_e)

NYC.PCT_HH_Disability_e <- 100 * NYC.HH_Disability_e / NYC.Total_HH_Dis_e
NYC.PCT_HH_Disability_s <- 100 * ifelse(NYC.PCT_HH_Disability_e != 100,
                                        ifelse((NYC.HH_Disability_s^2 - ((NYC.HH_Disability_e / NYC.Total_HH_Dis_e)^2 * NYC.Total_HH_Dis_s^2)) > 0,
                                               (1 / NYC.Total_HH_Dis_e) * ((NYC.HH_Disability_s^2 - ((NYC.HH_Disability_e / NYC.Total_HH_Dis_e)^2 * NYC.Total_HH_Dis_s^2))^0.5),
                                               (1 / NYC.Total_HH_Dis_e) * ((NYC.HH_Disability_s^2 + ((NYC.HH_Disability_e / NYC.Total_HH_Dis_e)^2 * NYC.Total_HH_Dis_s^2))^0.5)),
                                        NYC.HH_Disability_s / NYC.Total_HH_Dis_e)

NYC.PCT_RB_e <- 100 * NYC.RB_HH_e / NYC.Total_Rent_HH_e
NYC.PCT_RB_s <- 100 * ifelse(NYC.PCT_RB_e != 100,
                             ifelse((NYC.RB_HH_s^2 - ((NYC.RB_HH_e / NYC.Total_Rent_HH_e)^2 * NYC.Total_Rent_HH_s^2)) > 0,
                                    (1 / NYC.Total_Rent_HH_e) * ((NYC.RB_HH_s^2 - ((NYC.RB_HH_e / NYC.Total_Rent_HH_e)^2 * NYC.Total_Rent_HH_s^2))^0.5),
                                    (1 / NYC.Total_Rent_HH_e) * ((NYC.RB_HH_s^2 + ((NYC.RB_HH_e / NYC.Total_Rent_HH_e)^2 * NYC.Total_Rent_HH_s^2))^0.5)),
                             NYC.RB_HH_s / NYC.Total_Rent_HH_e)

NYC.PCT_CB_e <- 100 * NYC.CB_HH_e / NYC.Total_Owned_HH_e
NYC.PCT_CB_s <- 100 * ifelse(NYC.PCT_CB_e != 100,
                             ifelse((NYC.CB_HH_s^2 - ((NYC.CB_HH_e / NYC.Total_Owned_HH_e)^2 * NYC.Total_Owned_HH_s^2)) > 0,
                                    (1 / NYC.Total_Owned_HH_e) * ((NYC.CB_HH_s^2 - ((NYC.CB_HH_e / NYC.Total_Owned_HH_e)^2 * NYC.Total_Owned_HH_s^2))^0.5),
                                    (1 / NYC.Total_Owned_HH_e) * ((NYC.CB_HH_s^2 + ((NYC.CB_HH_e / NYC.Total_Owned_HH_e)^2 * NYC.Total_Owned_HH_s^2))^0.5)),
                             NYC.CB_HH_s / NYC.Total_Owned_HH_e)

## Moderate CityWide 

NYC.M.Total_Belpov_e <- sum(final_table[,"M.Total_Belpov_e"])
NYC.M.Total_Belpov_s <- (sum(final_table[,"M.Total_Belpov_s"]^2))^0.5
NYC.M.Belpov_e <- sum(final_table[,"M.Belpov_e"])
NYC.M.Belpov_s <- (sum(final_table[,"M.Belpov_s"]^2))^0.5

NYC.M.Total_HH_Income_e <- sum(final_table[,"M.Total_HH_Income_e"])
NYC.M.Total_HH_Income_s <- (sum(final_table[,"M.Total_HH_Income_s"]^2))^0.5
NYC.M.HH_Income_Bel75K_e <- sum(final_table[,"M.HH_Income_Bel75K_e"])
NYC.M.HH_Income_Bel75K_s <- (sum(final_table[,"M.HH_Income_Bel75K_s"]^2))^0.5

NYC.M.Total_HH_Dis_e <- sum(final_table[,"M.Total_HH_Dis_e"])
NYC.M.Total_HH_Dis_s <- (sum(final_table[,"M.Total_HH_Dis_s"]^2))^0.5
NYC.M.HH_Disability_e <- sum(final_table[,"M.HH_Disability_e"])
NYC.M.HH_Disability_s <- (sum(final_table[,"M.HH_Disability_s"]^2))^0.5

NYC.M.Total_Rent_HH_e <- sum(final_table[,"M.Total_Rent_HH_e"])
NYC.M.Total_Rent_HH_s <- (sum(final_table[,"M.Total_Rent_HH_s"]^2))^0.5
NYC.M.RB_HH_e <- sum(final_table[,"M.RB_HH_e"])
NYC.M.RB_HH_s <- (sum(final_table[,"M.RB_HH_s"]^2))^0.5

NYC.M.Total_Owned_HH_e <- sum(final_table[,"M.Total_Owned_HH_e"])
NYC.M.Total_Owned_HH_s <- (sum(final_table[,"M.Total_Owned_HH_s"]^2))^0.5
NYC.M.CB_HH_e <- sum(final_table[,"M.CB_HH_e"])
NYC.M.CB_HH_s <- (sum(final_table[,"M.CB_HH_s"]^2))^0.5

NYC.M.PCT_Belpov_e <- 100 * NYC.M.Belpov_e / NYC.M.Total_Belpov_e
NYC.M.PCT_Belpov_s <- 100 * ifelse(NYC.M.PCT_Belpov_e != 100,
                                   ifelse((NYC.M.Belpov_s^2 - ((NYC.M.Belpov_e / NYC.M.Total_Belpov_e)^2 * NYC.M.Total_Belpov_s^2)) > 0,
                                          (1 / NYC.M.Total_Belpov_e) * ((NYC.M.Belpov_s^2 - ((NYC.M.Belpov_e / NYC.M.Total_Belpov_e)^2 * NYC.M.Total_Belpov_s^2))^0.5),
                                          (1 / NYC.M.Total_Belpov_e) * ((NYC.M.Belpov_s^2 + ((NYC.M.Belpov_e / NYC.M.Total_Belpov_e)^2 * NYC.M.Total_Belpov_s^2))^0.5)),
                                   NYC.M.Belpov_s / NYC.M.Total_Belpov_e)

NYC.M.PCT_HH_Income_Bel75K_e <- 100 * NYC.M.HH_Income_Bel75K_e / NYC.M.Total_HH_Income_e
NYC.M.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.M.PCT_HH_Income_Bel75K_e != 100,
                                             ifelse((NYC.M.HH_Income_Bel75K_s^2 - ((NYC.M.HH_Income_Bel75K_e / NYC.M.Total_HH_Income_e)^2 * NYC.M.Total_HH_Income_s^2)) > 0,
                                                    (1 / NYC.M.Total_HH_Income_e) * ((NYC.M.HH_Income_Bel75K_s^2 - ((NYC.M.HH_Income_Bel75K_e / NYC.M.Total_HH_Income_e)^2 * NYC.M.Total_HH_Income_s^2))^0.5),
                                                    (1 / NYC.M.Total_HH_Income_e) * ((NYC.M.HH_Income_Bel75K_s^2 + ((NYC.M.HH_Income_Bel75K_e / NYC.M.Total_HH_Income_e)^2 * NYC.M.Total_HH_Income_s^2))^0.5)),
                                             NYC.M.HH_Income_Bel75K_s / NYC.M.Total_HH_Income_e)

NYC.M.PCT_HH_Disability_e <- 100 * NYC.M.HH_Disability_e / NYC.M.Total_HH_Dis_e
NYC.M.PCT_HH_Disability_s <- 100 * ifelse(NYC.M.PCT_HH_Disability_e != 100,
                                          ifelse((NYC.M.HH_Disability_s^2 - ((NYC.M.HH_Disability_e / NYC.M.Total_HH_Dis_e)^2 * NYC.M.Total_HH_Dis_s^2)) > 0,
                                                 (1 / NYC.M.Total_HH_Dis_e) * ((NYC.M.HH_Disability_s^2 - ((NYC.M.HH_Disability_e / NYC.M.Total_HH_Dis_e)^2 * NYC.M.Total_HH_Dis_s^2))^0.5),
                                                 (1 / NYC.M.Total_HH_Dis_e) * ((NYC.M.HH_Disability_s^2 + ((NYC.M.HH_Disability_e / NYC.M.Total_HH_Dis_e)^2 * NYC.M.Total_HH_Dis_s^2))^0.5)),
                                          NYC.M.HH_Disability_s / NYC.M.Total_HH_Dis_e)

NYC.M.PCT_RB_e <- 100 * NYC.M.RB_HH_e / NYC.M.Total_Rent_HH_e
NYC.M.PCT_RB_s <- 100 * ifelse(NYC.M.PCT_RB_e != 100,
                               ifelse((NYC.M.RB_HH_s^2 - ((NYC.M.RB_HH_e / NYC.M.Total_Rent_HH_e)^2 * NYC.M.Total_Rent_HH_s^2)) > 0,
                                      (1 / NYC.M.Total_Rent_HH_e) * ((NYC.M.RB_HH_s^2 - ((NYC.M.RB_HH_e / NYC.M.Total_Rent_HH_e)^2 * NYC.M.Total_Rent_HH_s^2))^0.5),
                                      (1 / NYC.M.Total_Rent_HH_e) * ((NYC.M.RB_HH_s^2 + ((NYC.M.RB_HH_e / NYC.M.Total_Rent_HH_e)^2 * NYC.M.Total_Rent_HH_s^2))^0.5)),
                               NYC.M.RB_HH_s / NYC.M.Total_Rent_HH_e)

NYC.M.PCT_CB_e <- 100 * NYC.M.CB_HH_e / NYC.M.Total_Owned_HH_e
NYC.M.PCT_CB_s <- 100 * ifelse(NYC.M.PCT_CB_e != 100,
                               ifelse((NYC.M.CB_HH_s^2 - ((NYC.M.CB_HH_e / NYC.M.Total_Owned_HH_e)^2 * NYC.M.Total_Owned_HH_s^2)) > 0,
                                      (1 / NYC.M.Total_Owned_HH_e) * ((NYC.M.CB_HH_s^2 - ((NYC.M.CB_HH_e / NYC.M.Total_Owned_HH_e)^2 * NYC.M.Total_Owned_HH_s^2))^0.5),
                                      (1 / NYC.M.Total_Owned_HH_e) * ((NYC.M.CB_HH_s^2 + ((NYC.M.CB_HH_e / NYC.M.Total_Owned_HH_e)^2 * NYC.M.Total_Owned_HH_s^2))^0.5)),
                               NYC.M.CB_HH_s / NYC.M.Total_Owned_HH_e)

## Moderate CityWide Unexposed

NYC.M.N.Total_Belpov_e <- sum(final_table[,"M.N.Total_Belpov_e"])
NYC.M.N.Total_Belpov_s <- (sum(final_table[,"M.N.Total_Belpov_s"]^2))^0.5
NYC.M.N.Belpov_e <- sum(final_table[,"M.N.Belpov_e"])
NYC.M.N.Belpov_s <- (sum(final_table[,"M.N.Belpov_s"]^2))^0.5

NYC.M.N.Total_HH_Income_e <- sum(final_table[,"M.N.Total_HH_Income_e"])
NYC.M.N.Total_HH_Income_s <- (sum(final_table[,"M.N.Total_HH_Income_s"]^2))^0.5
NYC.M.N.HH_Income_Bel75K_e <- sum(final_table[,"M.N.HH_Income_Bel75K_e"])
NYC.M.N.HH_Income_Bel75K_s <- (sum(final_table[,"M.N.HH_Income_Bel75K_s"]^2))^0.5

NYC.M.N.Total_HH_Dis_e <- sum(final_table[,"M.N.Total_HH_Dis_e"])
NYC.M.N.Total_HH_Dis_s <- (sum(final_table[,"M.N.Total_HH_Dis_s"]^2))^0.5
NYC.M.N.HH_Disability_e <- sum(final_table[,"M.N.HH_Disability_e"])
NYC.M.N.HH_Disability_s <- (sum(final_table[,"M.N.HH_Disability_s"]^2))^0.5

NYC.M.N.Total_Rent_HH_e <- sum(final_table[,"M.N.Total_Rent_HH_e"])
NYC.M.N.Total_Rent_HH_s <- (sum(final_table[,"M.N.Total_Rent_HH_s"]^2))^0.5
NYC.M.N.RB_HH_e <- sum(final_table[,"M.N.RB_HH_e"])
NYC.M.N.RB_HH_s <- (sum(final_table[,"M.N.RB_HH_s"]^2))^0.5

NYC.M.N.Total_Owned_HH_e <- sum(final_table[,"M.N.Total_Owned_HH_e"])
NYC.M.N.Total_Owned_HH_s <- (sum(final_table[,"M.N.Total_Owned_HH_s"]^2))^0.5
NYC.M.N.CB_HH_e <- sum(final_table[,"M.N.CB_HH_e"])
NYC.M.N.CB_HH_s <- (sum(final_table[,"M.N.CB_HH_s"]^2))^0.5

NYC.M.N.PCT_Belpov_e <- 100 * NYC.M.N.Belpov_e / NYC.M.N.Total_Belpov_e
NYC.M.N.PCT_Belpov_s <- 100 * ifelse(NYC.M.N.PCT_Belpov_e != 100,
                                     ifelse((NYC.M.N.Belpov_s^2 - ((NYC.M.N.Belpov_e / NYC.M.N.Total_Belpov_e)^2 * NYC.M.N.Total_Belpov_s^2)) > 0,
                                            (1 / NYC.M.N.Total_Belpov_e) * ((NYC.M.N.Belpov_s^2 - ((NYC.M.N.Belpov_e / NYC.M.N.Total_Belpov_e)^2 * NYC.M.N.Total_Belpov_s^2))^0.5),
                                            (1 / NYC.M.N.Total_Belpov_e) * ((NYC.M.N.Belpov_s^2 + ((NYC.M.N.Belpov_e / NYC.M.N.Total_Belpov_e)^2 * NYC.M.N.Total_Belpov_s^2))^0.5)),
                                     NYC.M.N.Belpov_s / NYC.M.N.Total_Belpov_e)

NYC.M.N.PCT_HH_Income_Bel75K_e <- 100 * NYC.M.N.HH_Income_Bel75K_e / NYC.M.N.Total_HH_Income_e
NYC.M.N.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.M.N.PCT_HH_Income_Bel75K_e != 100,
                                               ifelse((NYC.M.N.HH_Income_Bel75K_s^2 - ((NYC.M.N.HH_Income_Bel75K_e / NYC.M.N.Total_HH_Income_e)^2 * NYC.M.N.Total_HH_Income_s^2)) > 0,
                                                      (1 / NYC.M.N.Total_HH_Income_e) * ((NYC.M.N.HH_Income_Bel75K_s^2 - ((NYC.M.N.HH_Income_Bel75K_e / NYC.M.N.Total_HH_Income_e)^2 * NYC.M.N.Total_HH_Income_s^2))^0.5),
                                                      (1 / NYC.M.N.Total_HH_Income_e) * ((NYC.M.N.HH_Income_Bel75K_s^2 + ((NYC.M.N.HH_Income_Bel75K_e / NYC.M.N.Total_HH_Income_e)^2 * NYC.M.N.Total_HH_Income_s^2))^0.5)),
                                               NYC.M.N.HH_Income_Bel75K_s / NYC.M.N.Total_HH_Income_e)

NYC.M.N.PCT_HH_Disability_e <- 100 * NYC.M.N.HH_Disability_e / NYC.M.N.Total_HH_Dis_e
NYC.M.N.PCT_HH_Disability_s <- 100 * ifelse(NYC.M.N.PCT_HH_Disability_e != 100,
                                            ifelse((NYC.M.N.HH_Disability_s^2 - ((NYC.M.N.HH_Disability_e / NYC.M.N.Total_HH_Dis_e)^2 * NYC.M.N.Total_HH_Dis_s^2)) > 0,
                                                   (1 / NYC.M.N.Total_HH_Dis_e) * ((NYC.M.N.HH_Disability_s^2 - ((NYC.M.N.HH_Disability_e / NYC.M.N.Total_HH_Dis_e)^2 * NYC.M.N.Total_HH_Dis_s^2))^0.5),
                                                   (1 / NYC.M.N.Total_HH_Dis_e) * ((NYC.M.N.HH_Disability_s^2 + ((NYC.M.N.HH_Disability_e / NYC.M.N.Total_HH_Dis_e)^2 * NYC.M.N.Total_HH_Dis_s^2))^0.5)),
                                            NYC.M.N.HH_Disability_s / NYC.M.N.Total_HH_Dis_e)

NYC.M.N.PCT_RB_e <- 100 * NYC.M.N.RB_HH_e / NYC.M.N.Total_Rent_HH_e
NYC.M.N.PCT_RB_s <- 100 * ifelse(NYC.M.N.PCT_RB_e != 100,
                                 ifelse((NYC.M.N.RB_HH_s^2 - ((NYC.M.N.RB_HH_e / NYC.M.N.Total_Rent_HH_e)^2 * NYC.M.N.Total_Rent_HH_s^2)) > 0,
                                        (1 / NYC.M.N.Total_Rent_HH_e) * ((NYC.M.N.RB_HH_s^2 - ((NYC.M.N.RB_HH_e / NYC.M.N.Total_Rent_HH_e)^2 * NYC.M.N.Total_Rent_HH_s^2))^0.5),
                                        (1 / NYC.M.N.Total_Rent_HH_e) * ((NYC.M.N.RB_HH_s^2 + ((NYC.M.N.RB_HH_e / NYC.M.N.Total_Rent_HH_e)^2 * NYC.M.N.Total_Rent_HH_s^2))^0.5)),
                                 NYC.M.N.RB_HH_s / NYC.M.N.Total_Rent_HH_e)

NYC.M.N.PCT_CB_e <- 100 * NYC.M.N.CB_HH_e / NYC.M.N.Total_Owned_HH_e
NYC.M.N.PCT_CB_s <- 100 * ifelse(NYC.M.N.PCT_CB_e != 100,
                                 ifelse((NYC.M.N.CB_HH_s^2 - ((NYC.M.N.CB_HH_e / NYC.M.N.Total_Owned_HH_e)^2 * NYC.M.N.Total_Owned_HH_s^2)) > 0,
                                        (1 / NYC.M.N.Total_Owned_HH_e) * ((NYC.M.N.CB_HH_s^2 - ((NYC.M.N.CB_HH_e / NYC.M.N.Total_Owned_HH_e)^2 * NYC.M.N.Total_Owned_HH_s^2))^0.5),
                                        (1 / NYC.M.N.Total_Owned_HH_e) * ((NYC.M.N.CB_HH_s^2 + ((NYC.M.N.CB_HH_e / NYC.M.N.Total_Owned_HH_e)^2 * NYC.M.N.Total_Owned_HH_s^2))^0.5)),
                                 NYC.M.N.CB_HH_s / NYC.M.N.Total_Owned_HH_e)

## Extreme CityWide

NYC.E.Total_Belpov_e <- sum(final_table[,"E.Total_Belpov_e"])
NYC.E.Total_Belpov_s <- (sum(final_table[,"E.Total_Belpov_s"]^2))^0.5
NYC.E.Belpov_e <- sum(final_table[,"E.Belpov_e"])
NYC.E.Belpov_s <- (sum(final_table[,"E.Belpov_s"]^2))^0.5

NYC.E.Total_HH_Income_e <- sum(final_table[,"E.Total_HH_Income_e"])
NYC.E.Total_HH_Income_s <- (sum(final_table[,"E.Total_HH_Income_s"]^2))^0.5
NYC.E.HH_Income_Bel75K_e <- sum(final_table[,"E.HH_Income_Bel75K_e"])
NYC.E.HH_Income_Bel75K_s <- (sum(final_table[,"E.HH_Income_Bel75K_s"]^2))^0.5

NYC.E.Total_HH_Dis_e <- sum(final_table[,"E.Total_HH_Dis_e"])
NYC.E.Total_HH_Dis_s <- (sum(final_table[,"E.Total_HH_Dis_s"]^2))^0.5
NYC.E.HH_Disability_e <- sum(final_table[,"E.HH_Disability_e"])
NYC.E.HH_Disability_s <- (sum(final_table[,"E.HH_Disability_s"]^2))^0.5

NYC.E.Total_Rent_HH_e <- sum(final_table[,"E.Total_Rent_HH_e"])
NYC.E.Total_Rent_HH_s <- (sum(final_table[,"E.Total_Rent_HH_s"]^2))^0.5
NYC.E.RB_HH_e <- sum(final_table[,"E.RB_HH_e"])
NYC.E.RB_HH_s <- (sum(final_table[,"E.RB_HH_s"]^2))^0.5

NYC.E.Total_Owned_HH_e <- sum(final_table[,"E.Total_Owned_HH_e"])
NYC.E.Total_Owned_HH_s <- (sum(final_table[,"E.Total_Owned_HH_s"]^2))^0.5
NYC.E.CB_HH_e <- sum(final_table[,"E.CB_HH_e"])
NYC.E.CB_HH_s <- (sum(final_table[,"E.CB_HH_s"]^2))^0.5

NYC.E.PCT_Belpov_e <- 100 * NYC.E.Belpov_e / NYC.E.Total_Belpov_e
NYC.E.PCT_Belpov_s <- 100 * ifelse(NYC.E.PCT_Belpov_e != 100,
                                   ifelse((NYC.E.Belpov_s^2 - ((NYC.E.Belpov_e / NYC.E.Total_Belpov_e)^2 * NYC.E.Total_Belpov_s^2)) > 0,
                                          (1 / NYC.E.Total_Belpov_e) * ((NYC.E.Belpov_s^2 - ((NYC.E.Belpov_e / NYC.E.Total_Belpov_e)^2 * NYC.E.Total_Belpov_s^2))^0.5),
                                          (1 / NYC.E.Total_Belpov_e) * ((NYC.E.Belpov_s^2 + ((NYC.E.Belpov_e / NYC.E.Total_Belpov_e)^2 * NYC.E.Total_Belpov_s^2))^0.5)),
                                   NYC.E.Belpov_s / NYC.E.Total_Belpov_e)

NYC.E.PCT_HH_Income_Bel75K_e <- 100 * NYC.E.HH_Income_Bel75K_e / NYC.E.Total_HH_Income_e
NYC.E.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.E.PCT_HH_Income_Bel75K_e != 100,
                                             ifelse((NYC.E.HH_Income_Bel75K_s^2 - ((NYC.E.HH_Income_Bel75K_e / NYC.E.Total_HH_Income_e)^2 * NYC.E.Total_HH_Income_s^2)) > 0,
                                                    (1 / NYC.E.Total_HH_Income_e) * ((NYC.E.HH_Income_Bel75K_s^2 - ((NYC.E.HH_Income_Bel75K_e / NYC.E.Total_HH_Income_e)^2 * NYC.E.Total_HH_Income_s^2))^0.5),
                                                    (1 / NYC.E.Total_HH_Income_e) * ((NYC.E.HH_Income_Bel75K_s^2 + ((NYC.E.HH_Income_Bel75K_e / NYC.E.Total_HH_Income_e)^2 * NYC.E.Total_HH_Income_s^2))^0.5)),
                                             NYC.E.HH_Income_Bel75K_s / NYC.E.Total_HH_Income_e)

NYC.E.PCT_HH_Disability_e <- 100 * NYC.E.HH_Disability_e / NYC.E.Total_HH_Dis_e
NYC.E.PCT_HH_Disability_s <- 100 * ifelse(NYC.E.PCT_HH_Disability_e != 100,
                                          ifelse((NYC.E.HH_Disability_s^2 - ((NYC.E.HH_Disability_e / NYC.E.Total_HH_Dis_e)^2 * NYC.E.Total_HH_Dis_s^2)) > 0,
                                                 (1 / NYC.E.Total_HH_Dis_e) * ((NYC.E.HH_Disability_s^2 - ((NYC.E.HH_Disability_e / NYC.E.Total_HH_Dis_e)^2 * NYC.E.Total_HH_Dis_s^2))^0.5),
                                                 (1 / NYC.E.Total_HH_Dis_e) * ((NYC.E.HH_Disability_s^2 + ((NYC.E.HH_Disability_e / NYC.E.Total_HH_Dis_e)^2 * NYC.E.Total_HH_Dis_s^2))^0.5)),
                                          NYC.E.HH_Disability_s / NYC.E.Total_HH_Dis_e)

NYC.E.PCT_RB_e <- 100 * NYC.E.RB_HH_e / NYC.E.Total_Rent_HH_e
NYC.E.PCT_RB_s <- 100 * ifelse(NYC.E.PCT_RB_e != 100,
                               ifelse((NYC.E.RB_HH_s^2 - ((NYC.E.RB_HH_e / NYC.E.Total_Rent_HH_e)^2 * NYC.E.Total_Rent_HH_s^2)) > 0,
                                      (1 / NYC.E.Total_Rent_HH_e) * ((NYC.E.RB_HH_s^2 - ((NYC.E.RB_HH_e / NYC.E.Total_Rent_HH_e)^2 * NYC.E.Total_Rent_HH_s^2))^0.5),
                                      (1 / NYC.E.Total_Rent_HH_e) * ((NYC.E.RB_HH_s^2 + ((NYC.E.RB_HH_e / NYC.E.Total_Rent_HH_e)^2 * NYC.E.Total_Rent_HH_s^2))^0.5)),
                               NYC.E.RB_HH_s / NYC.E.Total_Rent_HH_e)

NYC.E.PCT_CB_e <- 100 * NYC.E.CB_HH_e / NYC.E.Total_Owned_HH_e
NYC.E.PCT_CB_s <- 100 * ifelse(NYC.E.PCT_CB_e != 100,
                               ifelse((NYC.E.CB_HH_s^2 - ((NYC.E.CB_HH_e / NYC.E.Total_Owned_HH_e)^2 * NYC.E.Total_Owned_HH_s^2)) > 0,
                                      (1 / NYC.E.Total_Owned_HH_e) * ((NYC.E.CB_HH_s^2 - ((NYC.E.CB_HH_e / NYC.E.Total_Owned_HH_e)^2 * NYC.E.Total_Owned_HH_s^2))^0.5),
                                      (1 / NYC.E.Total_Owned_HH_e) * ((NYC.E.CB_HH_s^2 + ((NYC.E.CB_HH_e / NYC.E.Total_Owned_HH_e)^2 * NYC.E.Total_Owned_HH_s^2))^0.5)),
                               NYC.E.CB_HH_s / NYC.E.Total_Owned_HH_e)

## Extreme Citywide Unexposed

NYC.E.N.Total_Belpov_e <- sum(final_table[,"E.N.Total_Belpov_e"])
NYC.E.N.Total_Belpov_s <- (sum(final_table[,"E.N.Total_Belpov_s"]^2))^0.5
NYC.E.N.Belpov_e <- sum(final_table[,"E.N.Belpov_e"])
NYC.E.N.Belpov_s <- (sum(final_table[,"E.N.Belpov_s"]^2))^0.5

NYC.E.N.Total_HH_Income_e <- sum(final_table[,"E.N.Total_HH_Income_e"])
NYC.E.N.Total_HH_Income_s <- (sum(final_table[,"E.N.Total_HH_Income_s"]^2))^0.5
NYC.E.N.HH_Income_Bel75K_e <- sum(final_table[,"E.N.HH_Income_Bel75K_e"])
NYC.E.N.HH_Income_Bel75K_s <- (sum(final_table[,"E.N.HH_Income_Bel75K_s"]^2))^0.5

NYC.E.N.Total_HH_Dis_e <- sum(final_table[,"E.N.Total_HH_Dis_e"])
NYC.E.N.Total_HH_Dis_s <- (sum(final_table[,"E.N.Total_HH_Dis_s"]^2))^0.5
NYC.E.N.HH_Disability_e <- sum(final_table[,"E.N.HH_Disability_e"])
NYC.E.N.HH_Disability_s <- (sum(final_table[,"E.N.HH_Disability_s"]^2))^0.5

NYC.E.N.Total_Rent_HH_e <- sum(final_table[,"E.N.Total_Rent_HH_e"])
NYC.E.N.Total_Rent_HH_s <- (sum(final_table[,"E.N.Total_Rent_HH_s"]^2))^0.5
NYC.E.N.RB_HH_e <- sum(final_table[,"E.N.RB_HH_e"])
NYC.E.N.RB_HH_s <- (sum(final_table[,"E.N.RB_HH_s"]^2))^0.5

NYC.E.N.Total_Owned_HH_e <- sum(final_table[,"E.N.Total_Owned_HH_e"])
NYC.E.N.Total_Owned_HH_s <- (sum(final_table[,"E.N.Total_Owned_HH_s"]^2))^0.5
NYC.E.N.CB_HH_e <- sum(final_table[,"E.N.CB_HH_e"])
NYC.E.N.CB_HH_s <- (sum(final_table[,"E.N.CB_HH_s"]^2))^0.5

NYC.E.N.PCT_Belpov_e <- 100 * NYC.E.N.Belpov_e / NYC.E.N.Total_Belpov_e
NYC.E.N.PCT_Belpov_s <- 100 * ifelse(NYC.E.N.PCT_Belpov_e != 100,
                                     ifelse((NYC.E.N.Belpov_s^2 - ((NYC.E.N.Belpov_e / NYC.E.N.Total_Belpov_e)^2 * NYC.E.N.Total_Belpov_s^2)) > 0,
                                            (1 / NYC.E.N.Total_Belpov_e) * ((NYC.E.N.Belpov_s^2 - ((NYC.E.N.Belpov_e / NYC.E.N.Total_Belpov_e)^2 * NYC.E.N.Total_Belpov_s^2))^0.5),
                                            (1 / NYC.E.N.Total_Belpov_e) * ((NYC.E.N.Belpov_s^2 + ((NYC.E.N.Belpov_e / NYC.E.N.Total_Belpov_e)^2 * NYC.E.N.Total_Belpov_s^2))^0.5)),
                                     NYC.E.N.Belpov_s / NYC.E.N.Total_Belpov_e)

NYC.E.N.PCT_HH_Income_Bel75K_e <- 100 * NYC.E.N.HH_Income_Bel75K_e / NYC.E.N.Total_HH_Income_e
NYC.E.N.PCT_HH_Income_Bel75K_s <- 100 * ifelse(NYC.E.N.PCT_HH_Income_Bel75K_e != 100,
                                               ifelse((NYC.E.N.HH_Income_Bel75K_s^2 - ((NYC.E.N.HH_Income_Bel75K_e / NYC.E.N.Total_HH_Income_e)^2 * NYC.E.N.Total_HH_Income_s^2)) > 0,
                                                      (1 / NYC.E.N.Total_HH_Income_e) * ((NYC.E.N.HH_Income_Bel75K_s^2 - ((NYC.E.N.HH_Income_Bel75K_e / NYC.E.N.Total_HH_Income_e)^2 * NYC.E.N.Total_HH_Income_s^2))^0.5),
                                                      (1 / NYC.E.N.Total_HH_Income_e) * ((NYC.E.N.HH_Income_Bel75K_s^2 + ((NYC.E.N.HH_Income_Bel75K_e / NYC.E.N.Total_HH_Income_e)^2 * NYC.E.N.Total_HH_Income_s^2))^0.5)),
                                               NYC.E.N.HH_Income_Bel75K_s / NYC.E.N.Total_HH_Income_e)

NYC.E.N.PCT_HH_Disability_e <- 100 * NYC.E.N.HH_Disability_e / NYC.E.N.Total_HH_Dis_e
NYC.E.N.PCT_HH_Disability_s <- 100 * ifelse(NYC.E.N.PCT_HH_Disability_e != 100,
                                            ifelse((NYC.E.N.HH_Disability_s^2 - ((NYC.E.N.HH_Disability_e / NYC.E.N.Total_HH_Dis_e)^2 * NYC.E.N.Total_HH_Dis_s^2)) > 0,
                                                   (1 / NYC.E.N.Total_HH_Dis_e) * ((NYC.E.N.HH_Disability_s^2 - ((NYC.E.N.HH_Disability_e / NYC.E.N.Total_HH_Dis_e)^2 * NYC.E.N.Total_HH_Dis_s^2))^0.5),
                                                   (1 / NYC.E.N.Total_HH_Dis_e) * ((NYC.E.N.HH_Disability_s^2 + ((NYC.E.N.HH_Disability_e / NYC.E.N.Total_HH_Dis_e)^2 * NYC.E.N.Total_HH_Dis_s^2))^0.5)),
                                            NYC.E.N.HH_Disability_s / NYC.E.N.Total_HH_Dis_e)

NYC.E.N.PCT_RB_e <- 100 * NYC.E.N.RB_HH_e / NYC.E.N.Total_Rent_HH_e
NYC.E.N.PCT_RB_s <- 100 * ifelse(NYC.E.N.PCT_RB_e != 100,
                                 ifelse((NYC.E.N.RB_HH_s^2 - ((NYC.E.N.RB_HH_e / NYC.E.N.Total_Rent_HH_e)^2 * NYC.E.N.Total_Rent_HH_s^2)) > 0,
                                        (1 / NYC.E.N.Total_Rent_HH_e) * ((NYC.E.N.RB_HH_s^2 - ((NYC.E.N.RB_HH_e / NYC.E.N.Total_Rent_HH_e)^2 * NYC.E.N.Total_Rent_HH_s^2))^0.5),
                                        (1 / NYC.E.N.Total_Rent_HH_e) * ((NYC.E.N.RB_HH_s^2 + ((NYC.E.N.RB_HH_e / NYC.E.N.Total_Rent_HH_e)^2 * NYC.E.N.Total_Rent_HH_s^2))^0.5)),
                                 NYC.E.N.RB_HH_s / NYC.E.N.Total_Rent_HH_e)

NYC.E.N.PCT_CB_e <- 100 * NYC.E.N.CB_HH_e / NYC.E.N.Total_Owned_HH_e
NYC.E.N.PCT_CB_s <- 100 * ifelse(NYC.E.N.PCT_CB_e != 100,
                                 ifelse((NYC.E.N.CB_HH_s^2 - ((NYC.E.N.CB_HH_e / NYC.E.N.Total_Owned_HH_e)^2 * NYC.E.N.Total_Owned_HH_s^2)) > 0,
                                        (1 / NYC.E.N.Total_Owned_HH_e) * ((NYC.E.N.CB_HH_s^2 - ((NYC.E.N.CB_HH_e / NYC.E.N.Total_Owned_HH_e)^2 * NYC.E.N.Total_Owned_HH_s^2))^0.5),
                                        (1 / NYC.E.N.Total_Owned_HH_e) * ((NYC.E.N.CB_HH_s^2 + ((NYC.E.N.CB_HH_e / NYC.E.N.Total_Owned_HH_e)^2 * NYC.E.N.Total_Owned_HH_s^2))^0.5)),
                                 NYC.E.N.CB_HH_s / NYC.E.N.Total_Owned_HH_e)