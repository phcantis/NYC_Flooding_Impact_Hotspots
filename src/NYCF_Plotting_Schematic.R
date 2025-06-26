# Pluvial Flood Risk and Critical Infrastructures Exposure in New York City

# DATE: JUNE 2025
# AUTHOR: TO BE DISCLOSED UPON ACCEPTANCE OF MANUSCRIPT
# GOAL: IN THIS SCRIPT, WE WILL GENERATE BAR PLOTS THAT SHOW DIFFERENCES IN THE IMPACTS CAUSED BY THE TWO DIFFERENT SCENARIOS CONSIDERED

## we begin loading the libraries, functions, and variables that we will recurrently use

source("src/NYCF_housekeeping_GIS_vars.R")

## and we bring in the processed database which we will use to pull our data

data_plot_NYC <- read_csv("DATA/3_output/stormwater_analysis_final_database.csv") %>%
  filter(Geography == "NYC")

data_plot_NYC_bars <- data.frame(Scenario=c("Moderate", "Extreme"), 
                                 PCT_area=c(data_plot_NYC$M.PCT_area, data_plot_NYC$E.PCT_area), 
                                 Area=c(data_plot_NYC$M.Total_area, data_plot_NYC$E.Total_area), 
                                 PCT_Total_population=c(data_plot_NYC$M.PCT_Total_population, data_plot_NYC$E.PCT_Total_population), 
                                 Total_population=c(data_plot_NYC$M.Total_population, data_plot_NYC$E.Total_population))

data_plot_NYC_bars$Scenario <- factor(data_plot_NYC_bars$Scenario, levels = c("Extreme", "Moderate"))

plot.P_Area_Flooded <- ggplot(data_plot_NYC_bars, aes(fill = Scenario, y = PCT_area, x = Scenario)) +
  geom_bar(position="dodge", stat="identity", show.legend = FALSE, width = 0.7) +
  geom_text(aes(label=paste0(round((Area/1000000),1), "sqkm")), size = 7, position=position_dodge(width=0.9), hjust=-.1) +
  scale_fill_manual(values=c('#004466','#3485AD', "#777678")) + 
  labs( y = "% Total Area Flooded") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18),
        plot.margin = margin(1, 3.5, 1, 1, "cm"),
        aspect.ratio = 1/2) +
  coord_flip(clip = "off")

ggsave(plot.P_Area_Flooded, filename = paste0("data/4_display/bars/Citywide_barchart_Area.png"),
       dpi = 400,
       width = 10,
       height = 10,
       units = "in",
       bg = "transparent")

plot.P_TotPop_Flooded <- ggplot(data_plot_NYC_bars, aes(fill = Scenario, y = PCT_Total_population, x = Scenario)) +
  geom_bar(position="dodge", stat="identity", show.legend = FALSE, width = 0.7) +
  geom_text(aes(label=Total_population), size = 7, position=position_dodge(width=0.9), hjust=-.1) +
  scale_fill_manual(values=c('#004466', '#3485AD',"#777678")) + 
  labs(y = "% Total Population Impacted") +
  theme_classic() +
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18),
        plot.margin = margin(1, 3.5, 1, 1, "cm"),
        aspect.ratio = 1/2) +
  coord_flip(clip = "off")

ggsave(plot.P_TotPop_Flooded, filename = paste0("data/4_display/bars/Citywide_barchart_Population.png"),
       dpi = 400,
       width = 10,
       height = 10,
       units = "in",
       bg = "transparent")

## Now for distance-break values in the case of buildings and residential tax lots

data_plot_NYC_bars_buildings <- data.frame(Scenario=c("Moderate", "Extreme"), 
                                                PCT_Buildings_0 = c(data_plot_NYC$M.PCT_buildings_0, data_plot_NYC$E.PCT_buildings_0),
                                                PCT_Buildings_5 = c(data_plot_NYC$M.PCT_buildings_5, data_plot_NYC$E.PCT_buildings_5),
                                                PCT_Buildings_15 = c(data_plot_NYC$M.PCT_buildings_15, data_plot_NYC$E.PCT_buildings_15),
                                                PCT_Buildings_30 = c(data_plot_NYC$M.PCT_buildings_30, data_plot_NYC$E.PCT_buildings_30),
                                                PCT_Buildings_50 = c(data_plot_NYC$M.PCT_buildings_50, data_plot_NYC$E.PCT_buildings_50))

data_plot_NYC_bars_buildings_long <- data_plot_NYC_bars_buildings %>%
  pivot_longer(cols = starts_with("PCT_"), names_to = "Distance", values_to = "PCT_Exposed")

data_plot_NYC_bars_buildings_long$Distance <- c("0", "5", "15", "30", "50", "0", "5", "15", "30", "50")

data_plot_NYC_bars_buildings_long$Scenario <- factor(data_plot_NYC_bars_buildings_long$Scenario, levels = c("Extreme", "Moderate"))
data_plot_NYC_bars_buildings_long$Distance <- factor(data_plot_NYC_bars_buildings_long$Distance, levels = c("50", "30", "15", "5", "0"))

plot.P_Buildings_Flooded <- ggplot(data_plot_NYC_bars_buildings_long, aes(fill = Scenario, y = PCT_Exposed, x = Distance, group = Scenario)) +
  geom_bar(position=position_dodge(width = 1), stat="identity", show.legend = TRUE, width = 0.9) +
  scale_fill_manual(values=c('#004466', '#3485AD',"#777678")) +
  labs(y = "% Buildings", x = "Distance (m)") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 70, 10))+
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18),
        plot.margin = margin(1, 3.5, 1, 1, "cm"),
        aspect.ratio = 1/2) +
  coord_flip(clip = "off")

ggsave(plot.P_Buildings_Flooded, filename = paste0("data/4_display/bars/Citywide_barchart_Buildings.png"),
       dpi = 400,
       width = 10,
       height = 10,
       units = "in",
       bg = "transparent")


data_plot_NYC_bars_ResLots <- data.frame(Scenario=c("Moderate", "Extreme"), 
                                         PCT_ResLots_0 = c(100 * data_plot_NYC$M.Total_residential_lots_0 / data_plot_NYC$Total_residential_lots,
                                                           100 * data_plot_NYC$E.Total_residential_lots_0 / data_plot_NYC$Total_residential_lots),
                                         PCT_ResLots_5 = c(100 * data_plot_NYC$M.Total_residential_lots_5 / data_plot_NYC$Total_residential_lots,
                                                           100 * data_plot_NYC$E.Total_residential_lots_5 / data_plot_NYC$Total_residential_lots),
                                         PCT_ResLots_15 = c(100 * data_plot_NYC$M.Total_residential_lots_15 / data_plot_NYC$Total_residential_lots,
                                                           100 * data_plot_NYC$E.Total_residential_lots_15 / data_plot_NYC$Total_residential_lots),
                                         PCT_ResLots_30 = c(100 * data_plot_NYC$M.Total_residential_lots_30 / data_plot_NYC$Total_residential_lots,
                                                           100 * data_plot_NYC$E.Total_residential_lots_30 / data_plot_NYC$Total_residential_lots),
                                         PCT_ResLots_50 = c(100 * data_plot_NYC$M.Total_residential_lots_50 / data_plot_NYC$Total_residential_lots,
                                                           100 * data_plot_NYC$E.Total_residential_lots_50 / data_plot_NYC$Total_residential_lots))

data_plot_NYC_bars_ResLots_long <- data_plot_NYC_bars_ResLots %>%
  pivot_longer(cols = starts_with("PCT_"), names_to = "Distance", values_to = "PCT_Exposed")

data_plot_NYC_bars_ResLots_long$Distance <- c("0", "5", "15", "30", "50", "0", "5", "15", "30", "50")

data_plot_NYC_bars_ResLots_long$Scenario <- factor(data_plot_NYC_bars_ResLots_long$Scenario, levels = c("Extreme", "Moderate"))
data_plot_NYC_bars_ResLots_long$Distance <- factor(data_plot_NYC_bars_ResLots_long$Distance, levels = c("50", "30", "15", "5", "0"))

plot.P_ResLots_Flooded <- ggplot(data_plot_NYC_bars_ResLots_long, aes(fill = Scenario, y = PCT_Exposed, x = Distance, group = Scenario)) +
  geom_bar(position=position_dodge(width = 1), stat="identity", show.legend = TRUE, width = .9) +
  scale_fill_manual(values=c('#004466', '#3485AD',"#777678")) +
  labs(y = "% Residential Lots", x = "Distance (m)") +
  theme_classic() +
  scale_y_continuous(breaks = seq(0, 70, 10))+
  theme(panel.border = element_blank(),
        legend.background = element_rect(color = NA),
        text=element_text(size=20),
        legend.title = element_text(size=20, face = "bold"),
        legend.text = element_text(size=18),
        axis.title=element_text(size=18),
        plot.margin = margin(1, 3.5, 1, 1, "cm"),
        aspect.ratio = 1/2) +
  coord_flip(clip = "off")

ggsave(plot.P_ResLots_Flooded, filename = paste0("data/4_display/bars/Citywide_barchart_ResLots.png"),
       dpi = 400,
       width = 10,
       height = 10,
       units = "in",
       bg = "transparent")
