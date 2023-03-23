### barplots for the city

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

