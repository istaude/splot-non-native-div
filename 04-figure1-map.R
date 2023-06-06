# This script loads and creates figure 1 (worldmap with sampling locations)
setwd("C:/Users/is68jafy/Desktop/backupSEPT2022/msc_students/suzanne/R/rcode")


# preamble ----------------------------------------------------------------
source("00-preamble.R")
showtext_auto()

# load data ---------------------------------------------------------------
load("Data/sPlotOpen.RData")

# invasive vs native
dt_invvsnat_plots_divs <- read_csv("Data/dt_invvsnat_plots_divs.csv")

#non-native vs native
dt_nonvsnat_plots_divs <- read_csv("Data/dt_nonvsnat_plots_divs.csv")

names(dt_invvsnat_plots_divs)

# world map of splots used in analysis ------------------------------------
world <- map_data("world") 
world <- world %>% filter(region != "Antarctica")
cc <- paletteer_d("ggthemes::calc") 

# create full data frame with invasive and non-native plot locations
d_locations <- bind_rows(
  dt_invvsnat_plots_divs %>% 
    dplyr::select(cell, PlotObservationID, plot_status, Longitude, Latitude, Biome, Releve_area) %>% 
    mutate(Comparison = "invasive vs. native"),
  dt_nonvsnat_plots_divs %>% 
    dplyr::select(cell, PlotObservationID, plot_status, Longitude, Latitude, Biome, Releve_area) %>% 
    mutate(Comparison = "non-native vs. native")
) %>% distinct

# plot
ggplot() + 
  geom_map(data = world, 
           map = world, 
           aes(x = long, y = lat, map_id = region), 
           color = "grey70", fill = "white") + 
  geom_point(data = d_locations, 
             aes(x = Longitude, y = Latitude, color = Biome, shape = Comparison), 
             size = 3,
             alpha = 0.9) + 
  theme_void(base_family = "Roboto Condensed") +
  scale_color_manual(values=cc, na.value="grey70")

# save
showtext_opts(dpi=600)
ggsave(
  "Figures/map_figure1.png",
  height= 4.55,
  width = 9.86,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)


# summary stats for fig caption
d_locations %>% nrow

d_locations %>% 
  group_by(Comparison) %>% 
  summarize(n_distinct(PlotObservationID))

d_locations %>% 
  group_by(Comparison, plot_status) %>% 
  summarize(n_distinct(PlotObservationID))

d_locations %>% 
  group_by(Comparison) %>% 
  summarize(n_distinct(cell))

d_locations %>% 
  group_by(Comparison) %>% 
  summarize(n_distinct(Biome))

d_locations %>% 
  group_by(Comparison) %>% 
  summarize(min(Releve_area),
            max(Releve_area),
            median(Releve_area))
