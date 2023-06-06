# This scripts selects plots for analyses,
# and explores different minimum number of plots per cell.

# preamble ----------------------------------------------------------------
source("00-preamble.R")

# load splot data ---------------------------------------------------------
load("Data/sPlotOpen.RData")


# load joined data --------------------------------------------------------
dt <- read_csv("Data/dt.csv")


# differentiate plots according to species status -------------------------
plot_status <- dt %>% 
  dplyr::select(PlotObservationID, species_status, cell) %>% 
  distinct() %>%  
  group_by(PlotObservationID) %>% 
  mutate(plot_status = paste(species_status, collapse=" ")) %>% 
  dplyr::select(-species_status) %>% 
  distinct() %>% 
  mutate(plot_status = ifelse(grepl("invasive", 
                                    c(plot_status)), 
                              "invasives", plot_status)) %>% 
  mutate(plot_status = ifelse(grepl("non-native", 
                                    c(plot_status)), 
                              "non-natives", plot_status))


# select plots with most frequent size inside each cell -------------------
# add plot area column
plot_status <- plot_status %>% 
  left_join(header.oa %>% dplyr::select(PlotObservationID, Releve_area))

# find most frequent size of plot inside each cell
plot_status_freq <- plot_status %>% 
  group_by(cell) %>% 
  count(Releve_area) %>%
  group_by(cell) %>% 
  filter(n == max(n)) %>% #keep only most frequent sizes in each cell
  filter(n != 1) %>% # remove cells that do not have 2 or more plots of the same size
  slice(1) %>% # keep only the first one of the most frequent size of plots when there are multiple most frequent plot sizes
  rename(most_frequent_area = Releve_area) %>% 
  # select plots with the most frequent size only
  dplyr::select(-n) %>% 
  left_join(plot_status) %>%  
  filter(Releve_area == most_frequent_area) %>% 
  dplyr::select(PlotObservationID, plot_status, cell, Releve_area) 


# find cells with enough plots to compare ---------------------------------
# for comparison:
# non-natives vs. natives: min 3 plots of each type per cell
dt_nonvsnat <- left_join(plot_status_freq,
                         left_join(
                           plot_status_freq %>% # count number of plots per cell
                             ungroup() %>% 
                             count(cell),
                           plot_status_freq %>% # count number of natives/non-natives plots within each cell
                             ungroup() %>% 
                             group_by(cell) %>% 
                             count(nplots = plot_status) %>% 
                             spread(key = nplots, value = n) 
                         )
) %>% 
  group_by(cell) %>% 
  dplyr::select(-PlotObservationID) %>% 
  distinct() %>% 
  mutate(cell_type = paste(plot_status, collapse=" ")) %>%
  dplyr::select(-plot_status) %>%  
  distinct() %>%
  rename(n_total = n, 
         n_non_natives = `non-natives`, 
         n_natives = native, 
         n_invasives = invasives) %>% 
  mutate(n_non_natives = ifelse(is.na(n_non_natives), 0, n_non_natives)) %>%
  mutate(n_invasives = ifelse(is.na(n_invasives), 0, n_invasives)) %>% 
  filter(n_non_natives >= 3 & n_natives >= 3)

# how many grid cells that have at least 3 native vs 3 non-native plots? 
View(dt_nonvsnat)
nrow(dt_nonvsnat)

# plots IDs to be analysed
dt_nonvsnat_plots <- dt_nonvsnat %>% 
  left_join(plot_status) %>% 
  filter(plot_status != "invasives") %>%
  dplyr::select(cell,
                PlotObservationID,
                plot_status,
                Releve_area,
                n_non_natives,
                n_natives)

# how many non-native vs native plors are compared in total?
dt_nonvsnat_plots %>% ungroup %>% count(plot_status)




# for comparison:
# invasives vs. natives: min 3 plots of each type per cell
dt_invvsnat <- left_join(plot_status_freq,
                         left_join(
                           plot_status_freq %>% # count number of plots per cell
                             ungroup() %>% 
                             count(cell),
                           plot_status_freq %>% # count number of natives/non-natives plots within each cell
                             ungroup() %>% 
                             group_by(cell) %>% 
                             count(nplots = plot_status) %>% 
                             spread(key = nplots, value = n) 
                         )
) %>% 
  group_by(cell) %>% 
  dplyr::select(-PlotObservationID) %>% 
  distinct() %>% 
  mutate(cell_type = paste(plot_status, collapse=" ")) %>%
  dplyr::select(-plot_status) %>%  
  distinct() %>%
  rename(n_total = n, 
         n_non_natives = `non-natives`, 
         n_natives = native, 
         n_invasives = invasives) %>% 
  mutate(n_non_natives = ifelse(is.na(n_non_natives), 0, n_non_natives)) %>%
  mutate(n_invasives = ifelse(is.na(n_invasives), 0, n_invasives)) %>% 
  filter(n_invasives >= 3 & n_natives >= 3)

# how many grid cells that have at least 3 native vs 3 non-native plots? 
View(dt_invvsnat)
nrow(dt_invvsnat)

# plots IDs to be analysed
dt_invvsnat_plots <- dt_invvsnat %>% 
  left_join(plot_status) %>% 
  filter(plot_status != "non-natives") %>%
  dplyr::select(cell,
                PlotObservationID,
                plot_status,
                Releve_area,
                n_invasives,
                n_natives)

# how many non-native vs native plots are compared in total?
dt_invvsnat_plots %>% ungroup %>% count(plot_status)


# write data --------------------------------------------------------------
# non-native vs native plots
write.csv(dt_nonvsnat_plots,
          "Data/dt_nonvsnat_plots.csv",row.names=FALSE)

# invasive vs native plots
write.csv(dt_invvsnat_plots,
          "Data/dt_invvsnat_plots.csv",row.names=FALSE)

