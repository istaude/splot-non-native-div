# This script calculates species richness, shannon diversity index, 
# and simpson's index for each splot and finally joins them to the 
# plots selected in script 2
setwd("C:/Users/is68jafy/Desktop/backupSEPT2022/msc_students/suzanne/R/rcode")


# preamble ----------------------------------------------------------------
source("00-preamble.R")

# load splot data ---------------------------------------------------------
load("Data/sPlotOpen.RData")


# create splot dataset for div metrics ------------------------------------
splot_diversity_data <- DT2.oa %>% 
  dplyr::select(PlotObservationID, 
                Species, 
                Abundance_scale, 
                Relative_cover) %>%
  group_by(PlotObservationID, Species) %>% 
  # group and sum relative cover in case some species are recorded twice
  summarise(Relative_cover = sum(Relative_cover)) %>% 
  ungroup()


# only with native species ------------------------------------------------
dt <- read_csv("Data/dt.csv")

splot_natives <- dt %>% 
  filter(species_status == "native") %>% 
  dplyr::select(PlotObservationID, Species) %>% 
  distinct() %>% 
  left_join(DT2.oa %>% 
              dplyr::select(PlotObservationID, 
                            Species, 
                            Abundance_scale, 
                            Relative_cover),
            by = c("PlotObservationID", "Species")) %>%
  # group and sum relative cover in case some species are recorded twice
  group_by(PlotObservationID, Species) %>% 
  summarise(Relative_cover = sum(Relative_cover)) %>% 
  ungroup() %>% 
  distinct() %>% 
  filter(!is.na(Relative_cover))

# calculate diversity metrics ---------------------------------------------
# species richness
# total
div_metrics_total <- 
  splot_diversity_data %>% 
  ungroup() %>% 
  group_by(PlotObservationID) %>% 
  summarise(N = sum(Relative_cover),
            shannon_di = - sum((Relative_cover/sum(Relative_cover)) * log(Relative_cover/sum(Relative_cover))),
            sr = n_distinct(Species)) %>% 
  mutate(pielous_j = shannon_di / log(sr))


# native-only
div_metrics_native <- splot_natives %>% 
  ungroup() %>% 
  group_by(PlotObservationID) %>% 
  summarise(N_natives = sum(Relative_cover),
            shannon_di_natives = -sum((Relative_cover/sum(Relative_cover))*log(Relative_cover/sum(Relative_cover))),
            sr_natives= n_distinct(Species)) %>% 
  mutate(pielous_j_natives = shannon_di_natives / log(sr_natives))


# join to plot selection --------------------------------------------------

# non-native vs native analysis -------------------------------------------
dt_nonvsnat_plots <- read_csv("Data/dt_nonvsnat_plots.csv")

dt_nonvsnat_plots_divs <- dt_nonvsnat_plots %>% 
  # add diversity measures
  left_join(div_metrics_total) %>% 
  left_join(div_metrics_native, 
            by = c("PlotObservationID")) %>%
  mutate(sr_natives = ifelse(is.na(sr_natives) == T, 0, sr_natives)) %>% 
  # add biome, Latitude, Longitude
  left_join(header.oa %>% 
              dplyr::select(PlotObservationID, Biome, Latitude, Longitude)) %>% 
  # add abundance scale
  left_join(DT2.oa %>% 
              dplyr::select(PlotObservationID, Abundance_scale) %>%
              distinct() %>% 
              group_by(PlotObservationID) %>% 
  # there can be more than one abundance scale per plot, collapse
              mutate(abundance_scale = paste(Abundance_scale, collapse = "")) %>% 
              dplyr::select(-Abundance_scale) %>% 
              distinct()) %>%
  # replace all evenness variables with NA when abundance scale is presence/absence (pa)
  mutate(shannon_di = ifelse(grepl("pa", abundance_scale), NA_character_, shannon_di)) %>%
  mutate(pielous_j = ifelse(grepl("pa", abundance_scale), NA_character_, pielous_j)) %>%
  mutate(shannon_di_natives = ifelse(grepl("pa", abundance_scale), NA_character_, shannon_di_natives)) %>%
  mutate(pielous_j_natives = ifelse(grepl("pa", abundance_scale), NA_character_, pielous_j_natives)) %>% 
  distinct()

# check presence/absence cover data 
nrow(dt_nonvsnat_plots_divs)
dt_nonvsnat_plots_divs %>% filter(is.na(pielous_j)) %>% nrow

# write
write.csv(dt_nonvsnat_plots_divs, "Data/dt_nonvsnat_plots_divs.csv", row.names=FALSE)


# invasive non-native vs native analysis ----------------------------------
dt_invvsnat_plots <- read_csv("Data/dt_invvsnat_plots.csv")

dt_invvsnat_plots_divs <- dt_invvsnat_plots %>% 
  # add diversity measures
  left_join(div_metrics_total) %>% 
  left_join(div_metrics_native, 
            by = c("PlotObservationID")) %>% 
  mutate(sr_natives = ifelse(is.na(sr_natives) == T, 0, sr_natives)) %>% 
  # add biome, Latitude, Longitude
  left_join(header.oa %>% 
              dplyr::select(PlotObservationID, Biome, Latitude, Longitude)) %>% 
  # add abundance scale
  left_join(DT2.oa %>% 
              dplyr::select(PlotObservationID, Abundance_scale) %>%
              distinct() %>% 
              group_by(PlotObservationID) %>% 
              # there can be more than one abundance scale per plot, collapse
              mutate(abundance_scale = paste(Abundance_scale, collapse = "")) %>% 
              dplyr::select(-Abundance_scale) %>% 
              distinct()) %>%
  # replace all evenness variables with NA when abundance scale is presence/absence (pa)
  mutate(shannon_di = ifelse(grepl("pa", abundance_scale), NA_character_, shannon_di)) %>%
  mutate(pielous_j = ifelse(grepl("pa", abundance_scale), NA_character_, pielous_j)) %>%
  mutate(shannon_di_natives = ifelse(grepl("pa", abundance_scale), NA_character_, shannon_di_natives)) %>%
  mutate(pielous_j_natives = ifelse(grepl("pa", abundance_scale), NA_character_, pielous_j_natives)) %>% 
  distinct()

# check presence/absence cover data 
nrow(dt_invvsnat_plots_divs)
dt_invvsnat_plots_divs %>% filter(is.na(pielous_j)) %>% nrow

# write
write.csv(dt_invvsnat_plots_divs, "Data/dt_invvsnat_plots_divs.csv", row.names=FALSE)
