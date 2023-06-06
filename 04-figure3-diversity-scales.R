# This script loads data and creates figure 3 and all underlying analyses
# aim is to see how diversity in invaded / non-invaded sites differs across
# spatial scales

# 1) cell diversity
# 2) biome diversity
# 3) global diversity

# what is needed is a sampling procedures in which the same number of invaded 
# and non-invaded plots are sampled.
setwd("C:/Users/is68jafy/Desktop/backupSEPT2022/msc_students/suzanne/R/rcode")


# preamble ----------------------------------------------------------------
source("00-preamble.R")
showtext_auto()

# load data ---------------------------------------------------------------
load("Data/sPlotOpen.RData")

# non-native vs native
dt_nonvsnat_plots_divs <- read_csv("Data/dt_nonvsnat_plots_divs.csv")

# how often do I sample, sample the number of plots of the type with fewer plots, in the type with more plots 
samples_number <- dt_nonvsnat_plots_divs %>% select(cell, n_non_natives, n_natives) %>% distinct %>% 
  mutate(sample_n = ifelse(n_non_natives >= n_natives, n_natives, n_non_natives)) %>% 
  dplyr::select(-n_non_natives, -n_natives)

# now sample, get plot IDs
samples_plotid <- dt_nonvsnat_plots_divs %>% 
  dplyr::select(Biome, cell, PlotObservationID, plot_status) %>% 
  left_join(samples_number) %>% 
  group_by(cell, plot_status) %>% 
  sample_n(sample_n)



# Cell scale --------------------------------------------------------------

# repeat 100 times
# empty list
repetition_mean_srs <- NULL
for (i in 1:100){
# now sample, get plot IDs
samples_plotid <- dt_nonvsnat_plots_divs %>% 
  dplyr::select(cell, PlotObservationID, plot_status) %>% 
  left_join(samples_number) %>% 
  group_by(cell, plot_status) %>% 
  sample_n(sample_n)

# now use these plot IDS to calculate sr across plots, for this you need
# the original community data
cell_sr <- left_join(samples_plotid,
          DT2.oa %>% select(PlotObservationID, Species)) %>% 
  group_by(cell, plot_status) %>% 
  summarise(sr = n_distinct(Species))

# calculate sample means 100 times
repetition_mean_srs[[i]] <- cell_sr
}

# calculate for each cell the mean sr across sampling repetitions
mean_cell_sr <- bind_rows(repetition_mean_srs) %>% 
  arrange(cell) %>% 
  group_by(cell, plot_status) %>% 
  summarize(mean_cell_sr = mean(sr))
  
write_excel_csv2(mean_cell_sr, "Data/spatial_scales_celllevel.csv")

# relevel
mean_cell_sr$plot_status <- relevel(factor(mean_cell_sr$plot_status), ref = "non-natives")

# model
mod_cell <- lmer(log(mean_cell_sr) ~ plot_status + (1|cell), data = mean_cell_sr)
summary(mod_cell)
emmeans(mod_cell, ~ plot_status, type = "response")
pairs(emmeans(mod_cell, ~ plot_status, type = "response")) %>% confint

# contrast
pairs(emmeans(mod_cell, ~ plot_status)) %>% confint %>%
  plot +
  geom_vline(xintercept = 0) +
  theme_ipsum(
    grid = "Y",
    axis_title_just = "mm",
    axis_text_size = 10,
    base_family = "Roboto Condensed"
  ) +
  theme(axis.text.y = element_blank()) +
  coord_cartesian(xlim=c(-0.8, 1.5)) +
  labs(x = "Estimated mean log ratio", 
       y = "")


# Biome scale -------------------------------------------------------------

# repeat 100 times
# empty list
biome_repetition_mean_srs <- NULL
for (i in 1:100){
  # now sample, get plot IDs
  samples_plotid <- dt_nonvsnat_plots_divs %>% 
    dplyr::select(Biome, cell, PlotObservationID, plot_status) %>% 
    left_join(samples_number) %>% 
    group_by(cell, plot_status) %>% 
    sample_n(sample_n)
  
  # now use these plot IDS to calculate sr across plots, for this you need
  # the original community data
  biome_sr <- left_join(samples_plotid,
                        DT2.oa %>% select(PlotObservationID, Species)) %>% 
    group_by(Biome, plot_status) %>% 
    summarise(sr = n_distinct(Species))
  
  # calculate sample means 100 times
  biome_repetition_mean_srs[[i]] <- biome_sr
}

# calculate for each biome the mean sr across sampling repetitions
mean_biome_sr <- bind_rows(biome_repetition_mean_srs) %>% 
  arrange(Biome) %>% 
  group_by(Biome, plot_status) %>% 
  summarize(mean_biome_sr = mean(sr))

write_excel_csv2(mean_biome_sr, "Data/spatial_scales_biomelevel.csv")

# relevel
mean_biome_sr$plot_status <- relevel(factor(mean_biome_sr$plot_status), ref = "non-natives")

# model
mod_biome <- lmer(log(mean_biome_sr) ~ plot_status + (1|Biome), data = mean_biome_sr)
summary(mod_biome)
emmeans(mod_biome, ~ plot_status, type = "response")
pairs(emmeans(mod_biome, ~ plot_status, type = "response")) %>% confint


# contrast
pairs(emmeans(mod_biome, ~ plot_status)) %>% confint %>%
    plot +
    geom_vline(xintercept = 0) +
    theme_ipsum(
      grid = "Y",
      axis_title_just = "mm",
      axis_text_size = 10,
      base_family = "Roboto Condensed"
    ) +
    theme(axis.text.y = element_blank()) +
    coord_cartesian(xlim=c(-0.05, 0.35)) +
    labs(x = "Estimated mean log ratio", 
         y = "") 

# global scale ------------------------------------------------------------

global_repetition_mean_srs <- NULL
for (i in 1:100){
  # now sample, get plot IDs
  samples_plotid <- dt_nonvsnat_plots_divs %>% 
    dplyr::select(Biome, cell, PlotObservationID, plot_status) %>% 
    left_join(samples_number) %>% 
    group_by(cell, plot_status) %>% 
    sample_n(sample_n)
  
  # now use these plot IDS to calculate sr across plots, for this you need
  # the original community data
  global_sr <- left_join(samples_plotid,
                        DT2.oa %>% select(PlotObservationID, Species)) %>% 
    group_by(plot_status) %>% 
    summarise(sr = n_distinct(Species))
  
  # calculate sample means 100 times
  global_repetition_mean_srs[[i]] <- global_sr
}


global_srs <- bind_rows(global_repetition_mean_srs)


# bring together data into one dataframe to use facetwrap -----------------
d_scales <- bind_rows(
  
  list(
    mean_cell_sr %>% 
      pivot_wider(names_from = plot_status, values_from = mean_cell_sr) %>% 
      mutate(logratio = log(`non-natives`/native)) %>% 
      mutate(scale = "Cell scale"),
    
    mean_biome_sr %>% 
      pivot_wider(names_from = plot_status, values_from = mean_biome_sr) %>% 
      mutate(logratio = log(`non-natives`/native)) %>% 
      mutate(scale = "Biome scale"),
    
    global_srs %>%
      group_by(plot_status) %>% 
      summarize(mean_global_sr = mean(sr)) %>% 
      pivot_wider(names_from = plot_status, values_from = mean_global_sr) %>% 
      mutate(logratio = log(`non-natives`/native)) %>% 
      mutate(scale = "Global scale")
  )
) %>% mutate(scale = factor(scale, levels = c("Cell scale", 
                                          "Biome scale", "Global scale")))

write.csv(d_scales, "Data/scales_non_natives.csv", row.names = F)


means <- bind_rows(
  pairs(emmeans(mod_biome, ~ plot_status)) %>% confint %>% mutate(scale = "Cell scale"),
  pairs(emmeans(mod_cell, ~ plot_status)) %>% confint %>% mutate(scale = "Biome scale")
) %>%
  select(estimate, scale) %>% 
  mutate(scale = factor(scale, levels = c("Cell scale",  "Biome scale")))


(
  ggplot(d_scales, aes(x = logratio)) +
    facet_wrap( ~ scale) +
    geom_histogram(fill = "#654CFFFF") +
    geom_vline(xintercept = 0) +
    geom_vline(data = means, aes(xintercept = estimate), lty = 2) +
    theme_ipsum(
      grid = "Y",
      axis_title_just = "mm",
      axis_text_size = 10,
      base_family = "Roboto Condensed"
    ) +
    theme(legend.position = "none",
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12)) +
    labs(x = "Log ratio gamma richness non-native plots / native plots", 
         y = "Count",
         title = "non-native vs. native") -> fig3a
)
















# same for invasive vs. native --------------------------------------------

# non-native vs native
dt_invvsnat_plots_divs <- read_csv("Data/dt_invvsnat_plots_divs.csv")

# how often do I sample, sample the number of plots of the type with fewer plots, in the type with more plots 
samples_number <- dt_invvsnat_plots_divs %>% select(cell, n_invasives, n_natives) %>% distinct %>% 
  mutate(sample_n = ifelse(n_invasives >= n_natives, n_natives, n_invasives)) %>% 
  dplyr::select(-n_invasives, -n_natives)

# now sample, get plot IDs
samples_plotid <- dt_invvsnat_plots_divs %>% 
  dplyr::select(Biome, cell, PlotObservationID, plot_status) %>% 
  left_join(samples_number) %>% 
  group_by(cell, plot_status) %>% 
  sample_n(sample_n)



# Cell scale --------------------------------------------------------------

# repeat 100 times
# empty list
inv_repetition_mean_srs <- NULL
for (i in 1:100){
  # now sample, get plot IDs
  samples_plotid <- dt_invvsnat_plots_divs %>% 
    dplyr::select(cell, PlotObservationID, plot_status) %>% 
    left_join(samples_number) %>% 
    group_by(cell, plot_status) %>% 
    sample_n(sample_n)
  
  # now use these plot IDS to calculate sr across plots, for this you need
  # the original community data
  cell_sr <- left_join(samples_plotid,
                       DT2.oa %>% select(PlotObservationID, Species)) %>% 
    group_by(cell, plot_status) %>% 
    summarise(sr = n_distinct(Species))
  
  # calculate sample means 100 times
  inv_repetition_mean_srs[[i]] <- cell_sr
}

# calculate for each cell the mean sr across sampling repetitions
inv_mean_cell_sr <- bind_rows(inv_repetition_mean_srs) %>% 
  arrange(cell) %>% 
  group_by(cell, plot_status) %>% 
  summarize(mean_cell_sr = mean(sr))

write_excel_csv2(inv_mean_cell_sr, "Data/spatial_scales_celllevel_inv.csv")


# relevel
inv_mean_cell_sr$plot_status <- relevel(factor(inv_mean_cell_sr$plot_status), ref = "invasives")

# model
mod_cell <- lmer(log(inv_mean_cell_sr) ~ plot_status + (1|cell), data = inv_mean_cell_sr)
summary(mod_cell)
emmeans(mod_cell, ~ plot_status, type = "response")
pairs(emmeans(mod_cell, ~ plot_status, type = "response")) %>% confint

# contrast
pairs(emmeans(mod_cell, ~ plot_status)) %>% confint %>%
  plot +
  geom_vline(xintercept = 0) +
  theme_ipsum(
    grid = "Y",
    axis_title_just = "mm",
    axis_text_size = 10,
    base_family = "Roboto Condensed"
  ) +
  theme(axis.text.y = element_blank()) +
  labs(x = "Estimated mean log ratio", 
       y = "")


# Biome scale -------------------------------------------------------------

# repeat 100 times
# empty list
inv_biome_repetition_mean_srs <- NULL
for (i in 1:100){
  # now sample, get plot IDs
  samples_plotid <- dt_invvsnat_plots_divs %>% 
    dplyr::select(Biome, cell, PlotObservationID, plot_status) %>% 
    left_join(samples_number) %>% 
    group_by(cell, plot_status) %>% 
    sample_n(sample_n)
  
  # now use these plot IDS to calculate sr across plots, for this you need
  # the original community data
  biome_sr <- left_join(samples_plotid,
                        DT2.oa %>% select(PlotObservationID, Species)) %>% 
    group_by(Biome, plot_status) %>% 
    summarise(sr = n_distinct(Species))
  
  # calculate sample means 100 times
  inv_biome_repetition_mean_srs[[i]] <- biome_sr
}

# calculate for each biome the mean sr across sampling repetitions
inv_mean_biome_sr <- bind_rows(inv_biome_repetition_mean_srs) %>% 
  arrange(Biome) %>% 
  group_by(Biome, plot_status) %>% 
  summarize(mean_biome_sr = mean(sr))


write_excel_csv2(inv_mean_biome_sr, "Data/spatial_scales_biomelevel_inv.csv")


# relevel
inv_mean_biome_sr$plot_status <- relevel(factor(inv_mean_biome_sr$plot_status), ref = "invasives")

# model
mod_biome <- lmer(log(mean_biome_sr) ~ plot_status + (1|Biome), data = inv_mean_biome_sr)
summary(mod_biome)
emmeans(mod_biome, ~ plot_status, type = "response")
pairs(emmeans(mod_biome, ~ plot_status, type = "response")) %>% confint



# contrast
pairs(emmeans(mod_biome, ~ plot_status)) %>% confint %>%
  plot +
  geom_vline(xintercept = 0) +
  theme_ipsum(
    grid = "Y",
    axis_title_just = "mm",
    axis_text_size = 10,
    base_family = "Roboto Condensed"
  ) +
  theme(axis.text.y = element_blank()) +
  labs(x = "Estimated mean log ratio", 
       y = "") 

# global scale ------------------------------------------------------------

inv_global_repetition_mean_srs <- NULL
for (i in 1:100){
  # now sample, get plot IDs
  samples_plotid <- dt_invvsnat_plots_divs %>% 
    dplyr::select(Biome, cell, PlotObservationID, plot_status) %>% 
    left_join(samples_number) %>% 
    group_by(cell, plot_status) %>% 
    sample_n(sample_n)
  
  # now use these plot IDS to calculate sr across plots, for this you need
  # the original community data
  global_sr <- left_join(samples_plotid,
                         DT2.oa %>% select(PlotObservationID, Species)) %>% 
    group_by(plot_status) %>% 
    summarise(sr = n_distinct(Species))
  
  # calculate sample means 100 times
  inv_global_repetition_mean_srs[[i]] <- global_sr
}


inv_global_srs <- bind_rows(inv_global_repetition_mean_srs)


# bring together data into one dataframe to use facetwrap -----------------
d_scales_inv <- bind_rows(
  
  list(
    inv_mean_cell_sr %>% 
      pivot_wider(names_from = plot_status, values_from = mean_cell_sr) %>% 
      mutate(logratio = log(invasives/native)) %>% 
      mutate(scale = "Cell scale"),
    
    inv_mean_biome_sr %>% 
      pivot_wider(names_from = plot_status, values_from = mean_biome_sr) %>% 
      mutate(logratio = log(invasives/native)) %>% 
      mutate(scale = "Biome scale"),
    
    inv_global_srs %>%
      group_by(plot_status) %>% 
      summarize(mean_global_sr = mean(sr)) %>% 
      pivot_wider(names_from = plot_status, values_from = mean_global_sr) %>% 
      mutate(logratio = log(invasives/native)) %>% 
      mutate(scale = "Global scale")
  )
) %>% mutate(scale = factor(scale, levels = c("Cell scale", 
                                              "Biome scale", "Global scale")))


write.csv(d_scales_inv, "Data/scales_invasives.csv", row.names = F)


means_inv <- bind_rows(
  pairs(emmeans(mod_biome, ~ plot_status)) %>% confint %>% mutate(scale = "Cell scale"),
  pairs(emmeans(mod_cell, ~ plot_status)) %>% confint %>% mutate(scale = "Biome scale")
) %>%
  select(estimate, scale) %>% 
  mutate(scale = factor(scale, levels = c("Cell scale",  "Biome scale")))

(
  ggplot(d_scales_inv, aes(x = logratio)) +
  facet_wrap( ~ scale) +
  geom_histogram(fill = "#19B2FFFF") +
  geom_vline(xintercept = 0) +
  geom_vline(data = means_inv, aes(xintercept = estimate), lty = 2) +
  theme_ipsum(
    grid = "Y",
    axis_title_just = "mm",
    axis_text_size = 10,
    base_family = "Roboto Condensed"
  ) +
  theme(legend.position = "none",
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12)) +
  labs(x = "Log ratio gamma richness invasive plots / native plots", 
       y = "Count",
       title = "invasive vs. native") -> fig3b
)



fig3b /
  fig3a + plot_annotation(tag_levels = 'a', title = "Gamma species richness across spatial scales",
                          theme = theme(plot.title = element_text(family = "Roboto Condensed", 
                                                                  size = 16))
  )


showtext_opts(dpi=600)
ggsave(
  "Figures/figure4.png",
  height= 8.97,
  width = 8.96,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)


