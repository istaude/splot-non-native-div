# This script creates supp figures
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


# Supp 1: raw data for local div difference -------------------------------

# in two cells there has been a biome transition that is some plots there are
# alpine / boreal
out <- dt_invvsnat_plots_divs %>% select(cell, Biome) %>% distinct %>% 
  count(cell) %>% arrange(desc(n)) %>% filter(n > 1)
dt_invvsnat_plots_divs <- dt_invvsnat_plots_divs %>% filter(!cell %in% out$cell)

# modify x labels
dt_invvsnat_plots_divs <- dt_invvsnat_plots_divs %>% 
  mutate(plot_status = ifelse(plot_status == "native", "native", "invaded"))

ggplot(dt_invvsnat_plots_divs, aes(x = plot_status, y = sr, col = Biome)) +
  facet_wrap(~
               
               factor(dt_invvsnat_plots_divs$cell, 
                      levels=unique(dt_invvsnat_plots_divs$cell[order(dt_invvsnat_plots_divs$Biome)]), 
                      ordered=TRUE),
             
             scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.4)   +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.4
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 14, family = "Roboto Condensed")
  ) +
  scale_color_manual(values = cc) +
  labs(y = "Species richness", x = "", title = "Invasive vs. native") -> figs1a


# same for non-native vs native
out <- dt_nonvsnat_plots_divs %>% select(cell, Biome) %>% distinct %>% 
  count(cell) %>% arrange(desc(n)) %>% filter(n > 1)
dt_nonvsnat_plots_divs <- dt_nonvsnat_plots_divs %>% filter(!cell %in% out$cell)

dt_nonvsnat_plots_divs <- dt_nonvsnat_plots_divs %>% 
  mutate(plot_status = ifelse(plot_status == "native", "native", "invaded"))


ggplot(dt_nonvsnat_plots_divs, aes(x = plot_status, y = sr, col = Biome)) +
  facet_wrap(~
               
               factor(dt_nonvsnat_plots_divs$cell, 
                      levels=unique(dt_nonvsnat_plots_divs$cell[order(dt_nonvsnat_plots_divs$Biome)]), 
                      ordered=TRUE),
             
             scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.4)   +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.4
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 14, family = "Roboto Condensed")
  ) +
  scale_color_manual(values = cc) +
  labs(y = "Species richness", x = "", title = "Non-native vs. native") -> figs1b


figs1a / figs1b + plot_annotation(tag_levels = 'a', title = "Total species richness",
                                  theme = theme(plot.title = element_text(family = "Roboto Condensed", 
                                                                          size = 16))
  )

showtext_opts(dpi=600)
ggsave(
  "Figures/figureS1.png",
  height= 16.97,
  width = 18.96,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)


# supp 2; native only part ------------------------------------------------

ggplot(dt_invvsnat_plots_divs, aes(x = plot_status, y = sr_natives, col = Biome)) +
  facet_wrap(~
               
               factor(dt_invvsnat_plots_divs$cell, 
                      levels=unique(dt_invvsnat_plots_divs$cell[order(dt_invvsnat_plots_divs$Biome)]), 
                      ordered=TRUE),
             
             scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.4)   +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.4
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 14, family = "Roboto Condensed")
  ) +
  scale_color_manual(values = cc) +
  labs(y = "Species richness", x = "", title = "Invasive vs. native") -> figs2a


ggplot(dt_nonvsnat_plots_divs, aes(x = plot_status, y = sr_natives, col = Biome)) +
  facet_wrap(~
               
               factor(dt_nonvsnat_plots_divs$cell, 
                      levels=unique(dt_nonvsnat_plots_divs$cell[order(dt_nonvsnat_plots_divs$Biome)]), 
                      ordered=TRUE),
             
             scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.4)   +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.4
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 14, family = "Roboto Condensed")
  ) +
  scale_color_manual(values = cc) +
  labs(y = "Species richness", x = "", title = "Non-native vs. native") -> figs2b


figs2a / figs2b + plot_annotation(tag_levels = 'a', title = "Native species richness",
                                  theme = theme(plot.title = element_text(family = "Roboto Condensed", 
                                                                          size = 16))
)

showtext_opts(dpi=600)
ggsave(
  "Figures/figureS2.png",
  height= 16.97,
  width = 18.96,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)



# supp 3; evenness --------------------------------------------------------

dt_invvsnat_plots_divs <- dt_invvsnat_plots_divs %>% filter(!is.na(pielous_j))

ggplot(dt_invvsnat_plots_divs, aes(x = plot_status, y = pielous_j, col = Biome)) +
  facet_wrap(~
               
               factor(dt_invvsnat_plots_divs$cell, 
                      levels=unique(dt_invvsnat_plots_divs$cell[order(dt_invvsnat_plots_divs$Biome)]), 
                      ordered=TRUE),
             
             scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.4)   +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.4
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 14, family = "Roboto Condensed")
  ) +
  scale_color_manual(values = cc) +
  labs(y = "Pielou's J", x = "", title = "Invasive vs. native") -> figs3a


dt_nonvsnat_plots_divs <- dt_nonvsnat_plots_divs %>% filter(!is.na(pielous_j))

ggplot(dt_nonvsnat_plots_divs, aes(x = plot_status, y = pielous_j, col = Biome)) +
  facet_wrap(~
               
               factor(dt_nonvsnat_plots_divs$cell, 
                      levels=unique(dt_nonvsnat_plots_divs$cell[order(dt_nonvsnat_plots_divs$Biome)]), 
                      ordered=TRUE),
             
             scales = "free_y") +
  geom_point(position = position_jitter(width = 0.1), alpha = 0.4)   +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.4
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    text = element_text(size = 14, family = "Roboto Condensed")
  ) +
  scale_color_manual(values = cc) +
  labs(y = "Pielou's J", x = "", title = "Non-native vs. native") -> figs3b


figs3a / figs3b + plot_annotation(tag_levels = 'a', title = "Evenness",
                                  theme = theme(plot.title = element_text(family = "Roboto Condensed", 
                                                                          size = 16))
)

showtext_opts(dpi=600)
ggsave(
  "Figures/figureS3.png",
  height= 16.97,
  width = 18.96,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)


# spatial scale analysis;  ---------------------------------------------

scales_non_natives <- read_csv("Data/scales_non_natives.csv")

scales_non_natives <- scales_non_natives %>% pivot_longer(!c(cell, scale, Biome, logratio), 
                                    names_to = "plot_status", values_to = "mean_cell_sr") %>% 
  mutate(scale = factor(scale, levels = c("Cell scale", "Biome scale", "Global scale"))) %>% 
  mutate(cell = ifelse(is.na(cell)== T, Biome, cell)) %>% 
  mutate(cell = ifelse(is.na(cell)== T, "Globe", cell)) %>% 
mutate(plot_status = ifelse(plot_status == "native", "native", "invaded"))

(
  ggplot(data = scales_non_natives, aes(
    x = plot_status, y = mean_cell_sr,
  )) +
    facet_wrap(~scale, scales = "free_y") +
    geom_point(position = position_jitter(width = 0.1), alpha = 0.4, col = "magenta")   +
    geom_line(aes(group = cell), alpha = 0.3) +
    geom_boxplot(
      outlier.shape = NA,
      width = 0.2,
      alpha = 0.4
    ) +
    #scale_y_continuous(trans = 'log10') +
    theme_ipsum(
      grid = "Y",
      axis_title_just = "mm",
      axis_text_size = 10,
      base_family = "Roboto Condensed"
    ) +
    labs(y = "Species richness", x = "", title = "Non-native vs. native") +
    theme(legend.position = "none") -> ps1a
)

scales_invasives <- read_csv("Data/scales_invasives.csv")

scales_invasives <- scales_invasives %>% pivot_longer(!c(cell, scale, Biome, logratio), 
                                                      names_to = "plot_status", values_to = "mean_cell_sr") %>% 
  mutate(scale = factor(scale, levels = c("Cell scale", "Biome scale", "Global scale"))) %>% 
  mutate(cell = ifelse(is.na(cell)== T, Biome, cell)) %>% 
  mutate(cell = ifelse(is.na(cell)== T, "Globe", cell)) %>% 
  mutate(plot_status = ifelse(plot_status == "native", "native", "invaded"))

(
  ggplot(data = scales_invasives, aes(
    x = plot_status, y = mean_cell_sr,
  )) +
    facet_wrap(~scale, scales = "free_y") +
    geom_point(position = position_jitter(width = 0.1), alpha = 0.4, col = "magenta")   +
    geom_line(aes(group = cell), alpha = 0.3) +
    geom_boxplot(
      outlier.shape = NA,
      width = 0.2,
      alpha = 0.4
    ) +
    #scale_y_continuous(trans = 'log10') +
    theme_ipsum(
      grid = "Y",
      axis_title_just = "mm",
      axis_text_size = 10,
      base_family = "Roboto Condensed"
    ) +
    labs(y = "Species richness", x = "", title = "Invasive vs. native") +
    theme(legend.position = "none") -> ps2a
)

ps2a / ps1a + plot_annotation(tag_levels = 'a', title = "Gamma species richness across spatial scales",
                              theme = theme(plot.title = element_text(family = "Roboto Condensed", 
                                                                      size = 16))
)

showtext_opts(dpi=600)
ggsave(
  "Figures/figureS4.png",
  height= 9.97,
  width = 8.96,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)


                              