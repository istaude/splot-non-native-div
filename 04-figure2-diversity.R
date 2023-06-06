# This script loads data and creates figure 2 and all underlying analyses
# (diversity differences)
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


# models ------------------------------------------------------------------
dt_invvsnat_plots_divs$plot_status <- relevel(factor(dt_invvsnat_plots_divs$plot_status), ref = "invasives")
dt_nonvsnat_plots_divs$plot_status <- relevel(factor(dt_nonvsnat_plots_divs$plot_status), ref = "non-natives")

# model 1: invasive, non-native vs native sr ------------------------------

# total SR
mod1 <- glmer(sr ~ plot_status + (1 | cell), 
              family = "poisson", 
              data = dt_invvsnat_plots_divs)
summary(mod1)
emmeans(mod1, ~ plot_status, type = "response")

contrast_mod1 <- pairs(emmeans(mod1, ~ plot_status, type = "response")) %>%
  confint %>%
  data.frame() %>%
  mutate(class = "Total")%>% 
  mutate(class2 = "invasive vs. native") %>% 
  mutate(Biome = "All")


# native SR
mod1a <- glmer(sr_natives ~ plot_status + (1 | cell), 
               family = "poisson", 
               data = dt_invvsnat_plots_divs)
summary(mod1a)

contrast_mod1a <- pairs(emmeans(mod1a, ~ plot_status, type = "response")) %>%
  confint %>%
  data.frame() %>%
  mutate(class = "Native") %>% 
  mutate(class2 = "invasive vs. native") %>% 
  mutate(Biome = "All")



# model 2: non-native vs native sr ----------------------------------------

# total SR
mod2 <- glmer(sr ~ plot_status + (1 | cell), 
              family = "poisson", 
              data = dt_nonvsnat_plots_divs)
summary(mod2)
emmeans(mod2, ~ plot_status, type = "response")

contrast_mod2 <- pairs(emmeans(mod2, ~ plot_status, type = "response")) %>%
  confint %>%
  data.frame() %>%
  mutate(class = "Total") %>% 
  mutate(class2 = "non-native vs. native") %>% 
  mutate(Biome = "All")



# native SR
mod2a <- glmer(sr_natives ~ plot_status + (1 | cell), 
               family = "poisson", 
               data = dt_nonvsnat_plots_divs)
summary(mod2a)

contrast_mod2a <- pairs(emmeans(mod2a, ~ plot_status, type = "response")) %>%
  confint %>%
  data.frame() %>%
  mutate(class = "Native") %>% 
  mutate(class2 = "non-native vs. native") %>% 
  mutate(Biome = "All")




# model 3: invasive, non-native vs native sr biome ------------------------

# total SR
mod3 <- glmer(sr ~ plot_status*Biome + (1 | cell), 
              family = "poisson", 
              control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
              data = dt_invvsnat_plots_divs)
summary(mod3)

contrast_mod3 <-
  pairs(emmeans(mod3, ~ plot_status |
                  Biome, type = "response")) %>% confint %>%
  data.frame() %>%
  mutate(class = "Total") %>% 
  mutate(class2 = "invasive vs. native")


# native SR
mod3a <- glmer(sr_natives ~ plot_status*Biome + (1 | cell), 
              family = "poisson", 
              control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
              data = dt_invvsnat_plots_divs)
summary(mod3a)

contrast_mod3a <-
  pairs(emmeans(mod3a, ~ plot_status |
                  Biome, type = "response")) %>% confint %>%
  data.frame() %>% 
  mutate(class = "Native") %>% 
  mutate(class2 = "invasive vs. native")



# model 4: non-native vs native sr biome ----------------------------------

# total SR
mod4 <- glmer(sr ~ plot_status*Biome + (1 | cell), 
              family = "poisson",
              control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
              data = dt_nonvsnat_plots_divs)
summary(mod4)

contrast_mod4 <-
  pairs(emmeans(mod4, ~ plot_status |
                  Biome, type = "response")) %>% confint %>%
  data.frame() %>% 
  mutate(class = "Total") %>% 
  mutate(class2 = "non-native vs. native")


# native SR
mod4a <- glmer(sr_natives ~ plot_status*Biome + (1 | cell), 
              family = "poisson",
              control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
              data = dt_nonvsnat_plots_divs)
summary(mod4a)

contrast_mod4a <-
  pairs(emmeans(mod4a, ~ plot_status |
                  Biome, type = "response")) %>% confint %>%
  data.frame() %>% 
  mutate(class = "Native") %>% 
  mutate(class2 = "non-native vs. native")



# summary fig -------------------------------------------------------------
d_models <- bind_rows(list(contrast_mod1,
                                   contrast_mod1a,
                                   contrast_mod2,
                                   contrast_mod2a,
                                   contrast_mod3,
                                   contrast_mod3a,
                                   contrast_mod4,
                                   contrast_mod4a
                                   ))

# add sample sizes to biomes
sample_size <- bind_rows(
  list(
    # invasives
    # all
    dt_invvsnat_plots_divs %>% summarize(cells = n_distinct(cell),
                                         plots = n_distinct(PlotObservationID)) %>%
      mutate(Biome = "All",
             class2 = "invasive vs. native"),
    # biomes
    dt_invvsnat_plots_divs %>%
      group_by(Biome) %>%
      summarize(cells = n_distinct(cell),
                plots = n_distinct(PlotObservationID)) %>%
      mutate(class2 = "invasive vs. native"),
    
    # non-natives
    # all
    dt_nonvsnat_plots_divs %>% summarize(cells = n_distinct(cell),
                                         plots = n_distinct(PlotObservationID)) %>%
      mutate(Biome = "All",
             class2 = "non-native vs. native"),
    # biomes
    dt_nonvsnat_plots_divs %>%
      group_by(Biome) %>%
      summarize(cells = n_distinct(cell),
                plots = n_distinct(PlotObservationID)) %>%
      mutate(class2 = "non-native vs. native")
  )
)

d_models <- full_join(d_models, sample_size)
d_models$sample_size <- paste0("n = ", d_models$cells, "; ", d_models$plots)

# plot
(ggplot(d_models,
        aes(
          x = ratio,
          y = Biome,
          group = class,
          col = Biome,
          shape = class
        )) +
    facet_wrap( ~ class2, nrow = 1) +
    geom_point(position = position_dodge(width = 0.8)) +
    geom_linerange(
      aes(xmin = asymp.LCL, xmax = asymp.UCL),
      position = position_dodge(width = 0.8)
    ) +
    geom_text(inherit.aes = F,
              data = d_models,
              aes(x = Inf, y = Biome, label = sample_size),
              vjust = 0.6, hjust = 1, size = 2.5,
              fontface = "italic",
              family = "Roboto Condensed",
              check_overlap = TRUE) +
    geom_vline(xintercept = 1, lty = 2) +
    labs(y = "", x = "Species richness invaded plot / native plot") +
    scale_color_manual(values = c("black", cc)) +
    scale_shape_manual(values = c(17, 16)) +
    scale_y_discrete(limits = rev) +
    theme_ipsum(grid = "X",
                axis_title_just = "mm",
                axis_text_size = 10,
                base_family = "Roboto Condensed") +
    labs(title = "Species richness") +
    theme(plot.margin = unit(c(0.3,0,0,0), "cm"),
          axis.title.x = element_text(size=12)) +
    guides(colour = "none",
           shape = guide_legend(title = "Part of community",
                                reverse = TRUE)) -> f2a)


# save
showtext_opts(dpi=600)
ggsave(
  "Figures/figure2.png",
  height= 4.55,
  width = 9.86,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)






# same for evenness -------------------------------------------------------
# only for the total community

# model 5: invasive, non-native vs native pj ------------------------------
mod5 <- lmer(pielous_j ~ plot_status + (1 | cell), 
              data = dt_invvsnat_plots_divs)
summary(mod5)
emmeans(mod5, ~ plot_status, type = "response")

contrast_mod5 <- pairs(emmeans(mod5, ~ plot_status, type = "response")) %>%
  confint %>%
  data.frame() %>%
  mutate(class2 = "invasive vs. native") %>% 
  mutate(Biome = "All")


# model 6: non-native vs native pj ----------------------------------------
mod6 <- lmer(pielous_j ~ plot_status + (1 | cell), 
              data = dt_nonvsnat_plots_divs)
summary(mod6)
emmeans(mod6, ~ plot_status, type = "response")

contrast_mod6 <- pairs(emmeans(mod6, ~ plot_status, type = "response")) %>%
  confint %>%
  data.frame() %>%
  mutate(class2 = "non-native vs. native") %>% 
  mutate(Biome = "All")


# model 7: invasive, non-native vs native pj biome ------------------------
mod7 <- lmer(pielous_j ~ plot_status*Biome + (1 | cell), 
              data = dt_invvsnat_plots_divs)
summary(mod7)

contrast_mod7 <-
  pairs(emmeans(mod7, ~ plot_status |
                  Biome, type = "response")) %>% confint %>%
  data.frame() %>%
  mutate(class2 = "invasive vs. native")


# model 8: non-native vs native pj biome ----------------------------------
mod8 <- lmer(pielous_j  ~ plot_status*Biome + (1 | cell), 
              data = dt_nonvsnat_plots_divs)
summary(mod8)

contrast_mod8 <-
  pairs(emmeans(mod8, ~ plot_status |
                  Biome, type = "response")) %>% confint %>%
  data.frame() %>% 
  mutate(class2 = "non-native vs. native")



# summary fig -------------------------------------------------------------
d_models_pj <- bind_rows(list(contrast_mod5,
                           contrast_mod6,
                           contrast_mod7,
                           contrast_mod8
))


# add sample sizes to biomes
sample_size_pj <- bind_rows(
  list(
    # invasives
    # all
    dt_invvsnat_plots_divs %>% 
      filter(!is.na(pielous_j)) %>% 
      summarize(cells = n_distinct(cell),
                plots = n_distinct(PlotObservationID)) %>%
      mutate(Biome = "All",
             class2 = "invasive vs. native"),
    # biomes
    dt_invvsnat_plots_divs %>%
      filter(!is.na(pielous_j)) %>% 
      group_by(Biome) %>%
      summarize(cells = n_distinct(cell),
                plots = n_distinct(PlotObservationID)) %>%
      mutate(class2 = "invasive vs. native"),
    
    # non-natives
    # all
    dt_nonvsnat_plots_divs %>% 
      filter(!is.na(pielous_j)) %>% 
      summarize(cells = n_distinct(cell),
                plots = n_distinct(PlotObservationID)) %>%
      mutate(Biome = "All",
             class2 = "non-native vs. native"),
    # biomes
    dt_nonvsnat_plots_divs %>%
      filter(!is.na(pielous_j)) %>% 
      group_by(Biome) %>%
      summarize(cells = n_distinct(cell),
                plots = n_distinct(PlotObservationID)) %>%
      mutate(class2 = "non-native vs. native")
  )
)


d_models_pj <- full_join(d_models_pj, sample_size_pj)
d_models_pj$sample_size_pj <- paste0("n = ", d_models_pj$cells, "; ", 
                                     d_models_pj$plots)

# plot
(ggplot(d_models_pj,
       aes(
         x = estimate,
         y = Biome,
         col = Biome
       )) +
  facet_wrap( ~ class2, nrow = 1, scales = "free_x") +
  geom_point() +
  geom_linerange(aes(xmin = lower.CL, xmax = upper.CL)) +
  geom_text(inherit.aes = F,
            data = d_models_pj,
            aes(x = Inf, y = Biome, label = sample_size_pj),
            vjust = -0.5, hjust = 1, size = 2.5,
            fontface = "italic",
            family = "Roboto Condensed",
            check_overlap = TRUE) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(y = "", x = "Pielou's J invaded plot - native plot") +
  scale_color_manual(values = c("black", cc)) +
  scale_y_discrete(limits = rev) +
  theme_ipsum(grid = "X",
              axis_title_just = "mm",
              axis_text_size = 10,
              base_family = "Roboto Condensed") +
  theme(plot.margin = unit(c(0.3,0,0,0), "cm"),
        axis.title.x = element_text(size=12)) +
  labs(title = "Evenness") +
  guides(colour = "none") -> f2b)


# save
showtext_opts(dpi=600)
ggsave(
  "Figures/figure3.png",
  height= 4.55,
  width = 9.86,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)


# combined plot -----------------------------------------------------------
f2a / f2b + plot_annotation(tag_levels = 'a')

# save
showtext_opts(dpi=600)
ggsave(
  "Figures/figure3.png",
  height= 6.97,
  width = 8.96,
  dpi = 600,
  bg = "white"
)
showtext_opts(dpi=96)
