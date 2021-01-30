library(DeclareDesign)
library(tidyverse)
library(ggridges)

dd_dark_blue <- "#3564ED"
dd_light_blue <- "#72B4F3"
dd_dark_blue_alpha <- "#3564EDA0"
dd_light_blue_alpha <- "#72B4F3A0"
dd_pink <- "#D43981"


dd_gray <- gray(0.2)

design <-
  declare_model(N = 100, U = rnorm(N, 0, 0.2)) +
  declare_potential_outcomes(Y ~ rnorm(N, mean = 0.1, sd = 0.1) * Z + U) + 
  declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment() +
  declare_estimator(Y ~ Z, inquiry = "ATE", model = lm_robust)

# diagnose_design(design, sims = 250)

set.seed(343)
simulations <- 
  simulate_design(design, sims = 10)   %>%
  mutate(significant = p.value <= 0.05)


gg_df <-
  simulations %>%
  group_by(sim_ID) %>%
  nest %>%
  mutate(densities = lapply(data, function(df) {
    with(df, tibble(
      x = seq(-2, 2, 0.0001),
      density = dnorm(x, mean = estimate, sd = std.error)
    ))
  })) %>%
  unnest(cols = c(data, densities)) %>%
  group_by(sim_ID)


gg_df <- 
  gg_df %>%
  mutate(
    
    low_cut = case_when(
      estimate < 0 ~ x < -2*abs(estimate),
      estimate > 0 ~ x <= 0),
    
    high_cut = case_when(estimate < 0 ~ x >= 0,
                         estimate > 0 ~ x >= 2*abs(estimate) ),
    
    area_under = case_when(low_cut ~ "low",
                           high_cut ~ "high",
                           TRUE ~ "middle")
  ) 
  



g <- 
ggplot(simulations, aes(x = estimate, y = as.factor(sim_ID)), color = dd_light_blue) +
  # geom_rect(xmin = -10, xmax = 0, ymin = -100, ymax = 100, fill = gray(0.96), size = 0) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.075,
                 size = 0.25,
                 color = gray(0.5)) +
  geom_point(size = 1.5, color = gray(0.5)) +
  geom_point(
    aes(x = estimand),
    size = 1.5,
    fill = NA,
    color = gray(0.5),
    pch = 24,
    position = position_nudge(y = -0.1)
  ) +
  # first plot the density for p-values
  geom_ridgeline(
    data = gg_df,
    size = 0,
    aes(x = x, height = density, fill = area_under),
    scale = 0.05,
    min_height = 0.01
  ) +
  # then the density lines (to make it plot correctly and not be cut off)
  geom_ridgeline(
    data = gg_df,
    size = 0.1,
    aes(x = x, height = density, fill = NA),
    scale = 0.05,
    min_height = 0.01,
    color = dd_light_blue,
  ) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.1, 0.25)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = 0.1,
    color = dd_light_blue,
    linetype = "dotted",
    alpha = 0.5
  ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_light_blue_alpha, dd_light_blue_alpha, NA)) +
  labs(x = "Estimated sampling distribution (from point estimate and standard error)")

ggsave("diagnosis.pdf", g, height = 3.5, width = 5)




simulation_1 <- simulations %>% filter(sim_ID == 1)


simulation_1 %>% 
  select(estimand, estimate, std.error, conf.low, conf.high, p.value) %>%
  knitr::kable(digits = 3, booktabs = TRUE)

gg_df_1 <- gg_df %>% filter(sim_ID == 1)

g_1 <- 
  ggplot(simulation_1, aes(x = estimate, y = as.factor(sim_ID)), color = dd_light_blue) +
  # geom_rect(xmin = -10, xmax = 0, ymin = -100, ymax = 100, fill = gray(0.96), size = 0) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.075,
                 size = 0.25,
                 color = gray(0.5)) +
  geom_point(size = 1.5, color = gray(0.5)) +
  geom_point(
    aes(x = estimand),
    size = 1.5,
    fill = NA,
    color = gray(0.5),
    pch = 24,
    position = position_nudge(y = -0.1)
  ) +
  # first plot the density for p-values
  geom_ridgeline(
    data = gg_df_1,
    size = 0,
    aes(x = x, height = density, fill = area_under),
    scale = 0.05,
    min_height = 0.01
  ) +
  # then the density lines (to make it plot correctly and not be cut off)
  geom_ridgeline(
    data = gg_df_1,
    size = 0.1,
    aes(x = x, height = density, fill = NA),
    scale = 0.05,
    min_height = 0.01,
    color = dd_light_blue,
  ) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.1, 0.25)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  # geom_vline(
  #   xintercept = 0.1,
  #   color = dd_light_blue,
  #   linetype = "dotted",
  #   alpha = 0.5
  # ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_light_blue_alpha, dd_light_blue_alpha, NA)) +
  labs(x = "Estimated sampling distribution (from point estimate and standard error)")

ggsave("diagnosis_onedraw.pdf", g_1, height = 1.5, width = 5)
