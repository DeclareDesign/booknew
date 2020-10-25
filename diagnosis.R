# ---
# Diagnosis
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(ggforce)
library(ggridges)

dag <-
  dagify(aw ~ w + I,
         m ~ M,
         am ~ m + I,
         d ~ D + m,
         ad ~ A + d,
         w ~ W)

dag_base <- tidy_dagitty(dag) %>%
  select(name, direction, to, circular) %>%
  as_tibble

nodes_df <-
  tibble(
    name = c("M", "I", "D", "A", "m", "am", "aw", "d", "ad", "w", "W"),
    label = c("M", "I", "D", "A", "m", "a<sup>m</sup>", "a<sup>w</sup>", "d", "a<sup>d</sup>", "w", "W"),
    long_label = c("Theoretical<br>causal model", "Inquiry", "Data<br>strategy", "Answer<br>strategy", "Model<br>draw", "Theoretical<br>answer", "True answer", "Simulated<br>data", "Simulated<br>answer", "Real<br>world", "True<br>causal model"),
    lbl_direction = c("N", "N", "N", "N", "S", "S", "S", "S", "S", "S", "N"),
    x = c(1, 2, 5.5, 6.5, 1, 2, 4.25, 5.5, 6.5, 3.25, 3.25),
    y = c(3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 3)
  )

endnodes_df <-
  nodes_df %>%
  transmute(to = name, xend = x, yend = y)

gg_df <-
  dag_base %>%
  left_join(nodes_df, by = "name") %>%
  left_join(endnodes_df, by = "to")

gg_df <-
  gg_df %>%
  mutate(arced1 = (name == "m" & to == "d"),
         arced2 = (name == "I" & to == "aw")) %>%
  arrange(name)

rect_df <-
  tibble(
    xmin = c(.4, 4.9),
    xmax = c(2.6, 7.1),
    ymin = c(1.15, 1.15),
    ymax = c(3.85, 3.85)
  )

g <-
  ggplot(data = filter(gg_df, !arced1 & !arced2), aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_point(color = gray(.1), fill = NA, size = 15, stroke = 0.5, pch = 1) +
  geom_dag_edges(edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced1), curvature = -0.575, edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced2), curvature = .7, edge_width = 0.35) +
  geom_richtext(color = "black",
                parse = TRUE,
                aes(label = label),
                fill = NA,
                label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt"),
                size = 4) +
  geom_richtext(
    aes(y = y + if_else(lbl_direction == "N", 0.4, -0.4),
        vjust = if_else(lbl_direction == "N", "bottom", "top"),
        label = long_label),
    color = gray(0.5),
    parse = TRUE,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 4) +
  coord_fixed(ylim = c(0.5, 4)) + 
  geom_rect(data = rect_df, aes(x = NULL, y = NULL, 
                                xend = NULL, yend = NULL,
                                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.15) +
  annotate("text", x = 1.5, y = 4.05, label = "Theory") +
  annotate("text", x = 6, y = 4.05, label = "Simulation") +
  annotate("text", x = 3.75, y = 4.05, label = "Reality") +
  # annotate("text", x = 3, y = 1.6, label = "Truth") +
  theme_dag()
g

design <-
  declare_population(N = 100, 
                     Tau = rnorm(N, mean = 0.1, sd = 0.1),
                     U = rnorm(N, 0, 0.2)) +
  declare_potential_outcomes(Y ~  Tau * Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y ~ Z, estimand = "ATE")

## run_design(design)

design <-
  declare_population(N = 100, U = rnorm(N, 0, 0.2)) +
  declare_potential_outcomes(Y ~ rnorm(N, mean = 0.1, sd = 0.1) * Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment() +
  declare_estimator(Y ~ Z, estimand = "ATE", model = lm_robust)

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

simulation_1 <- simulations %>% filter(sim_ID == 1)

simulation_1 %>% 
  select(estimand, estimate, std.error, conf.low, conf.high, p.value) %>%
  knitr::kable(digits = 3)

gg_df_1 <- gg_df %>% filter(sim_ID == 1)

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

## diagnosis <- diagnose_design(design, sims = 1000,
##                              diagnosands = declare_diagnosands(
##                                select = c("bias", "sd_estimate", "rmse", "power", "coverage")))





diagnosis

n <- 50

points_df <- tibble(rho = sqrt(runif(n)),
                    theta = runif(n, 0, 2*pi),
                    x = rho * cos(theta),
                    y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

unbiased_lowprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Unbiased, imprecise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

n <- 50

points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta),
  y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

unbiased_highprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Unbiased, imprecise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

n <- 50

points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta) + 0.5,
  y = rho * sin(theta) - 0.2
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

biased_highprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Unbiased, imprecise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

library(patchwork)

unbiased_lowprecision + unbiased_highprecision + biased_highprecision

n <- 50

points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta),
  y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias_1 = round(mean(sqrt(x^2 + y^2)), 2),
    mse_1 = round(mean(x^2 + y^2), 2),
    var_1 = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3),
    
    bias_2 = round(mean(sqrt((x-0.25)^2 + y^2)), 2),
    mse_2 = round(mean((x-0.25)^2 + y^2), 2),
    var_2 = round(mean(((x-0.25) - mean(x-0.25))^2 + (y - mean(y))^2), 3)
  )

circle_df <- 
  bind_rows(
    "left" = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ),
    "right" = tibble(
      x0 = rep(0.35, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ),
    .id = "side"
  ) %>% 
  arrange(desc(r), side)

dual_estimands <- ggplot() + 
  geom_circle(
    data = circle_df, 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Diagnosands depend on the estimand", subtitle = glue::glue("Variance = {summary_df$var_1} (left), {summary_df$var_2} (right)\nBias = {summary_df$bias_1} (left), {summary_df$bias_2} (right)\nRMSE = {summary_df$mse_1} (left), {summary_df$mse_2} (right)")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
dual_estimands

## robustness_checks_design <-
##   robustness_checks_design +
##   declare_estimator(handler = label_estimator(interacted_correlation_decision), label = "interacted")
## 
## robustness_checks_design_dgp2 <- replace_step(
##   robustness_checks_design,
##   step = 1,
##   new_step =
##     declare_population(
##       N = 100,
##       x = rnorm(N),
##       y1 = rnorm(N),
##       y2 = 0.15 * y1 + 0.01 * x + 0.05 * y1 * x + rnorm(N)
##     )
## )
## 
## robustness_checks_design_dgp3 <- replace_step(
##   robustness_checks_design,
##   step = 1,
##   new_step =
##     declare_population(
##       N = 100,
##       x = rnorm(N),
##       y1 = 0.15 * x + rnorm(N),
##       y2 = 0.15 * x + rnorm(N)
##     )
## )
## 
## robustness_checks_design_dgp3 <- replace_step(
##   robustness_checks_design_dgp3,
##   step = 2,
##   new_step = declare_estimand(y1_y2_are_related = FALSE)
## )
## 
## decision_diagnosis <- declare_diagnosands(correct = mean(decision == estimand), keep_defaults = FALSE)
## 
## diag <- diagnose_design(
##   robustness_checks_design, robustness_checks_design_dgp2, robustness_checks_design_dgp3,
##   sims = sims, diagnosands = decision_diagnosis)
