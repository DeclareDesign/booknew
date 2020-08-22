# ---
# Diagnosis
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(ggforce)

design <-
  declare_population(N = 100, 
                     Tau = rnorm(N, mean = 0.1, sd = 0.1),
                     U = rnorm(N, 0, 0.2)) +
  declare_potential_outcomes(Y ~  Tau * Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y ~ Z, estimand = "ATE")

## run_design(design)

include_graphics("figures/diagnosis_onedraw.png")

knitr::include_graphics("figures/diagnosis.png")

## diagnosis <- diagnose_design(design, sims = 1000,
##                              diagnosands = declare_diagnosands(
##                                select = c("bias", "sd_estimate", "rmse", "power", "coverage")))





diagnosis

knitr::include_app('https://ushintsho.shinyapps.io/diagnosis/', height = '400px')

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
