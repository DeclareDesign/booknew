rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

design <- 
  declare_population(
    unit = add_level(N = 8, X = rnorm(N)),
    period = add_level(N = 3, time = as.numeric(period), nest = FALSE),
    obs = cross_levels(by = join(unit, period), U = rnorm(N))
  ) + 
  declare_potential_outcomes(Y ~ X + U + Z * time) +
  declare_assignment(clusters = unit, conditions = 1:4, assignment_variable = "wave") + 
  declare_step(Z = as.numeric(time >= wave), ipw = 1 / (Z * 2/8 + (1 - Z) * (1 - 2/8)), handler = fabricate) + 
  declare_reveal(Y, Z) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE", label = "1: Wave 2 only", subset = period == 2) +
  declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE", label = "2: Weighted, clustered SW", weights = ipw, clusters = unit) +
  declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE", label = "3: Unweighted, unclustered SW") 

draw_data(design)

dag <- dagify(Y ~ Z + X + U + time,
              Z ~ time)

nodes <-
  tibble(
    name = c("X", "U", "time", "Z", "Y"),
    label = c("X", "U", "T", "Z", "Y"),
    annotation = c(
      "**Unknown heterogeneity**<br>Unit effects",
      "**Unknown heterogeneity**",
      "**Time period**",
      "**Treatment assignment**",
      "**Outcome variable**"
    ),
    x = c(50, -25, -25, 0, 50),
    y = c(-50, 50, -50, 0, 0), 
    text_x = x,
    nudge_y = c(-10, 10, -10, 10, 18),
    text_y = y + nudge_y
  )

ggdd_df <- make_ggdd_df(dag, nodes, design)

dag_gg %+% ggdd_df

