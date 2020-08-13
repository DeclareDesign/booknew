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
    group = add_level(N = 10, X = rnorm(N)),
    unit = add_level(N = 100, U = rnorm(N))
  ) +
  declare_assignment(clusters = group, conditions = c("low", "high"), assignment_variable = S) +
  declare_step(S_prob = case_when(S == "low" ~ 0.25, S == "high" ~ 0.75), mutate) +
  declare_assignment(blocks = group, prob_unit = S_prob) + 
  declare_potential_outcomes(
    Y ~ Z + (S == "high") + Z*(S == "high") + X + U,
    conditions = list(Z = c(0, 1), S = c("low", "high"))) +
  declare_estimand(ATE_saturation = mean(Y_Z_0_S_high - Y_Z_0_S_low),
                   ate_no_spill = mean(Y_Z_1_S_low - Y_Z_0_S_low)) +
  declare_reveal(Y, c(Z, S)) +
  declare_estimator(Y ~ Z + S, model = lm_robust, term = c("Z", "Shigh"), estimand = c("ATE_saturation", "ate_no_spill"), label = "main effect")

dag <- dagify(Y ~ Z + S + U + X,
              Z ~ S)

nodes <-
  tibble(
    name = c("X", "U", "S", "Z", "Y"),
    label = c("X", "U", "S", "Z", "Y"),
    annotation = c(
      "**Unknown heterogeneity**<br>Unit effects",
      "**Unknown heterogeneity**",
      "**Treatment assignment 1**<br>Saturation level",
      "**Treatment assignment 2**<br>Individual assignment",
      "**Outcome variable**"
    ),
    x = c(10, 10, -25, -25, 50),
    y = c(-50, 50, 25, -25, 0), 
    text_x = x,
    nudge_y = c(-10, 10, 18, -18, 18),
    text_y = y + nudge_y
  )

ggdd_df <- make_ggdd_df(dag, nodes, design)

dag_gg %+% ggdd_df
