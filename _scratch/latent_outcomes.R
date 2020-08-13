rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("_scratch/make_ggdd_df.R")

design <- 
  declare_population(N = 100, Y_star = rnorm(N)) +
  declare_estimand(Y_bar = mean(Y_star)) + 
  declare_measurement(Y_1 = 0.1 * Y_star + rnorm(N, sd = 0.25),
                      Y_2 = Y_star + rnorm(N, sd = 0.25),
                      Y_3 = 1 + 0.5 * Y_star + rnorm(N, sd = 0.25),
                      Y_idx = (Y_1 + Y_2 + Y_3) / 3) + 
  declare_estimator(Y_idx ~ 1, model = lm_robust, estimand = "Y_bar")

dag <- dagify(Y_1 ~ Y_star, Y_2 ~ Y_star, Y_3 ~ Y_star, Y_idx ~ Y_1 + Y_2 + Y_3)
nodes <- get_design_nodes(design)

nodes <-
  tibble(
    name = c("Y_star", "Y_1", "Y_2", "Y_3", "Y_idx"),
    label = c("Y^*", "Y<sup>1</sup>", "Y<sup>2</sup>", "Y<sup>3</sup>", "Index"),
    annotation = c(
      "**Latent outcome**<br>unmeasurable",
      "**Measured outcome**",
      "",
      "",
      "**Constructed index**"),
    x = c(-50, 0, 0, 0, 50),
    y = c(0, 25, 0, -25, 0),
    text_x = x,
    text_y = c(25, 50, 0, -25, 25),
  )

ggdd_df <- make_ggdd_df(dag, nodes, design)

base_dag_plot %+% ggdd_df
