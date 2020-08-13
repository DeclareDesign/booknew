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
    N = 100,
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = 0.2)),
    X = rbinom(N, size = 3, prob = 0.5)
  ) +
  declare_estimand(proportion = mean(Y_star)) +
  declare_measurement(Y_direct = Y_star - S) +
  declare_potential_outcomes(Y_list ~ Y_star * Z + X) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y_direct ~ 1,
                    model = lm_robust,
                    estimand = "proportion",
                    label = "direct") +
  declare_estimator(Y_list ~ Z, estimand = "proportion", label = "list")

draw_data(design)

dag <- dagify(Y_direct ~ Y_star + S,
              Y_list ~ Y_star + X + Z,
              S ~ U,
              X ~ U,
              Y_star ~ U)
get_design_nodes(design)



nodes <-
  tibble(
    name = c("U", "S", "Y_star", "X", "Y_direct", "Y_list", "Z"),
    label = c(
      "U",
      "S",
      "Y<sup>*</sup>",
      "X",
      "Y<sup>D</sup>",
      "Y<sup>L</sup>",
      "Z"
    ),
    annotation = c(
      "**Unknown heterogeneity**",
      "**Sensitivity bias**",
      "**Latent**<br> Sensitive trait",
      "Control item count",
      "**Outcome 1**<br> Direct question",
      "**Outcome 2**<br> List question",
      "**Random assignment**<br>List experiment condition"
    ),
    
    x = c(-75,-25,-25,-25, 25, 25, 75),
    y = c(0, 50, 0, -50, 50,-50,-50),
    text_x = x,
    nudge_y = c(18, 18, 18, 18, 18, 18, 18),
    text_y = y + nudge_y
  )

ggdd_df <- make_ggdd_df(dag, nodes, design)



g <-
  ggplot(ggdd_df, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
  )) +
  geom_point(
    stroke = .5,
    color = graemesgray,
    aes(shape = data_strategy, fill = data_strategy),
    size = 15
  ) +
  geom_dag_edges(
    edge_colour = graemesgray,
    edge_width = .4,
    arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed"),
  ) +
  geom_richtext(
    color = graemesgray,
    aes(label = label),
    fill = NA,
    label.color = NA,
    # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 4
  ) +
  geom_richtext(
    color = graemesgray,
    aes(label = annotation, x = text_x, y = text_y),
    size = 2,
    fill = NA,
    label.color = NA,
    # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  coord_cartesian(ylim = c(-100, 100),
                  xlim = c(-100, 100),
                  clip = "off") +
  theme_dag() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(override.aes = list(size = 4)))
g
