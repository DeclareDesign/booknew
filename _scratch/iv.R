rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

design <-
  declare_population(N = 100, u = rnorm(N)) +
  declare_potential_outcomes(D ~ if_else(Z + u > 0, 1, 0), assignment_variables = Z) + 
  declare_potential_outcomes(Y ~ 0.1 * D + 0.25 + u, assignment_variables = D) +
  declare_estimand(late = mean(Y_D_1[D_Z_1 == 1 & D_Z_0 == 0] - Y_D_0[D_Z_1 == 1 & D_Z_0 == 0])) +
  declare_assignment(prob = 0.5) +
  declare_reveal(D, Z) + 
  declare_reveal(Y, D) + 
  declare_estimator(Y ~ D | Z, model = iv_robust, estimand = "LATE") 

draw_data(design)

dag <- dagify(Y ~ D + u,
              D ~ Z + u)
get_design_nodes(design)



nodes <-
  tibble(
    name = c("Z", "D", "u", "Y"),
    label = c("Z", "D", "u", "Y"),
    annotation = c(
      "**Exogenous variable**<br>Instrument",
      "**Endogenous variable**",
      "**Unknown heterogeneity**",
      "**Outcome**"
    ),
    x = c(-75,-25,0, 25),
    y = c(0, 0, 50, 0),
    text_x = x,
    nudge_y = c(25, -18, 18, -18),
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

