rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

cutoff <- 0.5
control <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(.7, -.8, .5, 1))}
treatment <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(0, -1.5, .5, .8)) + .15}

design <-
  declare_population(
    N = 1000,
    U = rnorm(N, 0, 0.1),
    X = runif(N, 0, 1) + U - cutoff,
    Z = 1 * (X > 0)
  ) +
  declare_potential_outcomes(Y ~ Z * treatment(X) + (1 - Z) * control(X) + U) +
  declare_estimand(LATE = treatment(0) - control(0)) +
  declare_reveal(Y, Z) +
  declare_estimator(Y ~ poly(X, 4) * Z, model = lm_robust, estimand = "LATE")

dag <- dagify(Y ~ Z + X + U,
              Z ~ X,
              X ~ U)

get_design_nodes(design)



nodes <-
  tibble(
    name = c("U", "X", "Z", "Y"),
    label = c("U", "X", "Z", "Y"),
    annotation = c(
      "**Unknown heterogeneity**",
      "**Exogenous variable**",
      "**Treatment assignment**",
      "**Outcome variable**"
    ),
    x = c(-25, -75, -25, 25),
    y = c(25, 0, -25, 0), 
    text_x = x,
    nudge_y = c(18, -18, -18, -18),
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

