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
    unit = add_level(N = 2, X = rnorm(N, sd = 0.5)),
    period = add_level(N = 2, nest = FALSE),
    unit_period = cross_levels(by = join(unit, period), U = rnorm(N, sd = 0.01))
  ) + 
  declare_potential_outcomes(Y ~ X + 0.5 * as.numeric(period) + Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), subset = period == 2) + 
  declare_step(Z = X == max(X), handler = mutate) + 
  declare_reveal(Y = if_else(Z == 0 | period == 1, Y_Z_0, Y_Z_1), handler = mutate) +
  declare_estimator(Y ~ Z * period, model = lm_robust, se_type = "none")

dag <- dagify(Y ~ X + period + Z + U,
              Z ~ X)

get_design_nodes(design)

nodes <-
  tibble(
    name = c("U", "X", "period", "Z", "Y"),
    label = c("U", "X", "T", "Z", "Y"),
    annotation = c(
      "**Unknown heterogeneity**",
      "**Unit effect**",
      "**Time period**",
      "**Treatment assignment**",
      "**Outcome variable**"
    ),
    x = c(50, -25, -25, 0, 50),
    y = c(-50, 50, -50, 0, 0), 
    text_x = x,
    nudge_y = c(-10, 10, -10, -18, 18),
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

