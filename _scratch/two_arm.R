rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")


design <-
  declare_population(N = 100, U = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + U) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y ~ Z, estimand = "ATE")

# dagify(
#   Y ~ Z + U
# )

get_design_nodes(design)

dag <- get_dag(design)

# plot(design)
# plot(dag)

nodes <-
  tibble(
    name = c("Y", "Z", "U"),
    label = c("Y", "Z", "U"),
    annotation = c(
      "**Outcome**<br>",
      "**Random assignment**<br>",
      "**Unknown heterogeneity**"),
    x = c(25, -25, 25),
    y = c(-25, -25, 25), 
    text_x = x,
    nudge_y = c(-18, -18, 18),
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
  geom_point(stroke = .5,
             color = graemesgray,
             aes(shape = data_strategy, fill = data_strategy),
             size = 15) +
  geom_dag_edges(edge_colour = graemesgray, edge_width = .4,
                 arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed"),
  ) +
  geom_richtext(color = graemesgray,
                aes(label = label),
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt"),
                size = 4) +
  geom_richtext(color = graemesgray,
                aes(label = annotation, x = text_x, y = text_y),
                size = 2,
                fill = NA, label.color = NA, # remove background and outline
                label.padding = grid::unit(rep(0, 4), "pt")) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  coord_cartesian(ylim = c(-100, 100),
                  xlim = c(-100, 100),
                  clip = "off") +
  theme_dag() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(override.aes = list(size=4)))
g

