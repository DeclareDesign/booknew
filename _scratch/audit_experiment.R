rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

design <- 
  declare_population(N = 100,
                     U = rnorm(N)) +
  declare_potential_outcomes(R ~ if_else(Z + U > 0.5, 1, 0), conditions = list(Z = c(0, 1))) +
  declare_potential_outcomes(Q ~ if_else(R == 1, Z + U, NA_real_), conditions = list(Z = c(0, 1), R = c(0, 1))) +
  declare_estimand(ATE_R = mean(R_Z_1 - R_Z_0)) + 
  declare_estimand(CATE_ar = mean(Q_Z_1_R_1 - Q_Z_0_R_1), subset = (R_Z_1 == 1 & R_Z_0 == 0)) + 
  declare_assignment(prob = 0.5) +
  declare_reveal(R, Z) +
  declare_reveal(Q, c(Z, R)) +
  declare_estimator(R ~ Z, estimand = "ATE_R", label = "ATE_R") +
  declare_estimator(Q ~ Z, subset = (R == 1), estimand = "CATE_ar", label = "CATE_ar")

draw_data(design)

dag <- dagify(
  Q ~ R + Z + U,
  R ~ Z + U
)
get_design_nodes(design)



nodes <-
  tibble(
    name = c("Z", "R", "Q", "U"),
    label = c("Z", "R", "Q", "U"),
    annotation = c(
      "**Random assignment**<br> Subject sent email",
      "**Outcome 1**<br>Subject replies to email",
      "**Outcome 2**<br>Email quality",
      "**Unknown heterogeneity**"),
    x = c(-0.25, 0, 0, 0.25),
    y = c(0, 0.25, -.25, 0),
    text_x = x,
    text_y = c(0+ 0.2, 0.25+ 0.2, -.25 - 0.2, 0+ 0.2)
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
  coord_cartesian(ylim = c(-1, 1), 
                  xlim = c(-1, 1), clip = "off") + 
  theme_dag() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(override.aes = list(size=4)))
g

