rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

direct_effect_of_encouragement <- 0.0
proportion_defiers <- 0.0


design <-
  declare_population(
    N = 100,
    type = sample(
      x = c("Always-Taker", "Never-Taker", "Complier", "Defier"),
      prob = c(0.1, 0.1, 0.8, 0.0),
      size = N, replace = TRUE
    ),
    U = rnorm(N)
  ) +
  declare_potential_outcomes(
    D ~ case_when(
      Z == 1 & type %in% c("Always-Taker", "Complier") ~ 1,
      Z == 1 & type %in% c("Never-Taker", "Defier") ~ 0,
      Z == 0 & type %in% c("Never-Taker", "Complier") ~ 0,
      Z == 0 & type %in% c("Always-Taker", "Defier") ~ 1
    )
  ) +
  declare_potential_outcomes(
    Y ~ 0.5 * (type == "Complier") * D +
      0.25 * (type == "Always-Taker") * D +
      0.75 * (type == "Defier") * D +
      # Building in NO excludability violation
      0 * Z + U,
    assignment_variables = c("D", "Z")
  ) +
  declare_estimand(CACE = mean(Y_D_1_Z_1 - Y_D_0_Z_0),
                   subset = type == "Complier") +
  declare_assignment(prob = 0.5) +
  declare_reveal(D, assignment_variable = "Z") +
  declare_reveal(Y, assignment_variables = c("D", "Z")) +
  declare_estimator(Y ~ D | Z, model = iv_robust, estimand = "CACE")

draw_data(design)

dag <- dagify(Y ~ D + type + U,
              D ~ Z + type + U,
              type ~ U)

nodes <-
  tibble(
    name = c("Z", "D", "U", "Y", "type"),
    label = c("Z", "D", "U", "Y", "C"),
    annotation = c(
      "**Random assignment**",
      "**Treatment received**",
      "**Unknown heterogeneity**",
      "**Outcome**",
      "**Principal stratum**<br>Compliance type"
    ),
    x = c(-50,0,-25, 50, 25),
    y = c(0, 0, 50, 0, 50),
    text_x = x,
    nudge_y = c(18, -18, 18, -18, 18),
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

