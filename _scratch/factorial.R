rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

design <-
  declare_population(N = 1000, U = runif(N)) +
  declare_potential_outcomes(
    # Difference 1: 0.6 - 0.4 = 0.2
    Y_Z_1 = if_else(0.4 < U, 1, 0),
    Y_Z_2 = if_else(0.6 < U, 1, 0),
    # Difference 2: 0.6 - 0.5 = 0.1
    Y_Z_3 = if_else(0.5 < U, 1, 0),
    Y_Z_4 = if_else(0.6 < U, 1, 0)
    # Difference-in-differences (also called the interaction effect): 0.2 - 0.1 = 0.1
  ) +
  declare_assignment(conditions = 1:4) +
  declare_assignment(Z1 = as.numeric(Z %in% c(2, 4)),
                     Z2 = as.numeric(Z %in% c(3, 4)),
                     handler = mutate) +
  declare_reveal(Y, Z) +
  declare_estimator(Y ~ Z1 + Z2 + Z1 * Z2,
                    model = lm_robust,
                    term = "Z1:Z2")

# diagnosis <-
#   design %>%
#   redesign(N = c(500, 1000, 3000, 5000)) %>%
#   diagnose_design(sims = 500, bootstrap_sims = FALSE)
# 
# diagnosis %>%
#   get_diagnosands() %>%
#   ggplot(aes(N, power)) +
#   geom_line() +
#   geom_hline(yintercept = 0.8, linetype = "dashed") +
#   theme_bw() +
#   ggtitle("Power for the interaction term in a 2x2 factorial experiment",
#           "When the difference in the effect of factor 1 depending on the level of factor 2 is 10pp")

dag <-
  dagify(Y ~ Z1 + Z2 + U)

get_design_nodes(design)

dag <- get_dag(design)

# plot(design)
# plot(dag)

nodes <-
  tibble(
    name = c("Y", "Z1", "Z2", "U"),
    label = c("Y", "Z1", "Z2", "U"),
    annotation = c(
      "**Outcome**<br>",
      "**Random assignment 1**<br> Factor number 1",
      "**Random assignment 2**<br> Factor number 2",
      "**Unknown heterogeneity**"),
    x = c(0, -50, -50, 50),
    y = c(0, -50, 50, 0), 
    text_x = x,
    nudge_y = c(18, 18, 18, 18),
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

