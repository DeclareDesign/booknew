rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

fixed_population <- declare_population(N = 500, 
                                       X = sample(c("A", "B", "C"), N, replace = TRUE),
                                       Y = sample(1:7, N, replace = TRUE))()

design <- 
  declare_population(data = fixed_population) + 
  declare_estimand(Ybar = mean(Y)) + 
  declare_sampling(strata_prob = c(0.2, 0.1, 0.3), strata = X) + 
  declare_step(B_demeaned = (X == "B") - mean(X == "B"),
               C_demeaned = (X == "C") - mean(X == "C"), mutate) + 
  declare_estimator(Y ~ B_demeaned + C_demeaned, term = "(Intercept)", model = lm_robust, estimand = "Ybar")

dag <- dagify(Y ~ S + X,
              S ~ X)
nodes <-
  tibble(
    name = c("X", "Y", "S"),
    label = c("X", "Y", "S"),
    annotation = c(
      "**Strata**<br>changes sampling probabilites",
      "**Outcome**<br>measured only for sampled units",
      "**Sampling indicator**<br>randomly set by designer"),
    x = c(-25, 25, -25),
    y = c(50, 0, 0),
    text_x = x,
    nudge_y = c(-15, -25, -25),
    text_y = y + nudge_y
  )



ggdd_df <- make_ggdd_df(dag, nodes, random_sampling_design)

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
  geom_text(color = graemesgray,
            parse = TRUE,
            aes(label = label),
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

