rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

fixed_population <- declare_population(N = 500, Y = sample(1:7, N, replace = TRUE))()

random_sampling_design <- 
  declare_population(data = fixed_population) + 
  declare_estimand(Y_bar = mean(Y)) + 
  declare_sampling(n = 100) + 
  declare_estimator(Y ~ 1, model = lm_robust, estimand = "Y_bar")

dag <- dagify(Y ~ S)
nodes <- get_design_nodes(random_sampling_design)



nodes <-
  tibble(
    name = c("Y", "S"),
    label = c("Y", "S"),
    annotation = c("**Outcome**<br>measured only for sampled units",
                   "**Sampling indicator**<br>randomly set by designer"),
    x = c(0.25, -0.25),
    y = c(0, 0),
    text_x = x,
    text_y = c(0.25, 0.25)
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
                  xlim = c(-1, 1)) + 
  theme_dag() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(override.aes = list(size=4)))
g

