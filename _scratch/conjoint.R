rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")


# applies the function to each pair
Y_function <- function(data) {
  data %>%
    group_by(pair) %>%
    mutate(Y = if_else(E == max(E), 1, 0)) %>%
    ungroup
}

dat <- tibble(pair = c("A", "A"), E = c(.5, .6))

dat %>% Y_function()

design <- 
  declare_population(
    subject = add_level(N = 500),
    pair = add_level(N = 4),
    candidate = add_level(N = 2, U = runif(N))
  ) +
  declare_assignment(assignment_variable = "A1") +
  declare_assignment(assignment_variable = "A2", 
                     conditions = c("young", "middle", "old")) +
  declare_assignment(assignment_variable = "A3")  +
  declare_step(
    E = 
      0.05 * A1 + 
      0.04 * (A2 == "middle") + 
      0.08 * (A2 == "old") + 
      0.02 * A3 + U,
    handler = fabricate) +
  declare_measurement(handler = Y_function) +
  declare_estimator(Y ~ A1 + A2 + A3,
                    model = lm_robust, term = TRUE)



dag <- dagify(Y ~ E,
              E ~ U + A1 + A2 + A3)
nodes <- get_design_nodes(design)
tidy_dagitty(dag)



nodes <-
  tibble(
    name = c("A1", "A2", "A3", "E", "Y", "U"),
    label = c("A1", "A2", "A3", "E", "Y", "U"),
    annotation = c("**Random assignment**<br>Coethnicity attribute",
                   "**Random assignment**<br>Age attribute",
                   "**Random assignment**<br>Gender attribute",
                   "**Latent outcome**<br>Candidate evaluation",
                   "**Measured outcome**<br>Candidate choice",
                   "**Unknown heterogeneity**"),
    x = c(-50, -50, -50, 0, 50, 0),
    y = c(50, 0, -50, 0, 0, 50),
    text_x = x,
    nudge_y = c(18, 18, 18, -18, 18, 18),
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

