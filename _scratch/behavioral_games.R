rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

# The proposer is tasked with splitting it with another player, the responder. Once the proposer communicates their decision, the responder may accept it or reject it. If the responder accepts, the money is split per the proposal; if the responder rejects, both players receive nothing. Both players know in advance the consequences of the responder accepting or rejecting the offer.

design <-
  declare_population(
    games = add_level(N = 100),
    players = add_level(
      N = 2,
      prosociality = runif(N),
      fairness = prosociality,
      cutoff = pmax(prosociality - 0.25, 0)
    )
  ) +
  declare_estimand(mean_fairness = mean(fairness),
                   mean_cutoff = mean(cutoff)) +
  declare_assignment(blocks = games, conditions = c("proposer", "responder"), assignment_variable = "role") + 
  declare_step(id_cols = games, names_from = role, values_from = c(prosociality, fairness, cutoff), handler = pivot_wider) + 
  declare_measurement(proposal = fairness_proposer * 0.5, 
                      response = if_else(proposal >= cutoff_responder, 1, 0)) + 
  declare_estimator(proposal ~ 1,
                    model = lm_robust,
                    estimand = "mean_fairness",
                    label = "mean_fairness") +
  declare_estimator(response ~ 1,
                    model = lm_robust,
                    estimand = "mean_cutoff",
                    label = "mean_cutoff")

draw_data(design)

dag <- dagify(fairness ~ prosociality,
              cutoff ~ prosociality,
              proposal ~ fairness + role,
              response ~ proposal + cutoff + role)

# plot(dag)           
              
# get_design_nodes(design)

nodes <-
  tibble(
    name = c("prosociality", "cutoff", "fairness", "role", "proposal", "response"),
    label = c("Y<sup>1*</sup>", "Y<sup>3*</sup>", "Y<sup>2*</sup>", "Z", "Y<sup>2</sup>", "Y<sup>1</sup>"),
    annotation = c(
      "**Latent trait**<br>Prosociality",
      "**Latent trait**<br>Cutoff",
      "**Latent trait**<br>Fairness",
      "**Random assignment**<br>Role",
      "**Outcome 1**<br>Proposal",
      "**Outcome 2**<br>Response"
    ),
    
    x = c(-100, -50,-50, -25, 25, 25) + 25,
    y = c(0, -50, 50, 0, 50, -50),
    text_x = x,
    nudge_y = c(18, 18, 18, 18, 18, -18),
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
