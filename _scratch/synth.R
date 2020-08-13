rm(list = ls())
library(tidyverse)
library(DeclareDesign)
library(dagitty)
library(ggdag)
library(dddag)
library(ggtext)
source("~/Dropbox/declaredesign_AEC/dag_scratch/make_ggdd_df.R")

library(Synth)

synth_weights_tidy <- function(data, predictors, time.predictors.prior, dependent, unit.variable, time.variable, treatment.identifier, controls.identifier) {
  dataprep.out <- dataprep(
    foo = data,
    predictors = predictors,
    predictors.op = "mean",
    time.predictors.prior = time.predictors.prior,
    dependent = dependent,
    unit.variable = unit.variable,
    time.variable = time.variable,
    treatment.identifier = treatment.identifier,
    controls.identifier = controls.identifier, 
    time.optimize.ssr = time.predictors.prior,
    time.plot = time.predictors.prior)
  capture.output(fit <- synth(data.prep.obj = dataprep.out))
  tab <- synth.tab(dataprep.res = dataprep.out, synth.res = fit) 
  
  weights_df <- tab$tab.w %>% mutate(synth_weights = w.weights) %>% dplyr::select(synth_weights, !!unit.variable := unit.numbers)
  data %>%
    left_join(weights_df) %>%
    mutate(synth_weights = replace_na(synth_weights, 1))
}


design <- 
  declare_population(
    unit = add_level(N = 10, units = 1:N, X = rnorm(N, sd = 0.5)),
    period = add_level(N = 3, time = 1:N, nest = FALSE),
    unit_period = cross_levels(by = join(unit, period), U = rnorm(N))
  ) + 
  declare_potential_outcomes(Y ~ X + 0.5 * as.numeric(period) + Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0), subset = period == 3) + 
  declare_step(handler = mutate, Z = unit == "01") + 
  declare_reveal(Y = if_else(Z == 0 | period < 3, Y_Z_0, Y_Z_1), handler = mutate) +
  declare_step(predictors = "X",
               time.predictors.prior = 1:2,
               dependent = "Y",
               unit.variable = "units",
               time.variable = "time",
               treatment.identifier = 1,
               controls.identifier = 2:10, 
               handler = synth_weights_tidy) +
  declare_estimator(Y ~ Z, subset = time >= 3, weights = synth_weights, model = lm_robust, label = "synth")



# Simulation --------------------------------------------------------------

# simulations <- simulate_design(design, sims = 100)
# ggplot(simulations, aes(estimate)) + geom_histogram()


# Synth plot --------------------------------------------------------------

# data <- draw_data(design)
# summary_df <- data %>%
#   group_by(Z, time) %>%
#   summarize(Y = weighted.mean(Y, w = synth_weights))
# ggplot(summary_df, aes(x = time, y = Y, color = Z)) +
#   geom_line(size = 2, alpha = 0.5) +
#   geom_line(data = data, aes(x = time, y = Y, group = units), color = "black", alpha = 0.3) +
#   geom_point(data = data, aes(x = time, y = Y, size = synth_weights^2), alpha = 0.3) +
#   geom_vline(xintercept = 2.5)

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

