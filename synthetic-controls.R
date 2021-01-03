# ---
# Synthetic controls
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(Synth)
library(augsynth) # remotes::install_github("ebenmichael/augsynth")
library(glmnet)
library(sf)

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
  
  weights_df <- tab$tab.w %>% mutate(synth_weights = w.weights) %>% 
    dplyr::select(synth_weights, !!unit.variable := unit.numbers)
  
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
  reveal_outcomes(Y = if_else(Z == 0 | period < 3, Y_Z_0, Y_Z_1), 
                  handler = mutate) +
  declare_step(predictors = "X",
               time.predictors.prior = 1:2,
               dependent = "Y",
               unit.variable = "units",
               time.variable = "time",
               treatment.identifier = 1,
               controls.identifier = 2:10, 
               handler = synth_weights_tidy) +
  declare_estimator(Y ~ Z, subset = time >= 3, weights = synth_weights, 
                    model = lm_robust, label = "synth")

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
    
    x = c(5, 1, 1, 3, 5),
    y = c(1.5,3.5, 1, 2.5, 2.5), 
    nudge_direction = c("S", "N", "S", "S","N"),
    data_strategy = c("unmanipulated", "unmanipulated", "unmanipulated", "assignment", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df
