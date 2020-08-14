# ---
# Synthetic controls
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)


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
    answer_strategy = "uncontrolled")
    
    

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df



# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(Synth)
library(augsynth) # remotes::install_github("ebenmichael/augsynth")
library(glmnet)
library(sf)

# tidy function that takes data and just adds the synthetic control weights to it
synth_weights_tidy <- function(data) {
  dataprep.out <- dataprep(
    foo = data,
    predictors = "prop_non_hispanic_below_hs",
    predictors.op = "mean",
    time.predictors.prior = 1998:2006,
    dependent = "prop_non_hispanic_below_hs",
    unit.variable = "state_number",
    time.variable = "year",
    treatment.identifier = 4,
    controls.identifier = c(1:3, 5:50), # states without Arizona
    time.optimize.ssr = 1998:2006,
    time.plot = 1998:2009)
  capture.output(fit <- synth(data.prep.obj = dataprep.out))
  tab <- synth.tab(dataprep.res = dataprep.out, synth.res = fit) 
  
  data %>% 
    left_join(tab$tab.w %>% mutate(synth_weights = w.weights) %>% dplyr::select(synth_weights, unit.numbers), by = c("state_number" = "unit.numbers")) %>% 
    mutate(synth_weights = replace(synth_weights, state_number == 4, 1))
}

augsynth_tidy <- function(data) {
  fit <- augsynth(prop_non_hispanic_below_hs ~ legal_worker_act, state, year, t_int = 2007, data = data)
  res <- summary(fit)$att %>% filter(Time == 2007) %>% select(Estimate, Std.Error)
  names(res) <- c("estimate", "std.error")
  res$p.value <- 2 * pt(-abs(res$estimate/res$std.error), df = nrow(data) - 15)
  res$conf.low <- res$estimate - 1.96 * res$std.error
  res$conf.high <- res$estimate + 1.96 * res$std.error
  res
}

# note need to clean up the range of the data, currently over 1
design <-
  declare_population(
    states = add_level(
      N = 50, 
      state = state.abb,
      state_number = as.numeric(as.factor(state)),
      state_shock = runif(N, -.15, .15),
      border_state = state %in% c("AZ", "CA", "NM", "TX"),
      state_shock = ifelse(border_state, .2, state_shock)
    ),
    years = add_level(
      N = 12, nest = FALSE,
      year = 1998:2009,
      post_treatment_period = year >= 2007,
      year_shock = runif(N, -.025, .025), 
      year_trend = year - 1998
    ),
    obs = cross_levels(
      by = join(states, years),
      # treatment indicator:
      legal_worker_act = if_else(post_treatment_period == TRUE & state == "AZ", 1, 0),
      state_year_shock = runif(N, -.025, .025),
      prop_non_hispanic_below_hs_baseline = 
        0.4 + state_shock + year_shock + (.01 + .05 * border_state) * year_trend + state_year_shock
    )
  ) +
  declare_potential_outcomes(
    prop_non_hispanic_below_hs ~ prop_non_hispanic_below_hs_baseline + 0.25 * legal_worker_act, 
    assignment_variable = legal_worker_act) +
  declare_estimand(
    ATE_AZ = mean(prop_non_hispanic_below_hs_legal_worker_act_1 - 
                    prop_non_hispanic_below_hs_legal_worker_act_0), 
    subset = legal_worker_act == TRUE) +
  declare_reveal(prop_non_hispanic_below_hs, legal_worker_act) +
  declare_step(handler = synth_weights_tidy) +
  declare_estimator(
    prop_non_hispanic_below_hs ~ legal_worker_act, 
    subset = year >= 2007, weights = synth_weights, model = lm_robust, label = "synth") +
  declare_estimator(
    prop_non_hispanic_below_hs ~ legal_worker_act, subset = year >= 2007, 
    model = lm_robust, label = "unweighted") +
  declare_estimator(
    prop_non_hispanic_below_hs ~ I(state == "AZ") + post_treatment_period + legal_worker_act, term = "legal_worker_act", 
    model = lm_robust, label = "unweighted_did") +
  declare_estimator(handler = tidy_estimator(augsynth_tidy), label = "augsynth")

state_data <- draw_data(design)

state_data %>% dplyr::select(state, synth_weights) %>% distinct %>% arrange(-synth_weights) %>% head

state_data %>% 
  ggplot() +
  geom_line(aes(year, prop_non_hispanic_below_hs)) +
  facet_wrap(~ state)

state_data %>% 
  mutate(treatment_state = factor(state == "AZ", levels = c(FALSE, TRUE), labels = c("Synthethic Control", "Arizona"))) %>% 
  group_by(treatment_state, year) %>% 
  summarize(prop_non_hispanic_below_hs = weighted.mean(prop_non_hispanic_below_hs, w = synth_weights)) %>% 
  ggplot(aes(x = year, y = prop_non_hispanic_below_hs, color = treatment_state)) +
  geom_line() + 
  geom_vline(xintercept = 2007) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  annotate("text", x = 2006.7, y = 1.7, label = "Law Introduced in 2007", hjust = "right", family = "Palatino") +
  labs(color = "") +
  xlab("") + ylab("Proportion Non-Hispanic Below H.S. Education") +
  dd_theme()





synth_diagnosands <- declare_diagnosands(select = c("bias", "rmse", "coverage"))

diagnosis <- diagnose_design(simulations, diagnosands = synth_diagnosands, bootstrap_sims = b_sims)

kable(reshape_diagnosis(diagnosis))

# declaration outside the convex hull
design_outside_hull <- replace_step(
  design, 
  step = 2, 
  new_step = declare_potential_outcomes(
    prop_non_hispanic_below_hs ~ prop_non_hispanic_below_hs_baseline + 0.25 * legal_worker_act + 0.2 * (state == "AZ"), 
    assignment_variable = legal_worker_act))

state_data_outside_hull <- draw_data(design_outside_hull)





diagnosis_outside_hull <- diagnose_design(simulations_outside_hull, diagnosands = synth_diagnosands, bootstrap_sims = b_sims)

kable(reshape_diagnosis(diagnosis_outside_hull))

# plot the synthetic control constructed in this way (it usually picks just texas and is highly biased)
state_data_outside_hull %>% 
  mutate(treatment_state = factor(state == "AZ", levels = c(FALSE, TRUE), labels = c("Synthethic Control", "Arizona"))) %>% 
  group_by(treatment_state, year) %>% 
  summarize(prop_non_hispanic_below_hs = weighted.mean(prop_non_hispanic_below_hs, w = synth_weights)) %>% 
  ggplot(aes(x = year, y = prop_non_hispanic_below_hs, color = treatment_state)) +
  geom_line() + 
  geom_vline(xintercept = 2007) + 
  scale_x_continuous(breaks = scales::pretty_breaks()) + 
  annotate("text", x = 2006.7, y = 1.7, label = "Law Introduced in 2007", hjust = "right", family = "Palatino") +
  labs(color = "") +
  xlab("") + ylab("Proportion Non-Hispanic Below H.S. Education") +
  dd_theme()

## calculate_in_hull <- function(data) {
##   # NB: cannot get this function to work (GB)
##   # control_data <- data %>%
##   #   filter(post_treatment_period == FALSE & state != "AZ") %>%
##   #   transmute(year, prop_non_hispanic_below_hs, ID = 1:n())
##   #
##   # chull <- tibble(ID = with(control_data, chull(year, prop_non_hispanic_below_hs))) %>%
##   #   left_join(control_data)
##   #
##   # chull <- bind_rows(chull, chull[1, ])
##   #
##   # chull_sf <- st_sf(geom = st_sfc(st_polygon(x = list(chull %>% select(year, prop_non_hispanic_below_hs) %>% as.matrix))), crs = st_crs(27700))
##   #
##   # treat_points <- data %>%
##   #   filter(post_treatment_period == FALSE & state == "AZ") %>%
##   #   select(year, prop_non_hispanic_below_hs) %>%
##   #   # bind_rows(tibble(year = 100000, prop_non_hispanic_below_hs = 99)) %>%
##   #   st_as_sf(coords = c("year", "prop_non_hispanic_below_hs"), crs = st_crs(27700))
##   #
##   # tt <- st_join(treat_points, chull_sf, join = st_intersects)
##   #
##   #   st_join(chull_sf, join = st_within)
##   #
##   # ggplot() + geom_sf(data = chull_sf) + geom_sf(data = treat_points)
##   #
##   # data %>%
##   #   filter(post_treatment_period == FALSE) %>%
##   #   mutate(treatment_state = factor(state == "AZ", levels = c(FALSE, TRUE), labels = c("Synthetic Control", "Arizona"))) %>%
##   #   select(year, treatment_state, prop_non_hispanic_below_hs) %>%
##   #   with(., chull(x = year, y = prop_non_hispanic_below_hs)) %>%
##   #   tibble(points_in_hull)
##   #
## }
