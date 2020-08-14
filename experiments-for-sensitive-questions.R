# ---
# Experiments for sensitive questions
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

 
design <-
  declare_population(
    N = 100,
    U = rnorm(N),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = 0.2)),
    X = rbinom(N, size = 3, prob = 0.5)
  ) +
  declare_estimand(proportion = mean(Y_star)) +
  declare_measurement(Y_direct = Y_star - S) +
  declare_potential_outcomes(Y_list ~ Y_star * Z + X) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y_direct ~ 1,
                    model = lm_robust,
                    estimand = "proportion",
                    label = "direct") +
  declare_estimator(Y_list ~ Z, estimand = "proportion", label = "list")


dag <- dagify(Y_direct ~ Y_star + S,
              Y_list ~ Y_star + X + Z,
              S ~ U,
              X ~ U,
              Y_star ~ U)

nodes <-
  tibble(
    name = c("U", "S", "Y_star", "X", "Y_direct", "Y_list", "Z"),
    label = c(
      "U",
      "S",
      "Y<sup>*</sup>",
      "X",
      "Y<sup>D</sup>",
      "Y<sup>L</sup>",
      "Z"
    ),
    annotation = c(
      "Unknown<br>heterogeneity",
      "**Sensitivity bias**",
      "**Latent**<br> Sensitive trait",
      "Control item count",
      "**Outcome 1**<br> Direct question",
      "**Outcome 2**<br> List question",
      "**Random assignment**<br>List experiment condition"
    ),
    x = c(1, 7/3, 7/3, 7/3, 11/3, 11/3, 5),
    y = c(2.5, 4, 2.5, 1, 4, 2.5, 2.5),
    nudge_direction = c("N", "N", "S", "S", "N", "N", "S"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df



# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

# Model -------------------------------------------------------------------
proportion_shy <- .06

list_design <-
  
  # Model
  declare_population(
    N = 5000,
    # true trump vote (unobservable)
    truthful_trump_vote = draw_binary(.45, N),
    
    # shy voter (unobservable)
    shy = draw_binary(proportion_shy, N),
    
    # direct question response (1 if Trump supporter and not shy, 0 otherwise)
    Y_direct = if_else(truthful_trump_vote == 1 & shy == 0, 1, 0),
    
    # nonsensitive list experiment items
    raise_minimum_wage = draw_binary(.8, N),
    repeal_obamacare = draw_binary(.6, N),
    ban_assault_weapons = draw_binary(.5, N)
  ) +
  
  declare_potential_outcomes(
    Y_list_Z_0 = raise_minimum_wage + repeal_obamacare + ban_assault_weapons,
    Y_list_Z_1 = Y_list_Z_0 + truthful_trump_vote
  ) +
  
  # Inquiry
  declare_estimand(proportion_truthful_trump_vote = mean(truthful_trump_vote),
                   ATE = mean(Y_list_Z_1 - Y_list_Z_0)) +
  
  # Data Strategy
  declare_sampling(n = 500) +
  declare_assignment(prob = .5) +
  declare_reveal(Y_list) +
  
  # Answer Strategy
  declare_estimator(
    Y_direct ~ 1, model = lm_robust, term = "(Intercept)", estimand = "proportion_truthful_trump_vote", label = "direct") +
  declare_estimator(
    Y_list ~ Z, model = difference_in_means, estimand = c("proportion_truthful_trump_vote", "ATE"), label = "list")






summary_df <- 
  simulations_list %>%
  filter(estimand_label == "proportion_truthful_trump_vote") %>% 
  gather(key, value, estimand, estimate) %>%
  group_by(estimator_label, key) %>%
  summarize(average_value = mean(value))

simulations_list %>%
  ggplot(aes(estimate)) +
  geom_histogram(bins = 30) +
  geom_vline(data = summary_df, aes(xintercept = average_value, color = key, linetype = key)) +
  facet_wrap(~estimator_label)

list_design_ceiling <- replace_step(
  list_design, step = 2, 
  new_step = declare_potential_outcomes(
    Y_list_Z_0 = raise_minimum_wage + repeal_obamacare + ban_assault_weapons,
    Y_list_Z_1_no_liars = Y_list_Z_0 + truthful_trump_vote,
    Y_list_Z_1 = ifelse(Y_list_Z_1_no_liars == 4, 3, Y_list_Z_1_no_liars))
)





diagnosis_list_ceiling %>% get_diagnosands %>% select(estimator_label, estimand_label, bias, rmse) %>% kable(digits = 3)

library(rr)

rr_forced_known <- function(data) {
  fit  <- try(rrreg(Y_forced_known ~ 1, data = data, p = 2/3, p0 = 1/6, p1 = 1/6, design = "forced-known"))
  pred <- try(as.data.frame(predict(fit, avg = TRUE, quasi.bayes = TRUE)))
  if(class(fit) != "try-error" & class(pred) != "try-error") {
    names(pred) <- c("estimate", "std.error", "conf.low", "conf.high")
    pred$p.value <- with(pred, 2 * pnorm(-abs(estimate / std.error)))
  } else {
    pred <- data.frame(estimate = NA, std.error = NA, conf.low = NA, conf.high = NA, p.value = NA, error = TRUE)
  }
  pred
}

rr_mirrored <- function(data) {
  fit  <- try(rrreg(Y_mirrored ~ 1, data = data, p = 2/3, design = "mirrored"))
  pred <- try(as.data.frame(predict(fit, avg = TRUE, quasi.bayes = TRUE)))
  if(class(fit) != "try-error" & class(pred) != "try-error") {
    names(pred) <- c("estimate", "std.error", "conf.low", "conf.high")
    pred$p.value <- with(pred, 2 * pnorm(-abs(estimate / std.error)))
  } else {
    pred <- data.frame(estimate = NA, std.error = NA, conf.low = NA, conf.high = NA, p.value = NA, error = TRUE)
  }
  pred
}

proportion_shy <- .06

rr_design <-
  declare_population(
    N = 100, 
    
    # true trump vote (unobservable)
    truthful_trump_vote = draw_binary(.45, N),
    
    # shy voter (unobservable)
    shy = draw_binary(proportion_shy, N),
    
    # Direct question response (1 if Trump supporter and not shy, 0 otherwise)
    Y_direct = as.numeric(truthful_trump_vote == 1 & shy == 0)) +
  
  declare_estimand(sensitive_item_proportion = mean(truthful_trump_vote)) +
  
  declare_potential_outcomes(Y_forced_known ~ (dice == 1) * 0 + (dice %in% 2:5) * truthful_trump_vote + (dice == 6) * 1, conditions = 1:6, assignment_variable = "dice") +
  declare_potential_outcomes(Y_mirrored ~ (coin == "heads") * truthful_trump_vote + (coin == "tails") * (1 - truthful_trump_vote), conditions = c("heads", "tails"), assignment_variable = "coin") +
  
  declare_assignment(prob_each = rep(1/6, 6), conditions = 1:6, assignment_variable = "dice") +
  declare_assignment(prob_each = c(2/3, 1/3), conditions = c("heads", "tails"), assignment_variable = "coin") +
  
  declare_reveal(Y_forced_known, dice) +
  declare_reveal(Y_mirrored, coin) +
  
  declare_estimator(handler = tidy_estimator(rr_forced_known), label = "forced_known", estimand = "sensitive_item_proportion") +
  declare_estimator(handler = tidy_estimator(rr_mirrored), label = "mirrored", estimand = "sensitive_item_proportion") +
  declare_estimator(Y_direct ~ 1, model = lm_robust, term = "(Intercept)", label = "direct", estimand = "sensitive_item_proportion")

rr_design <- set_diagnosands(rr_design, diagnosands = declare_diagnosands(select = c(mean_estimate, bias, rmse, power)))





kable(reshape_diagnosis(rr_diagnosis))
