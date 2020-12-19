# ---
# List experiments
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

model <- 
  declare_population(
    N = 100,
    U = rnorm(N),
    X = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = 0.2))
  ) + 
  declare_potential_outcomes(Y_list ~ Y_star * Z + X) 

inquiry <- declare_estimand(proportion = mean(Y_star))

data_strategy <- 
  declare_measurement(Y_direct = Y_star - S) +
  declare_assignment(prob = 0.5) + 
  declare_reveal(Y_list, Z)

answer_strategy <-
  declare_estimator(Y_direct ~ 1,
                    model = lm_robust,
                    estimand = "proportion",
                    label = "direct") +
  declare_estimator(Y_list ~ Z, estimand = "proportion", label = "list")

design <- model + inquiry + data_strategy + answer_strategy

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

model_design_effects <- 
  declare_population(
    N = 100,
    U = rnorm(N),
    X_control = rbinom(N, size = 3, prob = 0.5),
    X_treat = rbinom(N, size = 3, prob = 0.25),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = 0.2))
  ) + 
  declare_potential_outcomes(
    Y_list ~ (Y_star + X_treat) * Z + X_control * (1 - Z)
  )

design_design_effects <- 
  model_design_effects + inquiry + data_strategy + answer_strategy





diagnose_design_effects %>%
  get_diagnosands %>%
  select(estimator_label, estimand_label, bias, rmse) %>%
  kable(digits = 3, booktabs = TRUE)

model_liars <- 
  declare_population(
    N = 100,
    U = rnorm(N),
    X = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = 0.2))
  ) + 
  declare_potential_outcomes(
    Y_list ~ if_else(X == 3 & Y_star == 1 & Z == 1, 3, Y_star * Z + X)
  )

design_liars <- model_liars + inquiry + data_strategy + answer_strategy





diagnose_liars %>%
  get_diagnosands %>%
  select(estimator_label, estimand_label, bias, rmse) %>%
  kable(digits = 3, booktabs = TRUE)

model_sample_size <- 
  declare_population(
    N = N,
    U = rnorm(N),
    X = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = proportion_shy))
  ) +
  declare_potential_outcomes(Y_list ~ Y_star * Z + X) 

design <- model_sample_size + inquiry + data_strategy + answer_strategy

designs <- redesign(design, proportion_shy = seq(from = 0, to = 0.5, by = 0.05), N = seq(from = 500, to = 5000, by = 500))





# make a plot
diagnosis_tradeoff %>%
  get_diagnosands %>%
  filter(proportion_shy < 0.35) %>% 
  mutate(estimator_lbl = factor(estimator_label, levels = c("direct", "list"), labels = c("Direct question", "List experiment"))) %>% 
  ggplot(aes(N, rmse, group = estimator_lbl, color = estimator_lbl)) +
  # bias line
  geom_line() + 
  scale_color_discrete("Question type") + 
  labs(y = "RMSE") + 
  facet_wrap(~ proportion_shy, ncol = 4) + 
  dd_theme() + 
  theme(legend.position = "bottom")

model_control_item_count <-
  declare_population(
    N = 100,
    U = rnorm(N),
    X = rbinom(N, size = J, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = 0.2))
  ) + 
  declare_potential_outcomes(Y_list ~ Y_star * Z + X) 

design <- model_control_item_count + inquiry + data_strategy + answer_strategy

designs <- redesign(design, J = 2:5)





diagnosis_control_item_count %>%
  get_diagnosands %>%
  filter(estimator_label == "list") %>% 
  select(J, bias, rmse) %>%
  kable(digits = 3, booktabs = TRUE)

model_control_item_correlation <-
  declare_population(
    N = 100,
    U = rnorm(N),
    X_1 = draw_binary(0.5, N), 
    X_2 = correlate(given = X_1, rho = rho, draw_binary, prob = 0.5),
    X_3 = draw_binary(0.5, N),
    X = X_1 + X_2 + X_3,
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = 0.2))
  ) + 
  declare_potential_outcomes(Y_list ~ Y_star * Z + X) 

design <- model_control_item_correlation + inquiry + data_strategy + answer_strategy

designs <- redesign(design, rho = seq(from = 0, to = 1, by = 0.25))





diagnose_control_item_correlation %>%
  get_diagnosands %>%
  filter(estimator_label == "list") %>% 
  select(rho, estimator_label, estimand_label, bias, rmse) %>%
  kable(digits = 3, booktabs = TRUE)

data_strategy_prop_treated <- 
  declare_measurement(Y_direct = Y_star - S) +
  declare_assignment(prob = prop_treated) + 
  declare_reveal(Y_list, Z)

design <- model + inquiry + data_strategy_prop_treated + answer_strategy

designs <- redesign(design, prop_treated = seq(from = 0.2, to = 0.8, by = 0.1))





diagnose_prop_treated %>%
  get_diagnosands %>%
  filter(estimator_label == "list") %>% 
  select(prop_treated, estimator_label, estimand_label, rmse) %>%
  kable(digits = 3, booktabs = TRUE)

rr_forced_known <- function(formula, data) {
  fit  <- try(rrreg(formula, data = data, p = 2/3, p0 = 1/6, p1 = 1/6, design = "forced-known"))
  pred <- try(as.data.frame(predict(fit, avg = TRUE, quasi.bayes = TRUE)))
  if(class(fit) != "try-error" & class(pred) != "try-error") {
    names(pred) <- c("estimate", "std.error", "conf.low", "conf.high")
    pred$p.value <- with(pred, 2 * pnorm(-abs(estimate / std.error)))
  } else {
    pred <- data.frame(estimate = NA, std.error = NA, conf.low = NA, conf.high = NA, p.value = NA, error = TRUE)
  }
  pred
}

rr_forced_known <- label_estimator(rr_forced_known)

library(rr)

model_rr <- 
  declare_population(
    N = 100,
    U = rnorm(N),
    X = rbinom(N, size = 3, prob = 0.5),
    Y_star = rbinom(N, size = 1, prob = 0.3),
    S = case_when(Y_star == 0 ~ 0L,
                  Y_star == 1 ~ rbinom(N, size = 1, prob = 0.2))
  ) + 
  declare_potential_outcomes(
    Y_rr ~ 
      case_when(
        dice == 1 ~ 0L,
        dice %in% 2:5 ~ Y_star,
        dice == 6 ~ 1L
      ),
    conditions = 1:6, assignment_variable = "dice")

data_strategy_rr <- 
  declare_measurement(Y_direct = Y_star - S) +
  declare_assignment(prob_each = rep(1/6, 6), conditions = 1:6,
                     assignment_variable = "dice") + 
  declare_reveal(Y_rr, dice)

answer_strategy_rr <-
  declare_estimator(Y_direct ~ 1,
                    model = lm_robust,
                    estimand = "proportion",
                    label = "direct") +
  declare_estimator(Y_rr ~ 1, handler = rr_forced_known, 
                    label = "forced_known", estimand = "proportion")


design <- model_rr + inquiry + data_strategy_rr + answer_strategy_rr
