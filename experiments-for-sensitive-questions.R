# ---
# Experiments for sensitive questions
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

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
