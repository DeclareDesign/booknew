# ---
# Designs encountering noncompliance
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

design <-
  declare_population(
    N = 100,
    type = 
      rep(c("Always-Taker", "Never-Taker", "Complier", "Defier"),
          c(0.2, 0.2, 0.6, 0.0)*N),
    U = rnorm(N),
    potential_outcomes(
      Y ~ case_when(
        type == "Always-Taker" ~ -0.25 - 0.50 * D + U,
        type == "Never-Taker" ~ 0.75 - 0.25 * D + U,
        type == "Complier" ~ 0.25 + 0.50 * D + U,
        type == "Defier" ~ -0.25 - 0.50 * D + U
      ),
      conditions = list(D = c(0, 1))
    ),
    potential_outcomes(
      D ~ case_when(
        Z == 1 & type %in% c("Always-Taker", "Complier") ~ 1,
        Z == 1 & type %in% c("Never-Taker", "Defier") ~ 0,
        Z == 0 & type %in% c("Never-Taker", "Complier") ~ 0,
        Z == 0 & type %in% c("Always-Taker", "Defier") ~ 1
      ),
      conditions = list(Z = c(0, 1))
    )
  ) +
  declare_estimand(
    ATE = mean(Y_D_1 - Y_D_0),
    CACE = mean(Y_D_1[type == "Complier"] - Y_D_0[type == "Complier"])) +
  declare_assignment(Z = conduct_ra(N = N, prob = 0.5), handler = fabricate) +
  declare_measurement(D = fabricatr::reveal_outcomes(D ~ Z),
                      Y = fabricatr::reveal_outcomes(Y ~ D)) +
  declare_estimator(
    Y ~ D | Z,
    model = iv_robust,
    estimand = c("ATE", "CACE"),
    label = "2SLS"
  ) +
  declare_estimator(
    Y ~ D,
    model = lm_robust,
    estimand = c("ATE", "CACE"),
    label = "As treated"
  ) +
  declare_estimator(
    Y ~ D,
    model = lm_robust,
    estimand = c("ATE", "CACE"),
    subset = D == Z,
    label = "Per protocol"
  )






inquiries_df <- draw_estimands(design) 

label_df <- inquiries_df %>% mutate(estimator_label = "2SLS", 
                                    x = c(-0.1, 0.8),
                                    y = 75)

dx$simulations_df %>% filter(estimand_label == "ATE") %>%
  ggplot(aes(estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap(~estimator_label, ncol = 1) +
  geom_vline(data = inquiries_df, aes(xintercept = estimand, color = estimand_label), linetype = "dashed") +
  geom_text(data = label_df, aes(x = x, y = y, label = estimand_label)) +
  dd_theme() +
  theme(legend.position = "none")
  

dag <- dagify(Y ~ D + type + U,
              D ~ Z + type + U,
              type ~ U)

nodes <-
  tibble(
    name = c("Z", "D", "U", "Y", "type"),
    label = c("Z", "D", "U", "Y", "C"),
    annotation = c(
      "**Random assignment**",
      "**Treatment received**",
      "**Unknown heterogeneity**",
      "**Outcome**",
      "**Principal stratum**<br>Compliance type"
    ),
    x = c(1, 3, 4, 5, 2),
    y = c(1, 1, 4, 1, 4),
    nudge_direction = c("S", "S", "N", "S", "N"),
    data_strategy = c("assignment", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled", 
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df

types <- c("Always-Taker", "Never-Taker", "Complier", "Defier")
direct_effect_of_encouragement <- 0.0
proportion_defiers <- 0.0

design <-
  declare_population(
    N = 500,
    type = sample(
      types,
      N,
      replace = TRUE,
      prob = c(0.1, 0.1, 0.8 - proportion_defiers, proportion_defiers)
    ),
    noise = rnorm(N)
  ) +
  declare_potential_outcomes(
    D ~ case_when(
      Z == 0 & type %in% c("Never-Taker", "Complier") ~ 0,
      Z == 1 & type %in% c("Never-Taker", "Defier") ~ 0,
      Z == 0 & type %in% c("Always-Taker", "Defier") ~ 1,
      Z == 1 & type %in% c("Always-Taker", "Complier") ~ 1
    )
  ) +
  declare_potential_outcomes(
    Y ~ 0.5 * (type == "Complier") * D +
      0.25 * (type == "Always-Taker") * D +
      0.75 * (type == "Defier") * D +
      direct_effect_of_encouragement * Z + noise,
    assignment_variables = c("D", "Z")
  ) +
  declare_estimand(CACE = mean((Y_D_1_Z_1 + Y_D_1_Z_0) / 2 -
                                 (Y_D_0_Z_1 + Y_D_0_Z_0) / 2),
                   subset = type == "Complier") +
  declare_assignment(prob = 0.5) +
  declare_reveal(D, assignment_variable = Z) +
  declare_reveal(Y, assignment_variables = c(D, Z)) +
  declare_estimator(Y ~ D | Z, model = iv_robust, estimand = "CACE")






gg_df <-
  simulations_df %>%
  group_by(proportion_defiers,
           direct_effect_of_encouragement) %>%
  summarize(bias = mean(estimate - estimand))

ggplot(gg_df,
       aes(
         proportion_defiers,
         bias,
         group = direct_effect_of_encouragement,
         color = direct_effect_of_encouragement
       )) +
  geom_point() +
  geom_line() + 
  dd_theme() + 
  theme(legend.position = "bottom")
