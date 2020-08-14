# ---
# Partial Population Design
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

design <- 
  declare_population(
    group = add_level(N = 10, X = rnorm(N)),
    unit = add_level(N = 100, U = rnorm(N))
  ) +
  declare_assignment(clusters = group, conditions = c("low", "high"), assignment_variable = S) +
  declare_step(S_prob = case_when(S == "low" ~ 0.25, S == "high" ~ 0.75), mutate) +
  declare_assignment(blocks = group, prob_unit = S_prob) + 
  declare_potential_outcomes(
    Y ~ Z + (S == "high") + Z*(S == "high") + X + U,
    conditions = list(Z = c(0, 1), S = c("low", "high"))) +
  declare_estimand(ATE_saturation = mean(Y_Z_0_S_high - Y_Z_0_S_low),
                   ate_no_spill = mean(Y_Z_1_S_low - Y_Z_0_S_low)) +
  declare_reveal(Y, c(Z, S)) +
  declare_estimator(Y ~ Z + S, model = lm_robust, term = c("Z", "Shigh"), estimand = c("ATE_saturation", "ate_no_spill"), label = "main effect")


dag <- dagify(Y ~ Z + S + U + X,
              Z ~ S)

nodes <-
  tibble(
    name = c("X", "U", "S", "Z", "Y"),
    label = c("X", "U", "S", "Z", "Y"),
    annotation = c(
      "**Unknown heterogeneity**<br>Unit effects",
      "**Unknown heterogeneity**",
      "**Treatment assignment 1**<br>Saturation level",
      "**Treatment assignment 2**<br>Individual assignment",
      "**Outcome variable**"
    ),
    x = c(2, 5, 1, 1, 5),
    y = c(2.5, 4, 4, 1, 2.5), 
    nudge_direction = c("N", "N", "N", "S", "S"),
    answer_strategy = "uncontrolled",
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df



# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(blockTools)

N_individuals <- 60
N_groups <- 15
G_per_saturation <- c(5,5,5)

design <- 
  declare_population(N = N_individuals, X = 1:N, U = rnorm(N), G = ntile(X, N_groups)) + 
  declare_assignment(assignment_variable = "S", 
                     clusters = G, 
                     conditions = c("low","med","high"),
                     m_each = G_per_saturation) +
  declare_assignment(prob = 0,
                     blocks = G,
                     assignment_variable = "Z_S_low") +
  declare_assignment(prob = .5,
                     blocks = G,
                     assignment_variable = "Z_S_med") +
  declare_assignment(prob = .75,
                     blocks = G,
                     assignment_variable = "Z_S_high") +
  declare_step(
    spillover_low = ave(Z_S_low, G, FUN = sum) * .1,
    spillover_med = ave(Z_S_med, G, FUN = sum) * .1,
    spillover_high = ave(Z_S_high, G, FUN = sum) * .1,
    handler = fabricate,
    label = "spillover")  +
  declare_potential_outcomes(
    Y ~ Z * -.20 + U + 
      spillover_low * (S == "low") + 
      spillover_med * (S == "med") + 
      spillover_high * (S == "high"),
    conditions = list(Z = c(0,1), S = c("low","med","high"))) +
  declare_estimand(high = mean(Y_Z_0_S_high - Y_Z_0_S_low),
                   med =  mean(Y_Z_0_S_med - Y_Z_0_S_low), 
                   ate_no_spill = mean(Y_Z_1_S_low - Y_Z_0_S_low)) +
  declare_reveal(Z,S) +
  declare_step(
    w = 1 / (S_cond_prob * (Z_S_low_cond_prob * (S == "low") + 
                   Z_S_med_cond_prob * (S == "med") + 
                   Z_S_high_cond_prob * (S == "high"))),
    handler = fabricate) +
  declare_reveal(Y,c(Z, S)) +
  declare_estimator(model = lm_robust, 
                    formula = Y ~ S,
                    subset = Z == 0 & S %in% c("high","low"), 
                    estimand = "high",
                    weights = w,
                    label = "high vs low") +
  declare_estimator(model = lm_robust, 
                    formula = Y ~ S,
                    subset = Z == 0 & S %in% c("med","low"), 
                    weights = w,
                    estimand = "med",
                    label = "med vs low") +
  declare_estimator(model = lm_robust, 
                    formula = Y ~ Z + S,
                    term = "Z", 
                    weights = w,
                    estimand = "ate_no_spill",
                    label = "main effect")

draw_data(design) %>% 
  ggplot(aes(x = 1, y = X, color = as.factor(G))) +
  geom_point() +
  scale_color_discrete("Ballot station") +
  scale_y_continuous("Latitude") +
  scale_x_continuous("Longitude") +
  geom_hline(yintercept = seq(1,N_individuals,by = N_individuals / N_groups) - .5)





diagnosis %>% reshape_diagnosis() %>% kable()

distal_design <- replace_step(design = design, step = "spillover",
                              new_step = declare_step(next_neighbor = c(N,1:(N-1)),
                                                      spillover_low = Z_S_low[next_neighbor],
                                                      spillover_med = Z_S_med[next_neighbor],
                                                      spillover_high = Z_S_high[next_neighbor],
                                                      handler = fabricate) )





distal_diagnosis %>% reshape_diagnosis() %>% kable()
