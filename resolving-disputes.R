# ---
# Resolving Disputes
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

D_M = 1 # effect of minority on stop
U_M = 1 # effect of suspicion on stop
D_Y = 1 # effect of minority on force
U_Y = 1 # effect of suspicion on force
M_Y = 1 # effect of stop on force

design_1 <-
  declare_population(N = 1000,
                     D = rbinom(N, size = 1, prob = 0.5),
                     U = rnorm(N)) +
  declare_potential_outcomes(M ~ rbinom(N, size = 1, prob = pnorm(D_M *
                                                                    D + U_M * U)),
                             assignment_variable = "D") +
  declare_reveal(M, D) +
  declare_potential_outcomes(Y ~ rnorm(N, D_Y * D + M_Y * M + U_Y * U),
                             conditions = list(D = c(0, 1), M = c(0, 1))) +
  declare_reveal(outcome_variables = "Y",
                 assignment_variables = c("D", "M")) +
  declare_estimand(CDE = mean(Y_D_1_M_1 - Y_D_0_M_1)) +
  declare_estimator(Y ~ D, subset = M == 1, estimand = "CDE")

# no effect of D on M
design_2 <- redesign(design_1, D_M = 0)

# no effect of U on M
design_3 <- redesign(design_1, U_M = 0)

# no effect of U on Y
design_4 <- redesign(design_1, U_Y = 0)






simulations <-
  simulations %>%
  mutate(`Assumed DAG` = factor(
    design_label,
    levels = c("design_1", "design_2", "design_3", "design_4"),
    labels = c(
      "All paths possible",
      "no effect of D on M",
      "no effect of U on M",
      "no effect of U on Y"
    )
  ))


summary_df <-
  simulations %>%
  group_by(`Assumed DAG`) %>%
  summarise(
    mean_estimand = mean(estimand),
    mean_estimate = mean(estimate),
    bias = mean(estimate - estimand)
  ) %>%
  pivot_longer(cols = c("mean_estimand", "mean_estimate"))

ggplot(simulations, aes(estimate)) +
  geom_histogram(bins = 50) +
  geom_vline(data = summary_df, aes(xintercept = value, color = name)) +
  facet_wrap(~`Assumed DAG`) +
  xlab("Simulated CDE estimates") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank())
