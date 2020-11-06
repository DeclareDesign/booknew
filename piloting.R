# ---
# Piloting
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

sd_estimator <- function(data){
  data.frame(sd_y_0_hat = sd(data$Y[data$Z == 0]), sd_y_1_hat = sd(data$Y[data$Z == 1]))
}

des <- 
  declare_population(N = 50, u = rnorm(N, mean = 0, sd = 1)) + 
  declare_potential_outcomes(Y ~ 0.2 * Z + u) + 
  declare_assignment() + 
  declare_estimator(Y ~ Z) + 
  declare_estimator(handler = label_estimator(sd_estimator), label = "sd_estimator")

sims <- simulate_design(des, sims = 1000)



gg_df <- sims %>% 
  mutate(
    se_hat_full_study = sqrt( sd_y_0_hat^2 / 250  + sd_y_1_hat^2 / 250),
    mde_hat_full_study = 2.8 * se_hat_full_study
  ) %>% 
  select(sim_ID, estimate, mde_hat_full_study) %>% 
  pivot_longer(cols = c(estimate, mde_hat_full_study), names_to = "statistic", values_to = "value") %>% 
  na.omit %>% 
  mutate(statistic = factor(statistic, levels = c("estimate", "mde_hat_full_study"), labels = c("Estimated effect size", "Estimated MDE of full study")))

ggplot(gg_df, aes(value)) + 
  geom_histogram() + 
  facet_wrap(~statistic) + 
  xlab("") + ylab("Number of simulations")
