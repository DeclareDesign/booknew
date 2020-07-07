# ---
# Choosing an answer strategy
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

tau <- .10
N <- 8
N_sampled <- 4
population <- declare_population(N = N, e = runif(N)) 
potential_outcomes <- declare_potential_outcomes(
  Y_Z_0 = .5 < e, Y_Z_1 = .5 < e + tau)
estimand <- declare_estimand(PATE = mean(Y_Z_1 - Y_Z_0))
sampling <- declare_sampling(n = N_sampled)
assignment <- declare_assignment(prob = .5)
reveal_outcomes <- declare_reveal(Y, Z)
estimator <- declare_estimator(Y ~ Z, label = "DiM", estimand = "PATE")
simple_design <- population + potential_outcomes + estimand + 
  sampling + assignment + reveal_outcomes + estimator
simple_design_data <- draw_data(simple_design)

estimates_df <- difference_in_means(Y ~ Z, data = simple_design_data)
kable(tidy(estimates_df))

# haven't gotten something good here. 

test_sample_estimator <-
  function(data){
    r_sq_full <- lm_robust(Y ~ Z + X, data = data, subset = train == 1)$r.squared
    r_sq_short <- lm_robust(Y ~ Z, data = data, subset = train == 1)$r.squared
    if(r_sq_full > r_sq_short) {
      lm_robust(Y ~ Z + X, subset = train == 0, data = data) %>% 
        tidy %>% filter(term == "Z") %>% mutate(which = "full")
    } else {
      lm_robust(Y ~ Z, subset = train == 0, data = data) %>% 
        tidy %>% filter(term == "Z") %>% mutate(which = "short")
    }
  }

design <-
  declare_population(
    N = 100, X = rbinom(N, 1, 0.5), u = rnorm(N), train = rep(0:1, each = 50)
  ) + 
  declare_potential_outcomes(Y ~ Z + u) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_assignment(prob = 0.5) + 
  declare_reveal(Y, Z) + 
  declare_estimator(Y ~ Z, model = lm_robust, label = "one-step", estimand = "ATE") + 
  declare_estimator(
    handler = tidy_estimator(test_sample_estimator),
    estimand = "ATE") 

dg <- diagnose_design(design, sims = 5000)



ATE <- 0.0

design <- 
  declare_population(N = 1000,
                     binary_covariate = rbinom(N, 1, 0.5),
                     normal_error = rnorm(N)) +
  # crucial step in POs: effects are not heterogeneous
  declare_potential_outcomes(Y ~ ATE * Z + normal_error) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y ~ Z, subset = (binary_covariate == 0), label = "CATE_0") + 
  declare_estimator(Y ~ Z, subset = (binary_covariate == 1), label = "CATE_1") +
  declare_estimator(Y ~ Z * binary_covariate, 
                    model = lm_robust, term = "Z:binary_covariate", label = "interaction")

estimates <- draw_estimates(design)

g1 <- ggplot(data = estimates %>% filter(term == "Z"), aes(estimator_label, estimate)) + 
  geom_point() + 
  geom_errorbar(aes(x = estimator_label, ymin = conf.low, ymax = conf.high), width = 0.2) + 
  ylab("Estimate (95% confidence interval)") +
  geom_hline(yintercept = 0, lty = "dashed") +
  ggtitle("Visualization A") +
  dd_theme() + 
  theme(axis.title.x = element_blank())

g2 <- ggplot(data = estimates, aes(estimator_label, estimate)) + 
  geom_point() + 
  geom_errorbar(aes(x = estimator_label, ymin = conf.low, ymax = conf.high), width = 0.2) + 
  ylab("Estimate (95% confidence interval)") +
  geom_hline(yintercept = 0, lty = "dashed") +
  ggtitle("Visualization B") +
  dd_theme() + 
  theme(axis.title.x = element_blank())

grid.arrange(g1, g2, nrow = 1)

# sweep across all ATEs from 0 to 0.5
designs <- redesign(design, ATE = seq(0, 0.5, 0.05))





# Summarize simulations ---------------------------------------------------

reshaped_simulations <-
  simulations_one_significant_not_other %>%
  transmute(ATE,
            sim_ID,
            estimator_label,
            estimate,
            conf.high,
            conf.low,
            significant = p.value < 0.05) %>%
  pivot_wider(id_cols = c("ATE", "sim_ID"), names_from = "estimator_label", values_from = c("estimate", "conf.high", "conf.low", "significant"))


# Plot 1 ------------------------------------------------------------------

gg_df <- 
  reshaped_simulations %>%
  group_by(ATE) %>%
  summarize(`Significant for one group but not the other` = mean(xor(significant_CATE_0, significant_CATE_1)),
            `Difference in subgroup effects is significant` = mean(significant_interaction)) %>%
  gather(condition, power, -ATE)

ggplot(gg_df, aes(ATE, power, color = condition)) +
  geom_point() +
  geom_line() +
  geom_label(data = (. %>% filter(ATE == 0.2)),
             aes(label = condition),
             nudge_y = 0.02,
             family = "Palatino") +
  dd_theme() +
  scale_color_manual(values = c("red", "blue")) +
  theme(legend.position = "none") +
  labs(
    x = "True constant effect size",
    y = "Probability of result (akin to statistical power)"
  )
