# ---
# Decisionmaking
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(metafor)
library(broom)

# compare status quo to a new proposed policy, given cost of switching only want to switch if effect is larger than 0.05
N <- 100
effect_size <- 0.1
design <-
  declare_model(
    N = N,
    U = rnorm(N),
    potential_outcomes(Y ~ effect_size * Z + U)
  ) + 
  declare_inquiry(
    ATE = mean(Y_Z_1 - Y_Z_0),
    treatment_better_than_control = if_else(ATE > 0.1, TRUE, FALSE)
  ) + 
  declare_assignment(Z = complete_ra(N), legacy = FALSE) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) + 
  declare_estimator(Y ~ Z, model = difference_in_means, inquiry = "ATE", label = "dim") + 
  declare_test(handler = label_estimator(function(data) subset(tidy(lh_robust(Y ~ Z, linear_hypothesis = "Z - 0.1 = 0", data = data)), term == "Z - 0.1 = 0")), label = "decision", inquiry = "treatment_better_than_control")
# declare_estimator(Y ~ Z, model = lh_robust, linear_hypothesis = "Z - 0.05 = 0", se_type = "HC2", label = "decision")





# the colors here are messed up
gg1 <- diagnosis %>% 
  get_diagnosands %>% 
  filter(estimator_label == "dim") %>% 
  ggplot(aes(N, power, group = effect_size, color = as.factor(effect_size))) + 
    geom_hline(yintercept = 0.8, lty = "dashed", color = "darkgray") +
  geom_line(size = 1) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_color_discrete("True effect size", type = rev(c('#b2182b','#ef8a62','#f7f7f7','#d1e5f0','#67a9cf','#2166ac'))) + 
  labs(y = "Statistical power") + 
  theme(legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))

gg2 <- diagnosis %>% 
  get_diagnosands %>% 
  filter(estimator_label == "decision") %>% 
  ggplot(aes(N, proportion_choose_control, group = effect_size, color = as.factor(effect_size))) + 
  geom_hline(yintercept = 0.8, lty = "dashed", color = "darkgray") +
  geom_line(size = 1) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_color_discrete("True effect size", type = rev(c('#b2182b','#ef8a62','#f7f7f7','#d1e5f0','#67a9cf','#2166ac'))) + 
  labs(y = "Probability of Choosing the Status Quo") + 
  theme(legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))

gg3 <- diagnosis %>% 
  get_diagnosands %>% 
  filter(estimator_label == "decision") %>% 
  ggplot(aes(N, proportion_choose_treatment, group = effect_size, color = as.factor(effect_size))) + 
  geom_hline(yintercept = 0.8, lty = "dashed", color = "darkgray") +
  geom_line(size = 1) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_color_discrete("True effect size", type = c('#b2182b','#ef8a62','#f7f7f7','#d1e5f0','#67a9cf','#2166ac')) + 
  labs(y = "Probability of Choosing the Treatment") + 
  theme(legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))

gg2 + gg3 + plot_layout(guides = "collect")  & theme(legend.position = "bottom")
