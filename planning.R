# ---
# Planning
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

# Bonila and Tillery PAP
rescale <- function(x) (x - min(x)) / (max(x) - min(x))

design <- 
  declare_population(
    N = 800,
    female = rbinom(N, 1, prob = 0.51),
    lgbtq = rbinom(N, 1, prob = 0.05),
    age = sample(18:80, N, replace = TRUE),
    linked_fate = sample(1:5, N, replace = TRUE, prob = c(0.05, 0.05, 0.15, 0.25, 0.5)),
    U = runif(N)
  ) + 
  declare_potential_outcomes(
    blm_support ~ 
      rescale(U + 0.45 * linked_fate + 0.001 * age + 0.25 * lgbtq) + 
      0.1 * (Z == "nationalism") * (1 + linked_fate * 0.1) + 
      0.1 * (Z == "feminism") * (1 + female * 0.1) + 
      0.1 * (Z == "intersectional") * (1 + lgbtq * 0.1),
    conditions = c("general", "nationalism", "feminism", "intersectional")
  ) + 
  declare_assignment(conditions = c("general", "nationalism", "feminism", "intersectional"), simple = TRUE) + 
  declare_reveal(blm_support, Z) + 
  declare_measurement(
    blm_support_likert = as.numeric(cut(blm_support, breaks = c(-100, 0.1, 0.3, 0.6, 0.8, 100), labels = 1:5))
  ) + 
  declare_estimator(blm_support_likert ~ Z, term = c(Znationalism, Zfeminism, Zintersectional), model = lm_robust) + 
  declare_estimator(blm_support_likert ~ Z * linked_fate, term = "Zfeminism:linked_fate", model = lm_robust, label = "het-fx-nationalism-linked-fate") + 
  declare_estimator(blm_support_likert ~ Z * female, term = "Zfeminism:female", model = lm_robust, label = "het-fx-feminism-female") + 
  declare_estimator(blm_support_likert ~ Z * lgbtq, term = "Zfeminism:lgbtq", model = lm_robust, label = "het-fx-intersectional-lgbtq")
