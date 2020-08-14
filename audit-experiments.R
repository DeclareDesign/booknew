# ---
# Audit experiments
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

design <- 
  declare_population(N = 100,
                     U = rnorm(N)) +
  declare_potential_outcomes(R ~ if_else(Z + U > 0.5, 1, 0), conditions = list(Z = c(0, 1))) +
  declare_potential_outcomes(Q ~ if_else(R == 1, Z + U, NA_real_), conditions = list(Z = c(0, 1), R = c(0, 1))) +
  declare_estimand(ATE_R = mean(R_Z_1 - R_Z_0)) + 
  declare_estimand(CATE_ar = mean(Q_Z_1_R_1 - Q_Z_0_R_1), subset = (R_Z_1 == 1 & R_Z_0 == 0)) + 
  declare_assignment(prob = 0.5) +
  declare_reveal(R, Z) +
  declare_reveal(Q, c(Z, R)) +
  declare_estimator(R ~ Z, estimand = "ATE_R", label = "ATE_R") +
  declare_estimator(Q ~ Z, subset = (R == 1), estimand = "CATE_ar", label = "CATE_ar")

dag <- dagify(
  Q ~ R + Z + U,
  R ~ Z + U
)

nodes <-
  tibble(
    name = c("Z", "R", "Q", "U"),
    label = c("Z", "R", "Q", "U"),
    annotation = c(
      "**Random assignment**<br> Subject sent email",
      "**Outcome 1**<br>Subject replies to email",
      "**Outcome 2**<br>Email quality",
      "Unknown<br>heterogeneity"),
    x = c(1, 3, 3, 5),
    y = c(2.5, 3.5, 1.5, 2.5), 
    nudge_direction = c("N", "N", "S", "N"),
    answer_strategy = c("uncontrolled", "controlled", "uncontrolled", "uncontrolled")
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df



# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

# Model -------------------------------------------------------------------
population <- declare_population(
  N = 500,
  type = sample(c("A", "B", "C", "D"), size = N, 
                replace = TRUE, prob = c(.40, .05, .10, .45)))

potential_outcomes <- declare_potential_outcomes(
  R_Z_0 = type %in% c("A", "C"),
  R_Z_1 = type %in% c("A", "B"),
  Y_Z_0 = ifelse(R_Z_0, rnorm(n = sum(R_Z_0), mean = .1*(type == "A") - 2*(type == "C")), NA),
  Y_Z_1 = ifelse(R_Z_1, rnorm(n = sum(R_Z_1), mean = .2*(type == "A") + 2*(type == "B")), NA)
)

# Inquiry -----------------------------------------------------------------
estimand_1 <- declare_estimand(ATE_R = mean(R_Z_1 - R_Z_0))
estimand_2 <- declare_estimand(ATE_Y = mean(Y_Z_1 - Y_Z_0))
estimand_3 <- declare_estimand(
  ATE_Y_for_As = mean(Y_Z_1[type == "A"] - Y_Z_0[type == "A"]))

# Data Strategy -----------------------------------------------------------
assignment <- declare_assignment(m = 250)

# Answer Strategy ---------------------------------------------------------
estimator_1 <- declare_estimator(R ~ Z, estimand = estimand_1, label = "ATE_R")
estimator_2 <- declare_estimator(Y ~ Z, estimand = estimand_2, label = "ATE_Y")
estimator_3 <- declare_estimator(Y ~ Z, estimand = estimand_3, label = "ATE_YA")

# Design ------------------------------------------------------------------
design <- 
  population + 
  potential_outcomes + 
  assignment + 
  estimand_1 + estimand_2 + estimand_3 + 
  declare_reveal(outcome_variables = c("R", "Y")) + 
  estimator_1 + estimator_2 + estimator_3





knitr::kable(reshape_diagnosis(diagnosis))
