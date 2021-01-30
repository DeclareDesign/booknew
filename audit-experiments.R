# ---
# Audit experiments
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

design <- 
  declare_model(N = 100,
                U = rnorm(N),
                potential_outcomes(
                  R ~ ifelse(Z + U > 0.5, 1, 0), 
                  conditions = list(Z = c(0, 1))),
                potential_outcomes(
                  Q ~ ifelse(R == 1, Z + U, NA_real_),
                  conditions = list(Z = c(0, 1), R = c(0, 1)))) +
  declare_inquiry(ATE_R = mean(R_Z_1 - R_Z_0)) + 
  declare_inquiry(CATE_ar = mean(Q_Z_1_R_1 - Q_Z_0_R_1), 
                   subset = (R_Z_1 == 1 & R_Z_0 == 0)) + 
  declare_assignment(Z = complete_ra(N, prob = 0.5), legacy = FALSE) +
  declare_measurement(R = reveal_outcomes(R ~ Z),
                      Q = reveal_outcomes(Q ~ Z + R)) + 
  # declare_reveal(Q, c(Z, R)) +
  declare_estimator(R ~ Z, inquiry = "ATE_R", label = "ATE_R") +
  declare_estimator(Q ~ Z, subset = (R == 1), 
                    inquiry = "CATE_ar", 
                    label = "CATE_ar")

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
    data_strategy = c("assignment", "unmanipulated", "unmanipulated", "unmanipulated"),
    answer_strategy = c("uncontrolled", "controlled", "uncontrolled", "uncontrolled")
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df + coord_fixed(xlim = c(0.5, 5.5))
