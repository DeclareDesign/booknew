# ---
# Conjoint experiments
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

# applies the function to each pair
Y_function <- function(data) {
  data %>%
    group_by(pair) %>%
    mutate(Y = if_else(E == max(E), 1, 0)) %>%
    ungroup
}
design <- 
  declare_population(
    subject = add_level(N = 500),
    pair = add_level(N = 4),
    candidate = add_level(N = 2, U = runif(N))
  ) +
  declare_assignment(assignment_variable = "A1") +
  declare_assignment(assignment_variable = "A2", 
                     conditions = c("young", "middle", "old")) +
  declare_assignment(assignment_variable = "A3")  +
  declare_step(
    E = 
      0.05 * A1 + 
      0.04 * (A2 == "middle") + 
      0.08 * (A2 == "old") + 
      0.02 * A3 + U,
    handler = fabricate) +
  declare_measurement(handler = Y_function) +
  declare_estimator(Y ~ A1 + A2 + A3,
                    model = lm_robust, term = TRUE)

dag <- dagify(Y ~ E,
              E ~ U + A1 + A2 + A3)

nodes <-
  tibble(
    name = c("A1", "A2", "A3", "E", "Y", "U"),
    label = c("A1", "A2", "A3", "E", "Y", "U"),
    annotation = c("**Random assignment**<br>Coethnicity attribute",
                   "**Random assignment**<br>Age attribute",
                   "**Random assignment**<br>Gender attribute",
                   "**Latent outcome**<br>Candidate evaluation",
                   "**Measured outcome**<br>Candidate choice",
                   "**Unknown heterogeneity**"),
    x = c(1, 1, 1, 3.5, 5, 3.5),
    y = c(4, 2.5, 1, 2.5, 2.5, 4),
    nudge_direction = c("N", "N", "N", "S", "N", "N"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df
