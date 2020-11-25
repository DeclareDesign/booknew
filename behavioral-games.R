# ---
# Behavioral Games
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

design <-
  declare_population(
    games = add_level(N = 100),
    players = add_level(
      N = 2,
      prosociality = runif(N),
      fairness = prosociality,
      cutoff = pmax(prosociality - 0.25, 0)
    )
  ) +
  declare_estimand(mean_fairness = mean(fairness),
                   mean_cutoff = mean(cutoff)) +
  declare_assignment(
    blocks = games, 
    conditions = c("proposer", "responder"),
    assignment_variable = "role"
  ) + 
  declare_step(
    id_cols = games, 
    names_from = role, 
    values_from = c(prosociality, fairness, cutoff), 
    handler = pivot_wider
  ) + 
  declare_measurement(
    proposal = fairness_proposer * 0.5, 
    response = if_else(proposal >= cutoff_responder, 1, 0)
  ) + 
  declare_estimator(proposal ~ 1,
                    model = lm_robust,
                    estimand = "mean_fairness",
                    label = "mean_fairness") +
  declare_estimator(response ~ 1,
                    model = lm_robust,
                    estimand = "mean_cutoff",
                    label = "mean_cutoff")

dag <- dagify(fairness ~ prosociality,
              cutoff ~ prosociality,
              proposal ~ fairness + role,
              response ~ proposal + cutoff + role)


nodes <-
  tibble(
    name = c("prosociality", "cutoff", "fairness", "role", "proposal", "response"),
    label = c("Y<sup>1*</sup>", "Y<sup>3*</sup>", "Y<sup>2*</sup>", "Z", "Y<sup>2</sup>", "Y<sup>1</sup>"),
    annotation = c(
      "**Latent trait**<br>Prosociality",
      "**Latent trait**<br>Cutoff",
      "**Latent trait**<br>Fairness",
      "**Random assignment**<br>Role",
      "**Outcome 1**<br>Proposal",
      "**Outcome 2**<br>Response"
    ),
    
    x = c(1, 2, 2, 3, 5, 5),
    y = c(2.5, 3.5, 1.5, 2.5, 1.5, 3.5),
    nudge_direction = c("S", "N", "S", "N", "S", "N"),
    answer_strategy = "uncontrolled"
  )


ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df
