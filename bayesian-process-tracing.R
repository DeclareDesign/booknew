# ---
# Bayesian Process tracing
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

types <- c('Z_caused_Y', 'Z_caused_not_Y', 'always_Y', 'always_not_Y')

design <-
  declare_population(
    N = 195,
    Z = draw_binary(prob = .3, N = N),
    type = sample(x = types, size = N, replace = TRUE, 
                  prob = c(.2, .1, .2, .5)))  +
  declare_potential_outcomes(
    Y ~ Z * (type == "Z_caused_Y") + 
      (1 - Z) * (type == "Z_caused_not_Y") + 
      (type == "always_Y"),
    conditions = list(Z = c(0, 1), type = types)) +
  declare_potential_outcomes(
    pr_C_1 ~ Z * (.25 * (type == "Z_caused_Y") + 
                    .005 * (type == "always_Y")),
    conditions = list(Z = c(0, 1), type = types)) +
  reveal_outcomes(c(Y, pr_C_1), c(Z, type)) +
  declare_measurement(C = draw_binary(prob = pr_C_1)) +
  declare_sampling(handler = function(data){
    data %>%
      filter(Z == 1 & Y == 1) %>%
      sample_n(size = 1)
    }) +
  declare_estimand(did_Z_cause_Y = type == 'Z_caused_Y') +
  declare_estimator(
    pr_type_Z_caused_Y = 0.500,
    pr_C_1_type_Z_caused_Y = 0.250,
    pr_C_1_type_always_Y = 0.005,
    pr_C_type_Z_caused_Y = 
      C * pr_C_1_type_Z_caused_Y + 
      (1 - C) * (1 - pr_C_1_type_Z_caused_Y),
    pr_C_type_always_Y = 
      C * pr_C_1_type_always_Y + 
      (1 - C) * (1 - pr_C_1_type_always_Y),
    posterior =
      pr_type_Z_caused_Y * pr_C_type_Z_caused_Y / 
      (pr_type_Z_caused_Y * pr_C_type_Z_caused_Y + 
         pr_C_type_always_Y * (1 - pr_type_Z_caused_Y)),
    estimator_label = "Smoking Gun",
    estimand_label = "did_Z_cause_Y",
    handler = summarize) 


dag <- dagify(C ~ Z + type,
              Y ~ Z + type)

nodes <-
  tibble(
    name = c("Z", "type","C", "Y"),
    label = name,
    annotation = c(
      "**Cause**<br>Suspected cause of Y",
      "**Type**<br>Causal relationship<br>between Z and Y",
      "**Clue**<br>Evidence that Z caused Y",
      "**Outcome**"),
    x = c(1, 1, 5, 5),
    y = c(1.5,3.5,1.5,3.5),
    nudge_direction = c("S", "N", "S", "N"),
    answer_strategy = "uncontrolled"
  )


ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df

## diagnose_design(design,
##                 diagnosands = declare_diagnosands(
##                   bias = mean(posterior - estimand),
##                   rmse = sqrt(mean((posterior - estimand) ^ 2)),
##                   mean_estimand = mean(estimand),
##                   mean_posterior = mean(posterior)),
##                 sims = 1000)
