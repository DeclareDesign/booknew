# ---
# Improving research designs
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

model <-
  declare_model(
    villages = add_level(N = 500, U_village = rnorm(N, sd = 0.1)),
    citizens = add_level(
      N = 100, 
      U_citizen = rnorm(N),
      potential_outcomes(
        Y ~ pnorm(
          U_citizen + U_village + 
            0.1 * (Z == "personal") + 0.15 * (Z == "social")),
          conditions = list(Z = c("neutral", "personal", "social"))
        )))

inquiry <- declare_inquiry(
  ATE_personal = mean(Y_Z_personal - Y_Z_neutral),
  ATE_social = mean(Y_Z_social - Y_Z_neutral)
)

n_villages <- 192
citizens_per_village <- 48

data_strategy <-
  declare_sampling(
    S_village = cluster_rs(clusters = villages, n = n_villages),
    filter = S_village == 1,
    legacy = FALSE) +
  
  declare_sampling(
    S_citizen = strata_rs(strata = villages, n = citizens_per_village),
    filter = S_citizen == 1,
    legacy = FALSE) +
  
  declare_assignment(
  	Z = cluster_ra(
  	  clusters = villages, 
  	  conditions = c("neutral", "personal", "social")),
  	legacy = FALSE) + 
  
  declare_measurement(
    Y_latent = reveal_outcomes(Y ~ Z),
    Y_observed = rbinom(N, 1, prob = Y_latent)
  )

answer_strategy <- 
  declare_estimator(Y_observed ~ Z, term = c("Zpersonal", "Zsocial"), 
                    clusters = villages, 
                    model = lm_robust,
                    inquiry = c("ATE_personal", "ATE_social"))

design <- model + inquiry + data_strategy + answer_strategy

dag <- dagify(
  Y ~ Z + X + U,
  Z ~ X
)

nodes <-
  tibble(
    name = c("Y", "Z", "U", "X"),
    label = c("Y", "Z", "U", "X"),
    annotation = c(
      "**Outcome**<br>",
      "**Random assignment**<br>",
      "**Unknown heterogeneity**",
      "**villages**<br>Used for cluster assignment"),
    x = c(5, 1, 5, 1),
    y = c(2.5, 2.5, 4, 4), 
    nudge_direction = c("S", "S", "N", "N"),
    data_strategy = c("unmanipulated", "assignment", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df + coord_fixed(ylim = c(2.05, 4.6), xlim = c(0.25 - epsilon, 5.75 + epsilon))

diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05),
  cost = mean(10 * n_villages + 1 * n_villages * citizens_per_village)
)





get_diagnosands(diagnosis) %>% 
  select(inquiry_label, estimator_label, bias, rmse, power) %>%
  kable(digits = 3, caption = "Diagnosis of the simplified Gulzar-Khan design.", booktabs = TRUE)

designs <- redesign(design, n_villages = c(192, 295, 397, 500), citizens_per_village = c(25, 50, 75, 100))

dx <- diagnose_design(designs, diagnosands = diagnosands, sims = 500)
