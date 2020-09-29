# ---
# Process tracing
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

types <- c('Z_caused_Y', 'Z_caused_not_Y', 'always_Y', 'always_not_Y')

design <-
  declare_population(N = 195,
                     Z = draw_binary(prob = .3, N = N),
                     type = sample(x = types, size = N, 
                                   replace = TRUE, prob = c(.2, .1, .2, .5)))  +
  declare_potential_outcomes(
    Y ~ Z * (type == "Z_caused_Y") + (1 - Z) * (type == "Z_caused_not_Y") + (type == "always_Y"),
    conditions = list(Z = c(0, 1), type = types)) +
  declare_potential_outcomes(
    pr_C_1 ~ Z * (.25 * (type == "Z_caused_Y") + .005 * (type == "always_Y")),
    conditions = list(Z = c(0, 1), type = types)) +
  declare_reveal(c(Y, pr_C_1), c(Z, type)) +
  declare_measurement(C = draw_binary(prob = pr_C_1)) +
  declare_sampling(handler = function(data) data %>% filter(Z==1 & Y==1) %>% sample_n(size = 1)) +
  declare_estimand(did_Z_cause_Y = type == 'Z_caused_Y') +
  declare_estimator(
    pr_type_Z_caused_Y = .5,
    pr_C_1_type_Z_caused_Y = .25,
    pr_C_1_type_always_Y = .005,
    pr_C_type_Z_caused_Y = C * pr_C_1_type_Z_caused_Y + (1 - C) * (1 - pr_C_1_type_Z_caused_Y),
    pr_C_type_always_Y = C * pr_C_1_type_always_Y + (1 - C) * (1 - pr_C_1_type_always_Y),
    posterior =
      pr_type_Z_caused_Y * pr_C_type_Z_caused_Y / (pr_type_Z_caused_Y * pr_C_type_Z_caused_Y + pr_C_type_always_Y * (1 - pr_type_Z_caused_Y)),
    estimator_label = "Smoking Gun",
    estimand_label = "did_Z_cause_Y",
    handler = summarize) 

# diagnose_design(process_tracing_design, 
#                 diagnosands = declare_diagnosands(
#                   bias = mean(posterior - estimand),
#                   rmse = sqrt(mean((posterior - estimand) ^ 2)),
#                   mean_estimand = mean(estimand),
#                   mean_posterior = mean(posterior),
#                   keep_defaults = FALSC
#                 ), sims = 1000)


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

# Calculate bivariate probabilities given correlation
joint_prob <- function(p1, p2, rho, which_prob = NULL) {
  r <- rho * (p1 * p2 * (1 - p1) * (1 - p2)) ^ .5
  probs <- c(`00` = (1 - p1) * (1 - p2) + r,
             `01` = p2 * (1 - p1) - r,
             `10` = p1 * (1 - p2) - r,
             `11` = p1 * p2 + r)
  
  if(!is.null(which_prob)) probs <- probs[which_prob]
  
  return(probs)
}



types <- c('Z_caused_Y', 'Z_caused_not_Y', 'always_Y', 'always_not_Y')




design <-
  declare_population(N = 20,
                     Z = draw_binary(prob = .5, N = N),
                     type = sample(x = types, size = N, replace = TRUE, prob = c(.5, 0, .5, 0)))  +
  declare_potential_outcomes(
    Y ~ Z * (type == "Z_caused_Y") + (1 - Z) * (type == "Z_caused_not_Y") + (type == "always_Y"),
    conditions = list(Z = c(0, 1), type = types)) +
  declare_potential_outcomes(
    pr_C1C2_00 ~ (joint_prob(.75,.3,0,"00") * (type == "Z_caused_Y") + joint_prob(.25,.005,0,"00") * (type == "always_Y")),
    conditions = list(Z = c(0, 1), type = types)) +
    declare_potential_outcomes(
    pr_C1C2_01 ~ (joint_prob(.75,.3,0,"01") * (type == "Z_caused_Y") + joint_prob(.25,.005,0,"01") * (type == "always_Y")),
    conditions = list(Z = c(0, 1), type = types)) +
    declare_potential_outcomes(
    pr_C1C2_10 ~ (joint_prob(.75,.3,0,"10") * (type == "Z_caused_Y") + joint_prob(.25,.005,0,"10") * (type == "always_Y")),
    conditions = list(Z = c(0, 1), type = types)) +
    declare_potential_outcomes(
    pr_C1C2_11 ~ (joint_prob(.75,.3,0,"11") * (type == "Z_caused_Y") + joint_prob(.25,.005,0,"11") * (type == "always_Y")),
    conditions = list(Z = c(0, 1), type = types)) +
  declare_reveal(c(Y, pr_C1C2_00,pr_C1C2_01,pr_C1C2_10,pr_C1C2_11), c(Z, type)) +
  declare_assignment(blocks = ID, block_prob_each = cbind(pr_C1C2_00,pr_C1C2_01,pr_C1C2_10,pr_C1C2_11),
                     conditions = c("00","01","10","11"), 
                     assignment_variable = "C1C2") +  
  declare_sampling(handler = function(data) data %>% filter(Z==1 & Y==1) %>% sample_n(size = 1)) +
  declare_estimand(did_Z_cause_Y = type == 'Z_caused_Y') +
  declare_measurement(
  C1 = ifelse(C1C2 == "10" | C1C2 == "11", 1, 0),
  C2 = ifelse(C1C2 == "01" | C1C2 == "11", 1, 0),
  handler = fabricate) +
  declare_estimator(
    pr_type_Z_caused_Y = .5,
    pr_C_1_type_Z_caused_Y = .75,
    pr_C_1_type_always_Y = .25,
    C = C1,
    pr_C_type_Z_caused_Y = C * pr_C_1_type_Z_caused_Y + (1 - C) * (1 - pr_C_1_type_Z_caused_Y),
    pr_C_type_always_Y = C * pr_C_1_type_always_Y + (1 - C) * (1 - pr_C_1_type_always_Y),
    posterior =
      pr_type_Z_caused_Y * pr_C_type_Z_caused_Y / (pr_type_Z_caused_Y * pr_C_type_Z_caused_Y + pr_C_type_always_Y * (1 - pr_type_Z_caused_Y)),
    label = "Straw in the Wind",
    estimand_label = "did_Z_cause_Y",
    handler = summarize) +
  declare_estimator(
    pr_type_Z_caused_Y = .5,
    pr_C_1_type_Z_caused_Y = .30,
    pr_C_1_type_always_Y = .005,
    C = C2,
    pr_C_type_Z_caused_Y = C * pr_C_1_type_Z_caused_Y + (1 - C) * (1 - pr_C_1_type_Z_caused_Y),
    pr_C_type_always_Y = C * pr_C_1_type_always_Y + (1 - C) * (1 - pr_C_1_type_always_Y),
    posterior =
      pr_type_Z_caused_Y * pr_C_type_Z_caused_Y / (pr_type_Z_caused_Y * pr_C_type_Z_caused_Y + pr_C_type_always_Y * (1 - pr_type_Z_caused_Y)),
    label = "Smoking Gun",
    estimand_label = "did_Z_cause_Y",
    handler = summarize) +
  declare_estimator(
    pr_type_Z_caused_Y = .5,
    pr_C1_1_type_Z_caused_Y = .30,
    pr_C1_1_type_always_Y = .005,
    pr_C2_1_type_Z_caused_Y = .75,
    pr_C2_1_type_always_Y = .25,
    rho = 0,
    pr_C_type_Z_caused_Y = joint_prob(pr_C1_1_type_Z_caused_Y, pr_C2_1_type_Z_caused_Y, 
                                      rho, which_prob = C1C2),
    pr_C_type_always_Y = joint_prob(pr_C1_1_type_always_Y, pr_C2_1_type_always_Y, 
                                      rho, which_prob = C1C2),
    posterior =
      pr_type_Z_caused_Y * pr_C_type_Z_caused_Y / (pr_type_Z_caused_Y * pr_C_type_Z_caused_Y + pr_C_type_always_Y * (1 - pr_type_Z_caused_Y)),
    label = "Joint Updating",
    estimand_label = "did_Z_cause_Y",
    handler = summarize) 


# diagnose_design(design,
#                 diagnosands = declare_diagnosands(
#                   bias = mean(posterior - estimand),
#                   rmse = sqrt(mean((posterior - estimand) ^ 2)),
#                   mean_estimand = mean(estimand),
#                   mean_posterior = mean(posterior),
#                   keep_defaults = FALSE), 
#                 sims = 500)

# design_sims <- simulate_design(design, sims = 500)


dag <- dagify(C1 ~ type,
              C2 ~ type,
              Y ~ Z + type)


nodes <-
  tibble(
    name = c( "type","Z","C1", "C2", "Y"),
    label = name,
    annotation = c(
      "**Type**<br>Causal relationship<br>between economic diversification<br>and breakdown of institutions",
      "**Cause**<br>Economic Diversification",
      "**Clue 1**<br>Smoking gun interview evidence",
      "**Clue 2**<br>Straw-in-the-wind archival evidence",
      "**Outcome**<br>Breakdown of traditional<br>institutions"),
    x = c(1, 1, 5, 5, 5),
    y = c(1.5,3.5,2,1.5,3.5),
    nudge_direction = c("S", "N", "N", "S","N"),
    answer_strategy = "uncontrolled"
  )


ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df
