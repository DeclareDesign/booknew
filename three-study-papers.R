# ---
# Three-study papers
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(reshape2)

n1 <- 100
n2 <- 100
n3 <- 100
rho <- .5
gamma <- tau <- .2

generate_study_sample <- function(n, rho, tau, gamma, data){
  fabricate(N = n, X = rnorm(N), M = rnorm(N, X * rho, sqrt(1 - rho^2)), 
            U = rnorm(N), Y = tau * X + gamma * M * X + U)
}

three_study_design <-
  # Study 1 -- Bivariate correlation between X and Y
  declare_model(
    n = n1,
    tau = tau,
    gamma = gamma,
    rho = rho,
    handler = generate_study_sample
  ) +
  declare_estimator(Y ~ X,
                    term = "X",
                    model = lm_robust,
                    label = "Study 1") +
  # Study 2 -- Bivariate correlation between M and X
  declare_model(
    n = n2,
    tau = tau,
    gamma = gamma,
    rho = rho,
    handler = generate_study_sample
  ) +
  declare_estimator(M ~ X,
                    term = "X",
                    model = lm_robust,
                    label = "Study 2") +
  # Study 3 -- Interaction in X and M
  declare_model(
    n = n3,
    tau = tau,
    gamma = gamma,
    rho = rho,
    handler = generate_study_sample
  ) +
  declare_estimator(Y ~ X + M + X:M,
                    term = "X:M",
                    model = lm_robust,
                    label = "Study 3") 

dag <- dagify(M ~ X,
              Y ~ X + M)

nodes <-
  tibble(
    name = c("X", "M", "Y"),
    label =  name,
    annotation = c(
      "**Treatment**",
      "**Mediator**",
      "**Outcome**"
    ),
    x = c(1, 3, 5),
    y = c(1.5, 3.5, 1.5), 
    nudge_direction = c("S", "N", "S"),
    data_strategy = c("assignment", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df






three_study_diagnosands <- 
  simulations %>% 
 group_by(sim_ID) %>% 
  mutate(all_significant = all(p.value < .05),
         any_significant = any(p.value < .05),
         all_positive = all(estimate > 0), 
         any_positive = any(estimate > 0)) %>% 
  group_by(estimator_label) %>% 
  summarize(
    all_significant = mean(all_significant),
    any_significant = mean(any_significant),
    power = mean(p.value < .05),
    all_positive = mean(all_positive),
    any_positive = mean(any_positive),
    positive = mean(estimate > 0)) %>% 
  mutate(tau = .2, gamma = .2, rho = .5)

null_three_study_diagnosands <- 
  null_simulations %>% 
  group_by(sim_ID) %>% 
  mutate(all_significant = all(p.value < .05),
         any_significant = any(p.value < .05),
         all_positive = all(estimate > 0), 
         any_positive = any(estimate > 0)) %>% 
  group_by(estimator_label) %>% 
  summarize(
    all_significant = mean(all_significant),
    any_significant = mean(any_significant),
    power = mean(p.value < .05),
    all_positive = mean(all_positive),
    any_positive = mean(any_positive),
    positive = mean(estimate > 0)) %>% 
  mutate(tau = 0, gamma = 0, rho = 0)


rbind(three_study_diagnosands,
      null_three_study_diagnosands) %>% 
  select(tau, estimator_label, gamma, power, all_significant, positive, all_positive) %>% kable(booktabs = TRUE)


rho <- .5
tau <- .2

N_studies <- 30


replication_design <-
  declare_model(
    n = 100,
    tau = tau,
    gamma = gamma,
    rho = rho,
    handler = generate_study_sample
  ) +
  declare_estimator(Y ~ X,
                    term = "X",
                    model = lm_robust,
                    label = paste0("Study 1")) 

for(i in 2:N_studies) {
  replication_design <-
    replication_design +
    declare_model(
      n = 100,
      tau = tau,
      gamma = gamma,
      rho = rho,
      handler = generate_study_sample
    ) +
    declare_estimator(Y ~ X,
                      term = "X",
                      model = lm_robust,
                      label = paste0("Study ", i))
}


tau <- 0
null_replication_design <-
  declare_model(
    n = 100,
    tau = tau,
    gamma = gamma,
    rho = rho,
    handler = generate_study_sample
  ) +
  declare_estimator(Y ~ X,
                    term = "X",
                    model = lm_robust,
                    label = paste0("Study 1"))

for (i in 2:N_studies) {
  null_replication_design <-
    null_replication_design +
    declare_model(
      n = 100,
      tau = tau,
      gamma = gamma,
      rho = rho,
      handler = generate_study_sample
    ) +
    declare_estimator(Y ~ X,
                      term = "X",
                      model = lm_robust,
                      label = paste0("Study ", i))
}





get_N_study_diagnosands <- function(simulations, N){
  studies <- paste0("Study ", 1:N)
  simulations %>% 
    filter(estimator_label %in% studies) %>% 
    group_by(sim_ID) %>% 
    mutate(all_significant = all(p.value < .05),
           any_significant = any(p.value < .05),
           prop_sig_95 = mean(p.value < .05) >= .95,
           all_positive = all(estimate > 0), 
           any_positive = any(estimate > 0)) %>% 
    group_by(estimator_label) %>% 
    summarize(
      all_significant = mean(all_significant),
      any_significant = mean(any_significant),
      power = mean(p.value < .05),
      prop_sig_95 = mean(prop_sig_95),
      all_positive = mean(all_positive),
      any_positive = mean(any_positive),
      positive = mean(estimate > 0)) %>% 
    select(-starts_with("estimator_label")) %>% 
    summarize_all("mean")
}

replication_diagnosands <-
  lapply(1:N_studies, get_N_study_diagnosands,
         simulations = replication_simulations) %>%
  do.call(what = "rbind")
replication_diagnosands$N_studies <- 1:nrow(replication_diagnosands)

null_replication_diagnosands <-
  lapply(1:N_studies, get_N_study_diagnosands,
         simulations = null_replication_simulations) %>%
  do.call(what = "rbind")
null_replication_diagnosands$N_studies <-
  1:nrow(null_replication_diagnosands)

plot_data <-
  rbind(
    melt(replication_diagnosands, id.vars = "N_studies") %>% mutate(tau = .2),
    melt(null_replication_diagnosands, id.vars = "N_studies") %>% mutate(tau = 0)
  )


plot_data  %>%
  filter(N_studies != 1) %>%
  filter(variable %in% c("all_significant", "all_positive")) %>%
  mutate(
    label = case_when(
      variable == "all_significant" ~ "Pr(All N studies have significant results)",
      variable == "all_positive" ~ "Pr(All N studies have positive results)",
      variable == "prop_sig_95" ~ "Pr(At least 95% of studies have significant results)"
    ),
    model = case_when(
      tau > 0 ~ "Model: all effects positive\n(Theory is correct)",
      tau == 0 ~ "Model: all effects zero\n(Theory is incorrect)"
    )
  ) %>%
  ggplot(aes(x = N_studies, y = value, color = label)) +
  geom_point() +
  facet_grid( ~ model) +
  scale_x_continuous(name = "Number of studies in design", breaks = seq(2, 30, 2)) +
  scale_y_continuous(name = "Probability observable implications support theory.") + 
  dd_theme() + 
  theme(legend.position = "bottom")

