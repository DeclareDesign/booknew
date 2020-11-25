# ---
# Regression Discontinuity
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R


cutoff <- 0.5
control <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(.7, -.8, .5, 1))}
treatment <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(0, -1.5, .5, .8)) + .15}

design <-
  declare_population(
    N = 1000,
    U = rnorm(N, 0, 0.1),
    X = runif(N, 0, 1) + U - cutoff,
    D = 1 * (X > 0)
  ) +
  declare_potential_outcomes(
    Y ~ D * treatment(X) + (1 - D) * control(X) + U,
    assignment_variable = D
  ) +
  declare_estimand(LATE = treatment(0) - control(0)) +
  reveal_outcomes(Y, D) +
  declare_estimator(
    Y ~ poly(X, 4) * D, model = lm_robust, estimand = "LATE"
  )

mock_data <- draw_data(design)
X <- seq(-.5, .5, .005)
treatment_frame <-
  data.frame(
    X = X,
    Y = treatment(X),
    observed = ifelse(X > 0, "a", "b"),
    D = 1
  )
control_frame <-
  data.frame(
    X = X,
    Y = control(X),
    observed = ifelse(X <= 0, "a", "b"),
    D = 0
  )
plot_frame <-
  rbind(treatment_frame, control_frame)

ggplot(plot_frame, aes(x = X, y = Y, color = as.factor(D))) +
  geom_line(aes(linetype = observed)) +
  geom_point(data = mock_data, alpha = .2, size = .5) +
  scale_linetype_discrete(name = "", labels = c("Observable", "Unobservable")) +
  scale_color_manual(name = "", values = c(dd_light_blue, dd_orange), labels = c("Untreated","Treated")) +
  geom_vline(xintercept = 0, size = .05) +
  xlab("Running Variable") +
  geom_segment(aes(
    x = 0,
    xend = 0,
    y = control(0),
    yend = treatment(0)
  ), color = "black") +
 dd_theme()

cutoff <- 0.5
control <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(.7, -.8, .5, 1))}
treatment <- function(X) {
  as.vector(poly(X, 4, raw = TRUE) %*% c(0, -1.5, .5, .8)) + .15}

design <-
  declare_population(
    N = 1000,
    U = rnorm(N, 0, 0.1),
    X = runif(N, 0, 1) + U - cutoff,
    D = 1 * (X > 0)
  ) +
  declare_potential_outcomes(
    Y ~ D * treatment(X) + (1 - D) * control(X) + U, 
    assignment_variable = D
  ) +
  declare_estimand(LATE = treatment(0) - control(0)) +
  reveal_outcomes(Y, D) +
  declare_estimator(
    Y ~ poly(X, 4) * D, 
    model = lm_robust, 
    estimand = "LATE"
  )

dag <- dagify(Y ~ D + X + U,
              D ~ X,
              X ~ U)

nodes <-
  tibble(
    name = c("U", "X", "D", "Y"),
    label = c("U", "X", "D", "Y"),
    annotation = c(
      "**Unknown heterogeneity**",
      "**Exogenous variable**",
      "**Treatment**",
      "**Outcome variable**"
    ),
    x = c(5, 1, 4, 5),
    y = c(4, 2.5, 2.5, 1), 
    nudge_direction = c("N", "S", "N", "S"),
    answer_strategy = c("uncontrolled", "controlled", "uncontrolled", "uncontrolled")
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df
