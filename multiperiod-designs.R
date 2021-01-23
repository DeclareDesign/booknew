# ---
# Multiperiod Designs
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

design <-
  declare_population(
    unit = add_level(N = 8,
                     X = rnorm(N)),
    period = add_level(
      N = 3,
      time = as.numeric(period),
      p = c(1 / 4, 1 / 4 + 1 / 4, 1 / 4 + 1 / 4 + 1 / 4),
      nest = FALSE
    ),
    obs = cross_levels(by = join(unit, period),
                       U = rnorm(N))
  ) +
  declare_potential_outcomes(Y ~ X + U + Z * time) +
  declare_assignment(
    clusters = unit,
    conditions = 1:4,
    assignment_variable = "wave"
  ) +
  declare_assignment(Z = as.numeric(time >= wave),
                     ipw = 1 / (Z * p + (1 - Z) * (1 - p)),
                     handler = fabricate) +
  declare_reveal(Y, Z) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_estimator(
    Y ~ Z,
    model = lm_robust,
    estimand = "ATE",
    label = "1: Stepped Wedge",
    weights = ipw,
    clusters = unit
  ) +
  declare_estimator(
    Y ~ Z,
    model = lm_robust,
    estimand = "ATE",
    label = "2: Wave 2 Only",
    subset = period == 2
  )

dat <- draw_data(design)

treatment_color <- "royalblue2"
control_color <- "darkorange3"

dat %>% 
  arrange(wave,unit) %>% 
  group_by(period) %>%
  mutate(unit = 1:n(), Assignment = ifelse(Z == 1, "Treatment", "Control")) %>% 
  ggplot(aes(x = period, y = unit, fill = Assignment)) +
  geom_tile(color = "white") + 
  scale_fill_manual(values = c(control_color, treatment_color)) +
  # scale_fill_grey(start = .9,end = .5) +
  geom_text(aes(label = round(ipw,2))) +
  dd_theme() +
  theme(legend.position = "right")

dag <- dagify(Y ~ Z + X + U + time,
              Z ~ time)

nodes <-
  tibble(
    name = c("X", "U", "time", "Z", "Y"),
    label = c("X", "U", "T", "Z", "Y"),
    annotation = c(
      "**Unknown heterogeneity**<br>Unit effects",
      "**Unknown heterogeneity**<br>",
      "**Time period**<br>",
      "**Treatment assignment**<br>",
      "**Outcome variable**<br>"
    ),
    x = c(1, 5, 1, 3, 5),
    y = c(4, 4, 1, 2.5, 2.5), 
    nudge_direction = c("N", "N", "S", "N", "S"),
    data_strategy = c("unmanipulated", "unmanipulated", "unmanipulated", "assignment", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df
