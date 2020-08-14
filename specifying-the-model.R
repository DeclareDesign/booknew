# ---
# Specifying the model
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(dagitty)
library(dddag)
library(ggraph)

a_blue <- "#0072B2"
a_gray <- "grey80"

dag <-
  dagify(Y ~ X + Z + U,
         X ~ U,
         latent = "U")

gg_df <-
  tidy_dagitty(dag,
               layout = "manual",
               x = c(1, 0, -1, 1),
               y = c(1, 1, 0, 0))

gg_df <-
  gg_df %>%
  mutate(
    color = case_when(
      name == "U" ~ a_gray,
      name == "X" ~ a_blue,
      name == "Y" ~ a_blue,
      name == "Z" ~ a_blue
    )
  )


g <- 
ggplot(gg_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(aes(color = color)) +
  scale_color_identity() +
  geom_dag_text(color = "black", family = "Helvetica") +
  geom_dag_edges() +
  theme_dag()

g

diff_in_cates <- 0.5
design <-
  declare_population(N = 100,
                     U = rnorm(N),
                     X = rbinom(N, 1, prob = pnorm(0.5 * U + rnorm(N)))) +
  declare_potential_outcomes(Y ~ 0.5 * X + 0.5 * Z + diff_in_cates * X * Z + 0.5 * U) +
  declare_estimands(
    ATE = mean(Y_Z_1 - Y_Z_0),
    CATE_1 = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]),
    CATE_0 = mean(Y_Z_1[X == 0] - Y_Z_0[X == 0]),
    DiC = mean(Y_Z_1[X == 1] - Y_Z_0[X == 1]) - mean(Y_Z_1[X == 0] - Y_Z_0[X == 0])
  )

design %>% draw_estimands %>% kable

dag <-
  dagify(Yt2 ~ Yt1 + U,
         Yt1 ~ D + C + Yt0 + U,
         Yt0 ~ U,
         Z ~ U,
         C ~ Z + U,
         D ~ C + U)

# tidy_dagitty(dag) %>% as_tibble %>% pull(name) %>% unique

gg_df <-
  tidy_dagitty(
    dag,
    layout = "manual",
    x = c(3, 4, 3, 1, 5, 2, 6),
    y = c(1, 1, 2, 0, 0, 1, 0)
  )


label_df <- tibble(
  name = c("C", "D", "U", "Yt0", "Yt1", "Yt2", "Z"),
  label = c("C", "D", "U", "Y<sub>t=0</sub>", "Y<sub>t=1</sub>", "Y<sub>t=2</sub>", "Z")
)

gg_df <-
  gg_df %>%
  mutate(arced_left = (name == "U" & to == "Yt0"),
         arced_right = (name == "U" & to %in% c("Yt1", "Yt2")),
         dashed = (name == "U" & to == "Z")) %>%
  arrange(name) %>% 
  as_tibble

gg_df <- left_join(gg_df, label_df)

g <-
  ggplot(data = filter(gg_df, !(arced_left | arced_right)), aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_node(color = "gray") +
  geom_richtext(color = "black",
                aes(label = label),
                size = 4,
                label.color = NA,
                fill = NA,
                label.padding = grid::unit(rep(0, 4), "pt")) +
  geom_dag_edges(aes(edge_linetype = dashed)) +
  geom_dag_edges_arc(data = filter(gg_df, arced_left), curvature = -.5) +
  geom_dag_edges_arc(data = filter(gg_df, arced_right), curvature = +.3) +
  theme_dag() +
  theme(legend.position = "none")
g


tau <- 0.15
cutoff <- 0.5
bandwidth <- 0.5

polynom_func <- function(x, coefs){ 
  as.vector(poly(x, length(coefs), raw = T) %*% coefs)
}

cholera_design <- 
  declare_population(
    houses = add_level(
      N = 1722,
      pump_border_distance = runif(N, 0, 1) - cutoff, 
      bsp = if_else(pump_border_distance > 0, 1, 0),
      noise = rnorm(N, mean = 0, sd = 0.1)
    )
  ) +
  
  declare_potential_outcomes(
    ln_prices_1864_bsp_0 = polynom_func(pump_border_distance, coefs = c(0.5, 0.5)) + noise, 
    ln_prices_1864_bsp_1 = polynom_func(pump_border_distance, coefs = c(-5, 1)) + tau + noise
  ) + 
  
  declare_estimand(
    LATE_bsp = (polynom_func(0, coefs = c(-5, 1)) + tau) - polynom_func(0, coefs = c(0.5, 0.5))) + 
  
  declare_reveal(ln_prices_1864, bsp) + 
  
  declare_estimator(
    ln_prices_1864 ~ stats::poly(pump_border_distance, degree = 4) * bsp, 
    subset = (pump_border_distance > 0 - abs(bandwidth)) & pump_border_distance < 0 + abs(bandwidth),
    model = lm_robust, 
    term = "bsp", estimand = "LATE_bsp")
  
diagnose_design(cholera_design, sims = 500)

pro_con_colors <- c("#C67800", "#205C8A")
mock_data <- draw_data(cholera_design)
pump_border_distance <- seq(-.5,.5,.005)
treatment_frame <- data.frame(
  pump_border_distance = pump_border_distance,
  ln_prices_1864 = polynom_func(pump_border_distance, coefs = c(-5, 1)),
  observed = if_else(pump_border_distance > 0, "a", "b"),
  bsp = 1
  )
control_frame <- data.frame(
  pump_border_distance = pump_border_distance,
  ln_prices_1864 = polynom_func(pump_border_distance, coefs = c(0.5, 0.5)),
  observed = if_else(pump_border_distance <= 0, "a", "b"),
  bsp = 0
  )
plot_frame <- bind_rows(treatment_frame, control_frame)

ggplot(plot_frame, aes(x = pump_border_distance, y = ln_prices_1864, color = as.factor(bsp))) +
  geom_line(aes(linetype = observed)) +
  geom_point(data = mock_data, alpha = .2, size = .5) +
  scale_linetype_discrete(name = "", labels = c("Observable", "Unobservable")) +
  scale_color_manual(
    name = "",
    labels = c("Untreated", "Treated"),
    values = pro_con_colors
  ) +
  geom_vline(xintercept = 0, size = .05) +
  ylab("Ln House Prices (1864)") + 
  xlab("Distance from Broad Street Pump Coverage Boundary") +
  geom_segment(aes(
    x = 0,
    xend = 0,
    y = polynom_func(0, coefs = c(0.5, 0.5)),
    yend = polynom_func(0, coefs = c(-5, 1))
  ), color = "black") +
  dd_theme()

population <- declare_population(N = 8, e = runif(N), X = rnorm(N, mean = e, sd = 1)) 

declare_potential_outcomes()

potential_outcomes <- declare_potential_outcomes(Y_Z_0 = .5 < e,
                                                 Y_Z_1 = .5 < e + .05)

potential_outcomes <- declare_potential_outcomes(Y ~ .5 < e + .05 * Z)

npotential_outcomes <- declare_potential_outcomes(
  income ~ employed + education + u, 
  assignment_variables = list(employed = c("No","Yes"), education = c(10,12)))

potential_outcomes <- declare_potential_outcomes(
  Y_X_0_Z_0 = .5 < e,
  Y_X_0_Z_1 = .5 < e + .05,
  Y_X_1_Z_0 = .5 < e,
  Y_X_1_Z_1 = .5 < e + .05 + .05)

## declare_potential_outcomes(Y ~ .5 < e + Z * .05 + Z * X * .05,
##   assignment_variables = list(Z = 0:1, X = 0:1))

potential_outcomes <- declare_potential_outcomes(
  X_Z_0 = .5 < e * 0.75,
  X_Z_1 = .5 < e * 1.25,
  Y_X_0 = .5 < e,
  Y_X_1 = .5 < e + .05)
