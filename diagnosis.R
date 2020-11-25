# ---
# Diagnosis
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(ggforce)
library(ggridges)

dag <-
  dagify(aw ~ w + I,
         m ~ M,
         am ~ m + I,
         d ~ D + w,
         ad ~ A + d,
         w ~ W)

dag_base <- tidy_dagitty(dag) %>%
  select(name, direction, to, circular) %>%
  as_tibble

nodes_df <-
  tibble(
    name = c("M", "I", "D", "A", 
             "m", "am", "aw", "d", 
             "ad", "w", "W"),
    label = TeX(c("M", "I", "D", "A", "m", "a^m", "a^w", "d", "a^d", "w", "W")),
    long_label = c("Theoretical<br>causal model", "Inquiry<br>", "Data<br>strategy", "Answer<br>strategy", "Model<br>draw", "Theoretical<br>answer", "True<br>answer", "Realized<br>data", "Empirical<br>answer", "Real<br>world", "True causal<br>model"),
    lbl_direction = c("N", "N", "N", "N", "S", "S", "S", "S", "S", "S", "N"),
    x = c(1.5, 3.5, 11.5, 13.5, 
          1.5, 3.5, 8.5, 11.5, 
          13.5, 6.5, 6.5),
    y = c(3.5, 3.5, 3.5, 3.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 3.5)
  )

endnodes_df <-
  nodes_df %>%
  transmute(to = name, xend = x, yend = y)

gg_df <-
  dag_base %>%
  left_join(nodes_df, by = "name") %>%
  left_join(endnodes_df, by = "to")

gg_df <-
  gg_df %>%
  mutate(arced1 = (name == "w" & to == "d"),
         arced2 = (name == "I" & to == "aw")) %>%
  arrange(name)

rect_df <-
  tibble(
    xmin = c(0, 10),
    xmax = c(5, 15),
    ymin = c(0, 0),
    ymax = c(5, 5)
  )

g1 <-
  ggplot(data = filter(gg_df, !arced1 & !arced2), aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_point(color = gray(.1), fill = NA, size = 14, stroke = 0.5, pch = 1) +
  geom_dag_edges(edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced1), curvature = -0.25, edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced2), curvature = .64, edge_width = 0.35) +
  geom_text(color = "black",
            aes(label = label),
            size = 4,
            parse = TRUE) +
  geom_richtext(aes(y = y + if_else(lbl_direction == "N", 0.75, -0.75),
        vjust = if_else(lbl_direction == "N", "bottom", "top"),
        label = long_label),
    color = gray(0.5),
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 3) +
  geom_rect(data = rect_df, aes(x = NULL, y = NULL, 
                                xend = NULL, yend = NULL,
                                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.15) +
  annotate("text", x = 2.5, y = 5.35, label = "Theory") +
  annotate("text", x = 12.5, y = 5.35, label = "Empirics") +
  annotate("text", x = 7.5, y = 5.35, label = "Reality") +
  annotate("text", x = -0.5, y = 2.5, label = "Actual Research Design", angle = 90) +
  scale_x_continuous(breaks = 0:15) +
  scale_y_continuous(breaks = 0:5) +
  coord_fixed(clip = "off") + 
  theme_dag()

dag <-
  dagify(m ~ M,
         am ~ m + I,
         d ~ D + m,
         ad ~ A + d)

dag_base <- tidy_dagitty(dag) %>%
  select(name, direction, to, circular) %>%
  as_tibble

nodes_df <-
  tibble(
    name = c("M", "I", "D", "A", 
             "m", "am", "aw", "d", 
             "ad", "w", "W"),
    label = TeX(c("M", "I", "D", "A", "m_k", "a^m_k", "a^w", "d_k", "a^d_k", "w", "W")),
    long_label = c("Theoretical<br>causal model", "Inquiry<br>", "Data<br>strategy", "Answer<br>strategy", "Model<br>draw", "Theoretical<br>answer", "True<br>answer", "Simulated<br>data", "Simulated<br>answer", "Real<br>world", "True causal<br>model"),
    lbl_direction = c("N", "N", "N", "N", "S", "S", "S", "S", "S", "S", "N"),
    x = c(1.5, 3.5, 11.5, 13.5, 
          1.5, 3.5, 8.5, 11.5, 
          13.5, 6.5, 6.5),
    y = c(3.5, 3.5, 3.5, 3.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 3.5)
  )

endnodes_df <-
  nodes_df %>%
  transmute(to = name, xend = x, yend = y)

gg_df <-
  dag_base %>%
  left_join(nodes_df, by = "name") %>%
  left_join(endnodes_df, by = "to")

gg_df <-
  gg_df %>%
  mutate(arced1 = (name == "m" & to == "d"),
         arced2 = (name == "I" & to == "aw")) %>%
  arrange(name)

rect_df <-
  tibble(
    xmin = c(0, 10),
    xmax = c(5, 15),
    ymin = c(0, 0),
    ymax = c(5, 5)
  )

g2 <-
  ggplot(data = filter(gg_df, !arced1 & !arced2), aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_point(color = gray(.1), fill = NA, size = 14, stroke = 0.5, pch = 1) +
  geom_dag_edges(edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced1), curvature = -0.2, edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced2), curvature = 0.64, edge_width = 0.35) +
  geom_text(
    color = "black",
    aes(label = label, y = y),
    size = 4,
    parse = TRUE
  ) +
  geom_richtext(aes(y = y + if_else(lbl_direction == "N", 0.75, -0.75),
        vjust = if_else(lbl_direction == "N", "bottom", "top"),
        label = long_label),
    color = gray(0.5),
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 3) +
  geom_rect(data = rect_df, aes(x = NULL, y = NULL, 
                                xend = NULL, yend = NULL,
                                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.15) +
  annotate("text", x = 2.5, y = 5.35, label = "Simulated Theory") +
  annotate("text", x = 12.5, y = 5.35, label = "Simulated Empirics") +
  annotate("text", x = -0.5, y = 2.5, label = "Simulated Research Design", angle = 90) +
  scale_x_continuous(breaks = 0:15) +
  scale_y_continuous(breaks = 0:5) +
  coord_fixed(clip = "off") + 
  theme_dag()

g1 / g2

## design <-
##   declare_population(N = 100,
##                      tau = rnorm(N, mean = 0.1, sd = 0.1),
##                      U = rnorm(N, 0, 0.2)) +
##   declare_potential_outcomes(Y ~  tau * Z + U) +
##   declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
##   declare_assignment(prob = 0.5) +
##   declare_estimator(Y ~ Z, estimand = "ATE")

set.seed(343)
design <-
  declare_population(N = 500, 
                     tau = rnorm(N, mean = 0.1, sd = 0.1),
                     U = rnorm(N)) +
  declare_potential_outcomes(Y ~ tau * Z + U) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(prob = 0.5) +
  declare_estimator(Y ~ Z, estimand = "ATE")

simulations <- 
  simulate_design(design, sims = 10) %>%
  mutate(significant = p.value <= 0.05)

estimation_df <-
  simulations %>%
  group_by(sim_ID) %>%
  nest %>%
  mutate(densities = lapply(data, function(dat) {
    with(dat, tibble(
      x = seq(-2, 2, 0.0001),
      density = dnorm(x, mean = estimate, sd = std.error)
    ))
  })) %>%
  unnest(cols = c(data, densities)) %>%
  group_by(sim_ID)

hypothesis_df <- 
  simulations %>%
  group_by(sim_ID) %>%
  nest %>%
  mutate(densities = lapply(data, function(dat) {
    with(dat, tibble(
      x = seq(-3.4, 3.4, 0.0001),
      density = dt(x = x, df = df)
    ))
  })) %>%
  unnest(cols = c(data, densities)) %>%
  group_by(sim_ID) %>%
  mutate(
    low_cut = case_when(
      statistic < 0 ~ x < statistic,
      statistic > 0 ~ x <  -1 *statistic),
    high_cut = case_when(statistic < 0 ~ x >= -1 *statistic,
                         statistic > 0 ~ x >=  statistic),
    area_under = case_when(low_cut ~ "low",
                           high_cut ~ "high",
                           TRUE ~ "middle")
  ) 

## run_design(design)

simulations %>% 
  filter(sim_ID == 1) %>%
  select(estimand, estimate, std.error, df, p.value, conf.low, conf.high) %>%
  kable(digits = 3, caption = "One simulation draw", booktabs = TRUE)

simulation_1 <- simulations %>% filter(sim_ID == 1) 
estimation_df_1 <- estimation_df %>% filter(sim_ID == 1)
hypothesis_df_1 <- hypothesis_df %>% filter(sim_ID == 1)

g_hypothesis_1 <-
  ggplot(simulation_1, aes( y = 0),
         color = dd_light_blue) +
  geom_ribbon(data = hypothesis_df_1,
              size = 0,
              aes(
                x = x,
                ymin = 0,
                ymax = density,
                fill = area_under
              ),) +
  # then the density lines (to make it plot correctly and not be cut off)
  geom_line(data = hypothesis_df_1,
            size = 0.1,
            aes(x = x, y = density),
            color = dd_light_blue) +
  geom_point(aes(x = statistic), size = 1.5, color = gray(0.5)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  )  +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_light_blue_alpha, dd_light_blue_alpha, NA)) +
  geom_richtext(
    aes(x = 0, y = 0.42, label = paste0("Hypothesized null distribution: t(df = ", format_num(df, 0), ")")),
    size = 2.5,
    fill = NA,
    label.colour = NA
  ) +
  geom_text(
    aes(x = statistic, y = -0.026, label = paste0("t-statistic: ",format_num(statistic))),
    size = 2.4,
  ) +
  geom_text(
    aes(x = -statistic, y = -0.026, label = paste0("p-value: 2*",format_num(p.value/2), " = ", format_num(p.value))),
    size = 2.4,
  ) +
  coord_cartesian(xlim = c(-3.5, 3.5), ylim = c(-0.035, NA)) 

g_estimation_1 <- 
ggplot(simulation_1, aes(y = 0), color = dd_light_blue) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.075,
                 size = 0.25,
                 color = gray(0.5)) +
  geom_point(aes(x = estimate),
             size = 1.5, color = gray(0.5)) +
  geom_point(
    aes(x = estimand, y = -0.5),
    size = 1.5,
    fill = NA,
    color = gray(0.5),
    pch = 24
  ) +
  geom_line(
    data = estimation_df_1,
    size = 0.1,
    aes(x = x, y = density),
    color = dd_light_blue,
  ) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_text(
    aes(x = estimand - 0.05, y = -0.5, label = paste0("Estimand: ",format_num(estimand))),
    hjust = 1,
    size = 2.5,
  ) +
  geom_text(
    aes(x = estimate, y = 0.5, label = paste0("Estimate: ",format_num(estimate))),
    size = 2.5,
  ) +
  geom_text(
    aes(x = estimate, y = 5.5, label = paste0("Estimated sampling distribution:\nN(", format_num(estimate), ", ", format_num(std.error), ")")),
    size = 2.5,
    fill = NA,
    label.colour = NA
  ) +
  geom_text(
    aes(x = conf.high, y = -0.5, label = paste0("95% CI: ", make_interval_entry(conf.low, conf.high, digits = 3))),
    hjust = 0.4,
    size = 2.5
  ) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(xlim = c(-0.250, 0.550), ylim = c(-0.5, 6))

g_estimation_1 + g_hypothesis_1

g_estimation <- 
ggplot(simulations, aes(x = estimate, y = as.factor(sim_ID)), color = dd_light_blue) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.075,
                 size = 0.25,
                 color = gray(0.5)) +
  geom_point(size = 1.5, color = gray(0.5)) +
  geom_point(
    aes(x = estimand),
    size = 1.5,
    fill = NA,
    color = gray(0.5),
    pch = 24,
    position = position_nudge(y = -0.15)
  ) +
  geom_ridgeline(
    data = estimation_df,
    size = 0.1,
    aes(x = x, height = density, fill = NA),
    scale = 0.05,
    min_height = 0.01,
    alpha = 0,
    color = dd_light_blue,
  ) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 7)
  ) +
  coord_cartesian(xlim = c(-0.250, 0.350)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = 0.1,
    color = dd_light_blue,
    linetype = "dotted",
    alpha = 0.5
  ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_light_blue_alpha, dd_light_blue_alpha, NA)) +
  ggtitle("Estimates and Estimands")


g_hypothesis <- 
ggplot(simulations, aes(x = statistic, y = as.factor(sim_ID)), color = dd_light_blue) +
  # first plot the density for p-values
  geom_ridgeline(
    data = hypothesis_df,
    size = 0,
    aes(x = x, height = density, fill = area_under),
    min_height = 0.01
  ) +
  # then the density lines (to make it plot correctly and not be cut off)
  geom_ridgeline(
    data = hypothesis_df,
    size = 0.1,
    aes(x = x, height = density, fill = NA),
    min_height = 0.01,
    color = dd_light_blue,
  ) +
  geom_point(size = 1.5, color = gray(0.5)) +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 7)
  ) +
  # coord_cartesian(xlim = c(-0.250, 0.350)) +
  geom_vline(
    xintercept = 0,
    color = gray(0.5),
    linetype = "dashed",
    alpha = 0.5
  ) +
  scale_fill_manual(name = "Probability",
                    values = c(dd_light_blue_alpha, dd_light_blue_alpha, NA))+
  ggtitle("Null hypothesis tests")

g_estimation + g_hypothesis

## diagnosis <-
##   diagnose_design(
##     design, sims = 1000,
##     diagnosands = declare_diagnosands(
##       bias = mean(estimate - estimand),
##       power = mean(p.value <= 0.05)
##     )
##   )





diagnosis$diagnosands_df %>% 
transmute(bias = make_se_entry(bias, `se(bias)`), 
power = make_se_entry(power, `se(power)`)) %>% 
kable(digits = 3, caption = "Diagnosand estimates with bootstrapped standard errors.", booktabs = TRUE)

n <- 50

points_df <- tibble(rho = sqrt(runif(n)),
                    theta = runif(n, 0, 2*pi),
                    x = rho * cos(theta),
                    y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

unbiased_lowprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Unbiased, imprecise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )


points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta),
  y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

unbiased_highprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Unbiased, precise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta) + 0.5,
  y = rho * sin(theta) - 0.2
)

summary_df <- points_df %>% 
  summarize(
    bias = round(mean(sqrt(x^2 + y^2)), 2),
    mse = round(mean(x^2 + y^2), 2),
    var = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3)
  )

biased_highprecision <- ggplot() + 
  geom_circle(
    data = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ), 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25, alpha = 0.5) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Biased, precise", subtitle = glue::glue("Variance = {summary_df$var}\nBias = {summary_df$bias}\nRMSE = {summary_df$mse}")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )

unbiased_lowprecision + unbiased_highprecision + biased_highprecision

n <- 50

points_df <- tibble(
  rho = sqrt(runif(n, min = 0, max = 0.015)),
  theta = runif(n, 0, 2*pi),
  x = rho * cos(theta),
  y = rho * sin(theta)
)

summary_df <- points_df %>% 
  summarize(
    bias_1 = round(mean(sqrt(x^2 + y^2)), 2),
    mse_1 = round(mean(x^2 + y^2), 2),
    var_1 = round(mean((x - mean(x))^2 + (y - mean(y))^2), 3),
    
    bias_2 = round(mean(sqrt((x-0.25)^2 + y^2)), 2),
    mse_2 = round(mean((x-0.25)^2 + y^2), 2),
    var_2 = round(mean(((x-0.25) - mean(x-0.25))^2 + (y - mean(y))^2), 3)
  )

circle_df <- 
  bind_rows(
    "left" = tibble(
      x0 = rep(0, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ),
    "right" = tibble(
      x0 = rep(0.35, 5),
      y0 = rep(0, 5),
      r = rev(seq(0.1, 1, length.out = 5))
    ),
    .id = "side"
  ) %>% 
  arrange(desc(r), side)

dual_estimands <- ggplot() + 
  geom_circle(
    data = circle_df, 
    aes(x0 = x0, y0 = y0, r = r, fill = fct_rev(as.factor(r))),
    col = gray(0.25), lwd = 0.25) +
  geom_point(data = points_df, aes(x, y), size = 0.5) + 
  scale_fill_manual(values = rev(c("yellow", "red", "cyan", "black", "white"))) + 
  coord_fixed() + 
  dd_theme() + 
  xlab("") + ylab("") + 
  ggtitle("Diagnosands depend on the estimand", subtitle = glue::glue("Variance = {summary_df$var_1} (left), {summary_df$var_2} (right)\nBias = {summary_df$bias_1} (left), {summary_df$bias_2} (right)\nRMSE = {summary_df$mse_1} (left), {summary_df$mse_2} (right)")) + 
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
dual_estimands

## model <- declare_population(
##   case_level = add_level(N = 1, b = rnorm(N, 1, 1)),
##   unit_level = add_level(N = 20, Z = rep(0:1, N/2), Y = b*Z + rnorm(N))) +
##   declare_estimand(super_b = 1, case_b = b[1]) +
##   declare_estimator(Y~Z, estimand = c("super_b", "case_b"))
## 
## diagnose_design(model, sims = 100)

## robustness_checks_design <-
##   robustness_checks_design +
##   declare_estimator(handler = label_estimator(interacted_correlation_decision), label = "interacted")
## 
## robustness_checks_design_dgp2 <- replace_step(
##   robustness_checks_design,
##   step = 1,
##   new_step =
##     declare_population(
##       N = 100,
##       x = rnorm(N),
##       y1 = rnorm(N),
##       y2 = 0.15 * y1 + 0.01 * x + 0.05 * y1 * x + rnorm(N)
##     )
## )
## 
## robustness_checks_design_dgp3 <- replace_step(
##   robustness_checks_design,
##   step = 1,
##   new_step =
##     declare_population(
##       N = 100,
##       x = rnorm(N),
##       y1 = 0.15 * x + rnorm(N),
##       y2 = 0.15 * x + rnorm(N)
##     )
## )
## 
## robustness_checks_design_dgp3 <- replace_step(
##   robustness_checks_design_dgp3,
##   step = 2,
##   new_step = declare_estimand(y1_y2_are_related = FALSE)
## )
## 
## decision_diagnosis <- declare_diagnosands(correct = mean(decision == estimand))
## 
## diag <- diagnose_design(
##   robustness_checks_design, robustness_checks_design_dgp2, robustness_checks_design_dgp3,
##   sims = sims, diagnosands = decision_diagnosis)
