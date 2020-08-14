# ---
# Stepped wedge designs
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)


design <- 
  declare_population(
    unit = add_level(N = 8, X = rnorm(N)),
    period = add_level(N = 3, time = as.numeric(period), nest = FALSE),
    obs = cross_levels(by = join(unit, period), U = rnorm(N))
  ) + 
  declare_potential_outcomes(Y ~ X + U + Z * time) +
  declare_assignment(clusters = unit, conditions = 1:4, assignment_variable = "wave") + 
  declare_assignment(Z = as.numeric(time >= wave), ipw = 1 / (Z * 2/8 + (1 - Z) * (1 - 2/8)), handler = fabricate) + 
  declare_reveal(Y, Z) + 
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) + 
  declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE", label = "1: Wave 2 only", subset = period == 2) +
  declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE", label = "2: Weighted, clustered SW", weights = ipw, clusters = unit) +
  declare_estimator(Y ~ Z, model = lm_robust, estimand = "ATE", label = "3: Unweighted, unclustered SW") 

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
    answer_strategy = "uncontrolled",
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df



p_00 <- p_W1 <- p_W2 <- p_W3 <- 2/8

design <- declare_population(
  t = add_level(N = 3, trend = as.numeric(t), 
                # u_t = rnorm(N), 
                p = c(p_W1, p_W1 + p_W2, p_W1 + p_W2 + p_W3)),
  i = add_level(N = 8, u_i = rnorm(N), nest = FALSE),
  obs = cross_levels(by = join(t, i), u_it = rnorm(N))) + 
  declare_potential_outcomes(Y_Z_0 = u_i + u_it, Y_Z_1 = u_i + u_it + trend) +
  declare_assignment(clusters = i, conditions = 1:4, 
                     prob_each = c(p_W1, p_W2, p_W3, p_00),
                     assignment_variable = "wave") + 
  declare_step(Z = as.numeric(t >= wave), 
               ip = 1 / (Z * p + (1 - Z) * (1 - p)),
               handler = fabricate) + 
  declare_reveal(Y, Z) + 
  declare_estimand(ate = mean(Y_Z_1 - Y_Z_0)) + 
  declare_estimator(Y ~ Z, model = lm_robust, label = "1: Wave 2 only", subset = t == 2) +
  declare_estimator(Y ~ Z, model = lm_robust, label = "2: Weighted, clustered SW", weights = ip, clusters = i) +
  declare_estimator(Y ~ Z, model = lm_robust, label = "3: Unweighted, unclustered SW") 

dat <- draw_data(design)

treatment_color <- "royalblue2"
control_color <- "darkorange3"

sw_plot <- dat %>% 
  arrange(wave,i) %>% 
  group_by(t) %>%
  mutate(i = 1:n(), Assignment = ifelse(Z == 1, "Treatment", "Control")) %>% 
  ggplot(aes(x = t, y = i, fill = Assignment)) +
  geom_tile(color = "white") + 
  scale_fill_manual(values = c(control_color, treatment_color)) +
  # scale_fill_grey(start = .9,end = .5) +
  geom_text(aes(label = round(ip,2))) +
  dd_theme() +
  theme(legend.position = "right")



ate_by_wave_plot <- dat %>% 
  group_by(t) %>%
  summarize(Y_Z_1 = mean(Y_Z_1), Y_Z_0 = mean(Y_Z_0)) %>% 
  mutate(t = as.numeric(t),
    ate = Y_Z_1 - Y_Z_0, 
         label_y_position = Y_Z_0 + ate/2, 
         label_x_position = t + .15,
         label = paste0("ATE at wave ",t,":\n",ate)) %>% 
  ggplot(aes(x = t)) +
  geom_segment(aes(x = t, xend = t, y = Y_Z_0, yend = Y_Z_1), linetype = "dashed", alpha = .5) +
  geom_line(aes(y = Y_Z_1), color = treatment_color) +
  geom_point(aes(y = Y_Z_1), color = treatment_color) +
  geom_line(aes(y = Y_Z_0), color = control_color) +
  geom_point(aes(y = Y_Z_0), color = control_color) +
  geom_text(aes(x = label_x_position, y = label_y_position, label = label)) +
  geom_text(aes(x = label_x_position[3], y = Y_Z_1[3]), label = "E[Y(Z = 1)]", color = treatment_color) +
  geom_text(aes(x = label_x_position[3], y = Y_Z_0[3]), label = "E[Y(Z = 0)]", color = control_color) +
  scale_y_continuous("Average potential outcomes") +
  scale_x_continuous("Wave", breaks = 1:3) +
  dd_theme()


grid.arrange(sw_plot, ate_by_wave_plot,nrow = 1)






diag_tab <- reshape_diagnosis(diagnosis) 
diag_tab %>% select(c("Estimator Label", "Bias","Power","Coverage","Mean Estimate","SD Estimate","Mean Se")) %>% kable()

designs <- list(
  a = redesign(design, p_00 = 3/8, p_W1 =  3/8, p_W2 = 1/8, p_W3 = 1/8),
  b = redesign(design, p_00 = 3/8, p_W1 =  2/8, p_W2 = 2/8, p_W3 = 1/8),
  c = redesign(design, p_00 = 3/8, p_W1 =  2/8, p_W2 = 1/8, p_W3 = 2/8),
  d = redesign(design, p_00 = 3/8, p_W1 =  1/8, p_W2 = 3/8, p_W3 = 1/8),
  e = redesign(design, p_00 = 3/8, p_W1 =  1/8, p_W2 = 2/8, p_W3 = 2/8),
  f = redesign(design, p_00 = 3/8, p_W1 =  1/8, p_W2 = 1/8, p_W3 = 3/8)
  )





designs_data <- names(designs) %>% 
  lapply(function(name) designs[[name]] %>% 
           draw_data %>% 
           mutate(design_id = name)) %>% 
  do.call(what = rbind,args = .) %>% 
  arrange(design_id,wave,i) %>% 
  group_by(t,design_id) %>%
  mutate(i = 1:n(), Assignment = ifelse(Z == 1, "Treatment", "Control")) 


diags <- diagnoses %>% 
  get_diagnosands() %>% 
  filter(estimator_label == "2: Weighted, clustered SW") %>% 
  mutate(
    design_name = paste0("Design ", design_label, ": W1 = ",8*p_W1,
                         "; W2 = ", 8*p_W2, 
                         "; W3 = ", 8*p_W3,",\nPower = ",round(power,2)
                         
                         )
  )

designs_data <- left_join(designs_data, diags %>% select(design_name, design_label), by = c("design_id" = "design_label"))

designs_data %>% 
  ggplot(aes(x = t, y = i, fill = Assignment)) +
  geom_tile(color = "white") + 
  scale_fill_manual(values = c(control_color, treatment_color)) +
  dd_theme() + facet_wrap(~ design_name, nrow = 1) + 
  scale_y_continuous("",labels = NULL)


