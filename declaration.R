# ---
# Declaration
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

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
g1

design <-
  declare_population(N = 100,
                     X = rbinom(N, 1, 0.3),
                     U = rnorm(N)) +
  declare_potential_outcomes(Y ~ Z + X + U) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_sampling(strata = X, prob = 1) + 
  declare_assignment(blocks = X, block_prob = c(0.1, 0.5)) +
  declare_estimator(Y ~ Z, estimand = "ATE", label = "Naive DIM") +
  declare_estimator(Y ~ Z,
                    blocks = X,
                    estimand = "ATE",
                    label = "Blocked DIM")

dag <- dagify(
  Y ~ Z + X + U,
  Z ~ X + S,
  S ~ X
)

nodes <-
  tibble(
    name = c("Y", "S", "Z", "U", "X"),
    label = c("Y", "S", "Z", "U", "X"),
    annotation = c(
      "**Outcome**<br>",
      "**Sampling**",
      "**Random assignment**<br>",
      "**Unknown heterogeneity**",
      "**villages**<br>Stratification, cluster assignment"),
    x = c(5, 1, 3, 5, 1),
    y = c(2.5, 2.5, 2.5, 4, 4), 
    nudge_direction = c("S", "S", "S", "N", "N"),
    data_strategy = c("unmanipulated", "sampling", "assignment", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df + coord_fixed(ylim = c(2.05, 4.6), xlim = c(0.25 - epsilon, 5.75 + epsilon)) 

model <-
  declare_population(
    districts = add_level(
      N = 2,
      district_name = c("Haripur", "Abbottabad"),
      N_villages = c(311, 359)
    ),
    villages = add_level(N = N_villages),
    citizens = add_level(N = 6500,
                         U = runif(N))
  ) +
  declare_potential_outcomes(
    Y_Z_control = U,
    Y_Z_neutral = U + 0.02,
    Y_Z_personal = U + 0.04,
    Y_Z_social = U + 0.05
  )

inquiry <- 
  declare_estimand(
    ATE = mean((Y_Z_neutral + Y_Z_personal + Y_Z_social) / 3 -
                 Y_Z_control),
    ATE_social_vs_personal = mean(Y_Z_social - Y_Z_personal)
  ) 

data_strategy <- 
  declare_assignment(
    m_each = c(478, 48, 72, 72),
    clusters = villages,
    conditions = c("control", "neutral", "social", "personal")
  ) + 
  reveal_outcomes(Y, Z) +
  declare_step(any_treatment = if_else(Z == "control", 0, 1),
               handler = fabricate) +
  declare_sampling(strata = villages, 
                   n_unit = if_else(Z == "control", 0, 48), 
                   drop_nonsampled = FALSE) 

answer_strategy <- 
  declare_estimator(
    Y ~ any_treatment,
    clusters = villages,
    model = lm_robust,
    se_type = "CR0",
    subset = (Z == "control" | S == 1),
    estimand = "ATE",
    label = "ATE"
  ) +
  declare_estimator(
    Y ~ Z,
    clusters = villages,
    model = lm_robust,
    se_type = "CR0",
    subset = (S == 1 & Z != "neutral"),
    estimand = "ATE_social_vs_personal",
    label = "ATE_social_vs_personal"
  )

design <- 
  model + inquiry + data_strategy + answer_strategy





## dat <- draw_data(design)

dat_head %>% 
  select(-villages, -N_villages, -citizens, -districts, -Z_cond_prob, -S_inclusion_prob) %>% 
  kable(digits = 3, caption = "Five rows of simulated data from the Gulzar-Khan design.", booktabs = TRUE)

## estimates <- run_design(design)

estimates$estimates_df %>% left_join(estimates$estimands_df) %>% 
select(estimator_label, estimate, std.error, estimand) %>% 
kable(digits = 3, caption = "Simulated estimates and estimands from one run of the Gulzar-Khan design.", booktabs = TRUE)

summary_df %>% 
  kable(caption = "Count of units by treatment assignment and sampling status.", booktabs = TRUE)
