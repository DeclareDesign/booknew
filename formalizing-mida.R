# ---
# Formalizing MIDA
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
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
    name = c("M", "I", "D", "A", "m", "am", "aw", "d", "ad", "w", "W"),
    label = c("M", "I", "D", "A", "m", "a<sup>m</sup>", "a<sup>w</sup>", "d", "a<sup>d</sup>", "w", "W"),
    long_label = c("Theoretical<br>causal model", "Inquiry", "Data<br>strategy", "Answer<br>strategy", "Model<br>draw", "Theoretical<br>answer", "True answer", "Realized<br>data", "Empirical<br>answer", "Real<br>world", "True<br>causal model"),
    lbl_direction = c("N", "N", "N", "N", "S", "S", "S", "S", "S", "S", "N"),
    x = c(1, 2, 5.5, 6.5, 1, 2, 4.25, 5.5, 6.5, 3.25, 3.25),
    y = c(3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 3)
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
    xmin = c(.4, 4.9),
    xmax = c(2.6, 7.1),
    ymin = c(1.15, 1.15),
    ymax = c(3.85, 3.85)
  )

g <-
  ggplot(data = filter(gg_df, !arced1 & !arced2), aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_point(color = gray(.1), fill = NA, size = 15, stroke = 0.5, pch = 1) +
  geom_dag_edges(edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced1), curvature = -0.28, edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced2), curvature = .7, edge_width = 0.35) +
  geom_richtext(color = "black",
                parse = TRUE,
                aes(label = label),
                fill = NA,
                label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt"),
                size = 4) +
  geom_richtext(
    aes(y = y + if_else(lbl_direction == "N", 0.4, -0.4),
        vjust = if_else(lbl_direction == "N", "bottom", "top"),
        label = long_label),
    color = gray(0.5),
    parse = TRUE,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 4) +
  coord_fixed(ylim = c(1.15, 4)) + 
  geom_rect(data = rect_df, aes(x = NULL, y = NULL, 
                                xend = NULL, yend = NULL,
                                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.15) +
  annotate("text", x = 1.5, y = 4.05, label = "Theory") +
  annotate("text", x = 6, y = 4.05, label = "Empirics") +
  annotate("text", x = 3.75, y = 4.05, label = "Reality") +
  # annotate("text", x = 3, y = 1.6, label = "Truth") +
  theme_dag()
g

dag <-
  dagify(aw ~ w + I,
         w ~ M,
         d ~ D + w,
         ad ~ A + d)

dag_base <- tidy_dagitty(dag) %>%
  select(name, direction, to, circular) %>%
  as_tibble

nodes_df <- nodes_df[-c(7, 10, 11),]
nodes_df[1, 3] <- c("Possible worlds") 
nodes_df[5, 1] <- c("w") 
nodes_df[5, 2] <- c("w") 
nodes_df[5, 3] <- c("World") 
nodes_df[6, 1] <- c("aw") 
nodes_df[6, 2] <- c("a<sup>w</sup>") 
nodes_df[6, 3] <- c("True answer") 

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
    xmin = c(.4, 4.9),
    xmax = c(2.6, 7.1),
    ymin = c(1.15, 1.15),
    ymax = c(3.85, 3.85)
  )

g <-
  ggplot(data = filter(gg_df, !arced1), aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_point(color = gray(.1), fill = NA, size = 15, stroke = 0.5, pch = 1) +
  geom_dag_edges(edge_width = 0.35) +
  geom_dag_edges_arc(data = filter(gg_df, arced1), curvature = -0.28, edge_width = 0.35) +
#  geom_dag_edges_arc(data = filter(gg_df, arced2), curvature = .7, edge_width = 0.35) +
  geom_richtext(color = "black",
                parse = TRUE,
                aes(label = label),
                fill = NA,
                label.color = NA,
                label.padding = grid::unit(rep(0, 4), "pt"),
                size = 4) +
  geom_richtext(
    aes(y = y + if_else(lbl_direction == "N", 0.4, -0.4),
        vjust = if_else(lbl_direction == "N", "bottom", "top"),
        label = long_label),
    color = gray(0.5),
    parse = TRUE,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 4) +
  coord_fixed(ylim = c(1.15, 4)) + 
  geom_rect(data = rect_df, aes(x = NULL, y = NULL, 
                                xend = NULL, yend = NULL,
                                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.15) +
  annotate("text", x = 1.5, y = 4.05, label = "Theory") +
  annotate("text", x = 6, y = 4.05, label = "Empirics") +
  # annotate("text", x = 3, y = 1.6, label = "Truth") +
  theme_dag()
g

gulzar_khan_design <- 
  
  declare_population(
    # study is conducted in two districts in Pakistan, with 311 and 359 villages in them
    districts = add_level(N = 2, name = c("Haripur", "Abbottabad"), N_villages = c(311, 359)),
    
    # villages nested within districts
    villages = add_level(N = N_villages),
    
    # avg. 6500 citizens per village
    citizens = add_level(N = 6500)
  ) +
  
  # main outcome is whether a citizen filed papers to run for office
  # we define potential outcomes in response to being assigned to a social, personal, or neutral appeal to run
  declare_potential_outcomes(
    filed_papers ~ rbinom(N, 1, prob = 0.05 + 0.05 * (Z_appeal == "social") + 0.01 * (Z_appeal == "personal")),
    assignment_variable = Z_appeal, conditions = c("neutral", "social", "personal")
  ) + 
  
  # inquiry is the difference in rates of filing papers between the social and personal appeal conditions
  declare_estimand(ATE = mean(filed_papers_Z_appeal_social - filed_papers_Z_appeal_personal)) + 
  
  # sample 192 villages
  declare_sampling(clusters = villages, n = 192, sampling_variable = "S_villages") + 
  
  # sample 48 citizens in each village via random walk
  declare_sampling(strata = villages, n = 48, sampling_variable = "S_citizens") + 
  
  # assign villages to three arms with equal probabilities for three types of appeals to run for office
  declare_assignment(
    m_each = c(48, 72, 72),
    clusters = villages,
    conditions = c("neutral", "social", "personal"),
    assignment_variable = Z_appeal
  ) + 
  
  # recode treatment assignment for analysis into indicators for the two conditions of interest
  declare_step(
    Z_social_village = if_else(Z_appeal == "social", 1, 0),
    Z_personal_village = if_else(Z_appeal == "personal", 1, 0),
    handler = mutate
  ) +
  
  # 1. run a linear regression with condition indicators
  # 2. calculate the difference in effects between people in villages assigned to social appeals compared
  #    to those assigned to personal appeals
  # 3. calculate robust standard errors clustered on village
  declare_estimator(
    filed_papers ~ Z_social_village + Z_personal_village, 
    linear_hypothesis = "Z_social_village - Z_personal_village = 0",
    term = "Z_social_village - Z_personal_village = 0",
    clusters = villages,
    model = lh_robust
  )

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
    answer_strategy = "uncontrolled"
  )
ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df
