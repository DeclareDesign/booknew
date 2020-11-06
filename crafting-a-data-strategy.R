# ---
# Crafting a data strategy
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

# design <-
#   declare_population(N = 100) +
#   declare_potential_outcomes(D ~ Z) +
#   declare_potential_outcomes(Ystar ~ D, assignment_variables = D) +
#   declare_sampling(n = 99) +
#   declare_assignment() +
#   declare_measurement(Y = Ystar)

dag <- dagify(Y ~ Ystar + Q + R,
              R ~ S + U,
              Ystar ~ D + U,
              D ~ Z + U,
              Z ~ S)
nodes <-
  tibble(
    name = c("S", "R", "Z", "Q", "D", "Ystar", "Y", "U"),
    label = c("S", "R", "Z", "Q", "D", "Y<sup>*</sup>", "Y", "U"),
    annotation = c(
      "**Sampling**",
      "**Response**",
      "**Treatment assignment**",
      "**Measurement tool**",
      "**Treatment received**",
      "**Latent outcome**",
      "**Observed outcome**",
      "**Unobserved heterogeneity**"
    ),
    x = c(1, 3, 1, 5, 3, 4, 5, 2.5),
    y = c(4, 4, 1, 4, 1, 2.5, 2.5, 2.5),
    nudge_direction = c("N", "N", "S", "N", "S", "N", "S", "S"),
    answer_strategy = "uncontrolled"
  )

endnodes <-
  nodes %>%
  transmute(to = name, xend = x, yend = y)

ggdd_df <-
  dag %>%
  tidy_dagitty() %>%
  select(name, direction, to, circular) %>%
  as_tibble %>%
  mutate(
    data_strategy = case_when(
      name == "D" ~ "unmanipulated",
      name == "Q" ~ "measurement",
      name == "S" ~ "sampling",
      name == "Ystar" ~ "unmanipulated",
      name == "Z" ~ "assignment",
      name == "Y" ~ "unmanipulated",
      name == "U" ~ "unmanipulated",
      name == "R" ~ "unmanipulated"
    ),
    exclusion_restriction = "no"
  ) %>%
  # left_join(design_nodes, by = "name") %>%
  left_join(nodes, by = "name") %>%
  left_join(endnodes, by = "to") %>%
  left_join(nudges_df, by = c("x", "y", "nudge_direction")) %>%
  left_join(aes_df, by = c("data_strategy", "answer_strategy")) %>%
  mutate(shape_y = y + shape_nudge_y)

base_dag_plot %+% ggdd_df

set.seed(348)
gg_df <- fabricate(
  villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
  households = add_level(N = 4,
                         household_num = 1:4 + 1:4 * 0.1),
  individuals = add_level(
    N = 4,
    X = village_num + c(0.25,-0.25, 0.25,-0.25),
    Y = household_num + c(0.25, 0.25,-0.25,-0.25),
    a = simple_rs(N),
    b = complete_rs(N),
    c = strata_rs(strata = villages),
    d = cluster_rs(clusters = households, simple = TRUE),
    e = cluster_rs(clusters = households),
    f = strata_and_cluster_rs(clusters = households, strata = villages),
    g = d * simple_rs(N),
    h = e * strata_rs(strata = households),
    i = f * strata_rs(strata = paste0(households, Y))
  )) %>%
  pivot_longer(cols = letters[1:9], names_to = "procedure", values_to = "sampled") %>%
  mutate(procedure = factor(
    procedure,
    levels = letters[1:9],
    labels = c(
      "Individual Random Sampling (simple)",
      "Individual Random Sampling (complete)",
      "Individual Random Sampling (stratified)",
      "Cluster Random Sampling (simple)",
      "Cluster Random Sampling (complete)",
      "Cluster Random Sampling (stratified)",
      "Multistage Random Sampling (simple)",
      "Multistage Random Sampling (complete)",
      "Multistage Random Sampling (stratified)"
    )
  ),
  sampled = as.factor(sampled),
  unit = case_when(
    str_detect(procedure, "Individual") ~ "Individual",
    str_detect(procedure, "Cluster") ~ "Cluster",
    str_detect(procedure, "Multistage") ~ "Multistage"
  ),
  sampling_type = case_when(
    str_detect(procedure, "simple") ~ "Simple",
    str_detect(procedure, "complete") ~ "Complete",
    str_detect(procedure, "stratified") ~ "Stratified"
  ),
  unit = factor(unit, levels = c("Individual", "Cluster", "Multistage")),
  sampling_type = factor(sampling_type, levels = c("Simple", "Complete", "Stratified"))
  )

cluster_df <- gg_df %>%
  group_by(households, unit, sampling_type) %>%
  summarize(hh_sampled = any(sampled == 1), X = mean(X), Y = mean(Y)) %>%
  filter(hh_sampled == TRUE, unit != "Individual")

ggplot(gg_df, aes(X, Y)) +
  geom_tile(aes(fill = sampled), color = NA, width = 0.46, height = 0.46) +
  geom_tile(data = cluster_df, fill = NA, color = gray(0.6), size = 0.25, width = 1.03, height = 1.03) +
  coord_fixed() +
  facet_grid(unit  ~ sampling_type, switch = "y") +
  dd_theme() +
  scale_fill_manual(values = c(gray(0.95), dd_light_blue)) +
  scale_x_continuous(name = "Stratum (e.g., locality)", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

#gg_df %>% group_by(unit, sampling_type) %>% summarise(n = n(), sampled = sum(sampled == 1))


# convenience
# quota (strata)
# RDS


convenience <-
c(1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 0, 0, 0,
  1, 1, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0)

quota <-
c(1, 1, 1, 1, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 1, 1, 1, 1, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 0, 1, 1, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  0, 0, 0, 1, 1, 0, 0, 0,
  0, 0, 0, 0, 1, 1, 1, 0)

rds <-
c(0, 0, 0, 1, 1, 1, 0, 1, 0, 0,
  0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
  1, 0, 0, 0, 0, 1, 1, 0, 1, 1,
  1, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  1, 1, 1, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
  1, 1, 1, 0)


gg_df <- fabricate(
  villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
  households = add_level(N = 4,
                         household_num = 1:4 + 1:4 * 0.1),
  individuals = add_level(
    N = 4,
    X = village_num + c(-0.25,0.25, -0.25,0.25),
    Y = household_num + c(-0.25, -0.25,0.25,0.25),
    a = convenience,
    b = quota,
    c = rds
  )) %>%
  pivot_longer(cols = letters[1:3], names_to = "procedure", values_to = "sampled") %>%
  mutate(procedure = factor(
    procedure,
    levels = letters[1:3],
    labels = c(
      "Convenience",
      "Quota",
      "Respondent-driven"
    )
  ),
  sampled = as.factor(sampled)
  )


ggplot(gg_df, aes(X, Y)) +
  geom_tile(aes(fill = sampled), color = NA, width = 0.46, height = 0.46) +
  # geom_text(aes(label = individuals)) +
  coord_fixed() +
  facet_wrap(~procedure) +
  dd_theme() +
  scale_fill_manual(values = c(gray(0.95), dd_light_blue)) +
  scale_x_continuous(name = "Stratum (e.g., locality)", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

points_df <- data.frame(
  X = c(.85, .85, 1.35, 1.95, 1.95, 3.05, 3.05, 3.55, 4.15, 4.65),
  Y = c(.85, 4.15, 1.35, 2.45, 3.55, 2.45, 3.55, 4.15, 1.35, 4.65))
points_df$id <- 1:nrow(points_df)

# lm(Y ~ X, points_df) %>% hatvalues() %>% sort() %>% tail(2)

gg_df <-
  fabricate(
  villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
  households = add_level(N = 4,
                         household_num = 1:4 + 1:4 * 0.1),
  individuals = add_level(
    N = 4,
    X = village_num + c(-0.25,0.25, -0.25,0.25),
    Y = household_num + c(-0.25, -0.25,0.25,0.25)
  )) %>%
  merge(points_df, by = c("X", "Y"), all.x = TRUE) %>%
  mutate(point_X = ifelse(is.na(id), NA, X),
         point_Y = ifelse(is.na(id), NA, Y),
         a = ifelse(id %in% c(4,7), 1, 0),
         b = ifelse(id %in% c(1,10), 1, 0),
         c = ifelse(id %in% c(9,10), 1, 0),
         d = ifelse(id %in% c(2,9), 1, 0),
         e = ifelse(id %in% c(2,10), 1, 0),
         f = ifelse(id %in% c(1,2), 1, 0),
         g = ifelse(id %in% c(3,9), 1, 0)
         ) %>%
  pivot_longer(cols = letters[1:7], names_to = "procedure", values_to = "sampled") %>%
  mutate(procedure = factor(
    procedure,
    levels = letters[1:7],
    labels = c(
      "Typical",
      "Diverse",
      "Extreme",
      "Deviant",
      "Influential",
      "Most Similar",
      "Most Different"
    )
  ),
  sampled = as.factor(sampled)
  )


ggplot(gg_df, aes(point_X, point_Y)) +
  geom_tile(aes(X, Y), fill = gray(0.95), color = NA, width = 0.46, height = 0.46) +
  geom_tile(aes(fill = sampled), color = NA, width = 0.46, height = 0.46) +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = .5) +
  coord_fixed() +
  facet_wrap(~procedure, nrow = 2) +
  dd_theme() +
  scale_fill_manual(values = c(gray(0.6), dd_light_blue)) +
  scale_x_continuous(name = "X", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  scale_y_continuous(name = "Y") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()
        )

set.seed(343)
gg_df <- fabricate(
  villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
  households = add_level(N = 4,
                         household_num = 1:4 + 1:4 * 0.1),
  individuals = add_level(
    N = 4,
    X = village_num + c(0.25,-0.25, 0.25,-0.25),
    Y = household_num + c(0.25, 0.25,-0.25,-0.25),
    a = simple_ra(N),
    b = complete_ra(N),
    c = block_ra(blocks = villages),
    d = cluster_ra(clusters = households, simple = TRUE),
    e = cluster_ra(clusters = households),
    f = block_and_cluster_ra(clusters = households, blocks = villages),
    g = d * simple_ra(N),
    h = e * block_ra(blocks = households),
    i = f * block_ra(blocks = paste0(households, Y))
  )) %>%
  pivot_longer(cols = letters[1:9], names_to = "procedure", values_to = "sampled") %>%
  mutate(procedure = factor(
    procedure,
    levels = letters[1:9],
    labels = c(
      "Individual Random Assignment (simple)",
      "Individual Random Assignment (complete)",
      "Individual Random Assignment (blocked)",
      "Cluster Random Assignment (simple)",
      "Cluster Random Assignment (complete)",
      "Cluster Random Assignment (blocked)",
      "Saturation Design (simple)",
      "Saturation Design (complete)",
      "Saturation Design (blocked)"
    )
  ),
  unit = case_when(
    str_detect(procedure, "Individual") ~ "Individual",
    str_detect(procedure, "Cluster") ~ "Cluster",
    str_detect(procedure, "Saturation") ~ "Saturation"
  ),
  sampling_type = case_when(
    str_detect(procedure, "simple") ~ "Simple",
    str_detect(procedure, "complete") ~ "Complete",
    str_detect(procedure, "blocked") ~ "Blocked"
  ),
  unit = factor(unit, levels = c("Individual", "Cluster", "Saturation")),
  sampling_type = factor(sampling_type, levels = c("Simple", "Complete", "Blocked"))
  )

gg_df <-
  gg_df %>%
  group_by(households, unit, sampling_type) %>%
  mutate(treated_cluster = any(sampled == 1),
         condition = case_when(
           sampled == 1 & unit == "Individual" ~ "T",
           sampled == 0 & unit == "Individual" ~ "C",
           sampled == 1 & unit == "Cluster" ~ "T",
           sampled == 0 & unit == "Cluster" ~ "C",
           sampled == 1 & unit == "Saturation" ~ "T",
           sampled == 0 & unit == "Saturation" & treated_cluster  ~ "Sp",
           sampled == 0 & unit == "Saturation" & !treated_cluster  ~ "C"
         ))

cluster_df <- gg_df %>%
  group_by(households, unit, sampling_type) %>%
  summarize(hh_sampled = any(sampled == 1), X = mean(X), Y = mean(Y)) %>%
  filter(hh_sampled == TRUE, unit != "Individual")

ggplot(gg_df, aes(X, Y)) +
  geom_tile(aes(fill = condition), color = NA, width = 0.46, height = 0.46) +
  geom_tile(data = cluster_df, fill = NA, color = gray(0.6), size = 0.25, width = 1.03, height = 1.03) +
  geom_text(aes(label = condition), size = 2, color = "white") +
  coord_fixed() +
  facet_grid(unit  ~ sampling_type, switch = "y") +
  dd_theme() +
  scale_fill_manual(values = c(dd_light_gray, "#72B4F366", dd_light_blue)) +
  scale_x_continuous(name = "Block (e.g., locality)", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

set.seed(343)
gg_df <-
  fabricate(
  villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
  households = add_level(N = 4, household_num = 1:4 + 1:4 * 0.1),
  individuals = add_level(
    N = 4,
    X = village_num + c(0.25, -0.25, 0.25, -0.25),
    Y = household_num + c(0.25, 0.25, -0.25, -0.25),
    a = block_ra(households, conditions = c("C", "T1", "T2")),
    b = block_ra(households, conditions = c("C", "T1", "T2", "T1\nT2")),
    c = if_else(b == "T1\nT2", "T3", as.character(b))
    # c = block_ra(households, conditions = c("C", "T1", "T2", "T3"))
  )
) %>%
  pivot_longer(cols = letters[1:3],
               names_to = "procedure",
               values_to = "assignment") %>%
  mutate(procedure = factor(
    procedure,
    levels = letters[1:3],
    labels = c("Three-arm", "Factorial", "Four-arm")
  ))

sort(unique(gg_df$assignment))

gg_df %>%
  ggplot(aes(X, Y)) +
  geom_tile(aes(fill = assignment), width = 0.46, height = 0.46) +
  geom_text(aes(label = assignment), size = 2, color = "white") +
  coord_fixed() +
  facet_grid(~ procedure, switch = "y") +
  dd_theme() +
  scale_fill_manual(values = c(dd_light_gray, dd_pink, dd_purple , dd_light_blue, dd_orange)) +
  # scale_fill_manual(values = c("#72B4F333", dd_orange, dd_light_blue)) +
  # scale_x_continuous(name = "Block (e.g., locality)", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

set.seed(343)
gg_df <- fabricate(
  villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
  households = add_level(N = 4,
                         household_num = 1:4 + 1:4 * 0.1),
  individuals = add_level(
    N = 4,
    X = village_num + c(0.25,-0.25, 0.25,-0.25),
    Y = household_num + c(0.25, 0.25,-0.25,-0.25),
    assignment = block_ra(blocks = households, conditions = c("P1", "P2", "P3")),
    assignment1 = assignment == "P1",
    assignment2 = assignment %in% c("P1", "P2"),
    assignment3 = 1
  )) %>%
  select(-assignment) %>%
  pivot_longer(cols = contains("assignment"), names_to = "procedure", values_to = "assignment") %>%
  mutate(procedure = factor(procedure, levels = paste0("assignment", 1:3), labels = c("Period 1", "Period 2", "Period 3")),
         assignment = as.factor(assignment))

gg_df %>%
  ggplot(aes(X, Y)) +
  geom_tile(aes(fill = assignment), color = NA, width = 0.46, height = 0.46) +
  # geom_text(aes(label = assignment), size = 2.5) +
  coord_fixed() +
  facet_grid(~ procedure, switch = "y") +
  dd_theme() +
  scale_fill_manual(values = c(dd_light_gray, dd_light_blue)) +
  # scale_x_continuous(name = "Block (e.g., locality)", breaks = 1:4 + 1:4 * 0.1, labels = LETTERS[1:4]) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())

# design <-
#   declare_population(N = 100) +
#   declare_potential_outcomes(D ~ Z) +
#   declare_potential_outcomes(Ystar ~ D, assignment_variables = D) +
#   declare_sampling(n = 99) +
#   declare_assignment() +
#   declare_measurement(Y = Ystar)

dag <- dagify(Y ~ Ystar + Q + R,
              R ~ S + U + Z,
              Ystar ~ D + U + S + Z + Q,
              D ~ Z + U,
              Z ~ S)
nodes <-
  tibble(
    name = c("S", "R", "Z", "Q", "D", "Ystar", "Y", "U"),
    label = c("S", "R", "Z", "Q", "D", "Y<sup>*</sup>", "Y", "U"),
    annotation = c(
      "**Sampling**",
      "**Response**",
      "**Treatment assignment**",
      "**Measurement tool**",
      "**Treatment received**",
      "**Latent outcome**",
      "**Observed outcome**",
      "**Unobserved heterogeneity**"
    ),
    x = c(1, 3, 1, 5, 3, 4, 5, 2.5),
    y = c(4, 4, 1, 4, 1, 2.5, 2.5, 2.5),
    nudge_direction = c("N", "N", "S", "N", "S", "N", "S", "S"),
    answer_strategy = "uncontrolled"
  )

endnodes <-
  nodes %>%
  transmute(to = name, xend = x, yend = y)

ggdd_df <-
  dag %>%
  tidy_dagitty() %>%
  select(name, direction, to, circular) %>%
  as_tibble %>%
  mutate(
    data_strategy = case_when(
      name == "D" ~ "unmanipulated",
      name == "Q" ~ "measurement",
      name == "S" ~ "sampling",
      name == "Ystar" ~ "unmanipulated",
      name == "Z" ~ "assignment",
      name == "Y" ~ "unmanipulated",
      name == "U" ~ "unmanipulated",
      name == "R" ~ "unmanipulated"
    ),
    exclusion_restriction = case_when(
      name %in% c("S", "Z", "Q") & to == "Ystar" ~ "yes",
      name %in% c("Z") & to == "R" ~ "yes",
      TRUE ~ "no"
    )
  ) %>%
  # left_join(design_nodes, by = "name") %>%
  left_join(nodes, by = "name") %>%
  left_join(endnodes, by = "to") %>%
  left_join(nudges_df, by = c("x", "y", "nudge_direction")) %>%
  left_join(aes_df, by = c("data_strategy", "answer_strategy")) %>%
  mutate(shape_y = y + shape_nudge_y)

base_dag_plot %+% ggdd_df
