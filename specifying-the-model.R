# ---
# Specifying the model
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(dagitty)
library(dddag)
library(ggraph)

dag <- dagify(Y ~ Z + U)

nodes <-
  tibble(
    name = c("Z", "U", "Y"),
    label = c("Z", "U", "Y"),
    annotation = c(
      "**Treatment assignment**",
      "**Unknown heterogeneity**",
      "**Outcome**"
    ),
    x = c(1, 5, 5),
    y = c(1.5, 3.5,  1.5), 
    nudge_direction = c("S", "N", "S"),
    data_strategy = c("assignment", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df

# assignment
dag <- dagify(Y ~ D,
              D ~ Z)

nodes <-
  tibble(
    name = c("Z", "D", "Y"),
    label = c("Z", "D", "Y"),
    annotation = c("**Random assignment**",
                   "**Treatment**",
                   "**Observed outcome**"),
    x = c(1, 3, 5),
    y = c(2, 2, 2), 
    nudge_direction = "N",
    data_strategy = c("assignment", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

gg_assignment <- base_dag_plot %+% ggdd_df

# sampling
dag <- dagify(Y ~ S)

nodes <-
  tibble(
    name = c("S", "Y"),
    label = c("S", "Y"),
    annotation = c("**Random sampling**",
                   "**Observed outcome**"),
    x = c(1, 5),
    y = c(2, 2), 
    nudge_direction = "N",
    data_strategy = c("sampling", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

gg_sampling <- base_dag_plot %+% ggdd_df


# measurement
dag <- dagify(Y ~ Q + Ystar)

nodes <-
  tibble(
    name = c("Ystar", "Y", "Q"),
    label = c("Y^*", "Y", "Q"),
    annotation = c("**Latent outcome**",
                   "**Observed outcome**",
                   "**Measurement tool**"),
    x = c(1, 3, 5),
    y = c(2, 2, 2), 
    nudge_direction = "N",
    data_strategy = c("unmanipulated", "unmanipulated", "measurement"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

gg_measurement <- base_dag_plot %+% ggdd_df

# adjustment 
dag <- dagify(Y ~ Z + X)

nodes <-
  tibble(
    name = c("Z", "Y", "X"),
    label = c("Z", "Y", "X"),
    annotation = c("**Treatment assignment**",
                   "**Observed outcome**",
                   "**Pretreatment covariate**"),
    x = c(1, 3, 5),
    y = c(2, 2, 2), 
    nudge_direction = "N",
    data_strategy = c("assignment", "unmanipulated", "unmanipulated"),
    answer_strategy = c("uncontrolled", "uncontrolled", "controlled")
  )

ggdd_df <- make_dag_df(dag, nodes)

gg_adjustment <- base_dag_plot %+% ggdd_df

(gg_assignment + gg_sampling) / (gg_measurement + gg_adjustment)

model <-
  declare_model(N = 100, 
                U = rnorm(N), 
                tau = rnorm(N, mean = 1, sd = 0.1), 
                Z = rbinom(N, 1, prob = 0.5),
                potential_outcomes(Y ~ tau * Z + U))

draw_data(model + NULL) %>% 
  head(5) %>% kable(caption = "Data from a simple model", digits = 3, booktabs = TRUE)


M <-
  declare_model(
    N = 100,
    U = rbinom(N, size = 1, prob = 0.25),
    X1 = rbinom(N, size = 1, prob = 0.25),
    X2 = rbinom(N, size = 1, prob = 0.25),
    potential_outcomes(D ~ Z * X1),
    potential_outcomes(M ~ D, conditions = list(D = c(0, 1))),
    potential_outcomes(K ~ D * U, conditions = list(D = c(0, 1))),
    potential_outcomes(Y ~ X2 + X1 + M + U, 
                       conditions = list(M = c(0, 1)))
  ) +
  declare_assignment(Z = complete_ra(N, prob = 0.5), legacy = FALSE) +
  declare_measurement(
    D = reveal_outcomes(D ~ Z),
    M = reveal_outcomes(M ~ D),
    K = reveal_outcomes(K ~ D),
    Y = reveal_outcomes(Y ~ M)
  )
# draw_data(M)


dag <- dagify(
  Y ~ X2 + X1 + M + U,
  M ~ D,
  K ~ D + U,
  D ~ Z + X1
)

# x = c(M = 3, U = 4, X1 = 3, X2 = 2, X3 = 4, Z = 1, Y = 4, K = 3)
# y = c(M = 1, U = 0, X1 = 2, X2 = 1, X3 = 2, Z = 1, Y = 1, K = 0)

nodes <-
  tibble(
    name = c("Z", "X1", "U", "Y", "X2", "K", "M", "D"),
    label = c("Z", "X1", "U", "Y", "X2", "K", "M", "D"),
    annotation = c(
      "**Instrument**",
      "**Confounder**",
      "**Unknown heterogeneity**",
      "**Outcome**",
      "**Moderator**",
      "**Collider**",
      "**Mediator**",
      "**Explanatory variable**"
    ),
    x = c(1, 3, 5, 5, 5, 3, 3, 2),
    y = c(2.5, 4, 1, 2.5, 4, 1, 2.5, 2.5), 
    nudge_direction = c("N", "N", "S", "E", "N", "S", "N", "S"),
    data_strategy = c("assignment", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated", "unmanipulated"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes)

base_dag_plot %+% ggdd_df

tau_X0 <- 0.5
tau_X1 <- 1

model <-
  declare_model(
    N = 100, 
    U = rnorm(N), 
    X = rbinom(N, 1, .5),
    potential_outcomes(
      Y ~ (X == 0) * Z * tau_X0 + (X == 1) * Z * tau_X1 + U
    )
  )

dag1 <- dagitty("dag{
                D -> Y
                }")

points_df_small <-
  tibble(name = c("D", "X", "Y", "U"),
         x = c(0, 1, 1, 0),
         y = c(0, 1, 0, 1))
ends_df_small <-
  tibble(to = c("D", "X", "Y", "U"),
         xend = c(0, 1, 1, 0),
         yend = c(0, 1, 0, 1))

gg_df_small <-
  list(dag1) %>%
  map_df( ~ as_tibble(tidy_dagitty(.)), .id = "dag") %>%
  select(-x, -y, -xend, -yend) %>%
  left_join(points_df_small) %>%
  left_join(ends_df_small)

ggplot(gg_df_small %>% filter(name %in% c("D", "Y")), aes(x, y)) +
  geom_text(data = points_df_small %>% filter(name %in% c("D", "Y")), aes(label = name)) +
  geom_dag_edges(aes(xend = xend, yend = yend)) +
  coord_fixed(ylim = c(-0.1, 0.1), xlim = c(-.1, 1.1)) +
  theme_void()

possible_models <-
  expand.grid(
    XU = c("X<-U", "none"),
    YU = c("Y<-U", "none"),
    YX = c("Y->X", "Y<-X", "none"),
    DU = c("D<-U", "none"),
    DX = c("D->X", "D<-X", "none"),
    DY = c("D->Y", "D<-Y", "none"),
    stringsAsFactors = FALSE
  )

possible_models <-
  possible_models %>%
  rowwise() %>%
  mutate(
    var = str_c(XU, YU, YX, DU, DX, DY, sep = ";"),
    var = str_remove_all(var, "none;"),
    var = str_remove_all(var, "none"),
    dag = list(dagitty(paste0(
      "dag{", var, "; X; D; Y; U}"
    ))),
    acyclic = isAcyclic(dag),
    consistent_with_ignorability = !(DU == "D<-U" & YU == "Y<-U"),
    id_if_adjusted = isAdjustmentSet(dag, "X", exposure = "D", outcome = "Y"),
    id_if_unadjusted = isAdjustmentSet(dag, NULL, exposure = "D", outcome = "Y"),
    id_adjustment_fac = as.factor(
      case_when(
        id_if_unadjusted &
          id_if_adjusted ~ "Yes, regardless of conditioning",
        id_if_unadjusted &
          !id_if_adjusted ~ "Only when NOT conditioning on X",!id_if_unadjusted &
          id_if_adjusted ~ "Only when conditioning on X",!id_if_unadjusted &
          !id_if_adjusted ~ "No, regardless of conditioning"
      )
    ),
    consistent_with_RA = DU != "D<-U" &
      DX != "D<-X" & DY != "D<-Y",
    consistent_with_X_pretreatment =
      DX != "D->X" &
      YX != "Y->X" &
      !(DU == "D->U" & XU == "X<-U") &
      !(YU == "Y->U" & XU == "X<-U")
  ) 

nested_data <-
  possible_models %>%
  filter(var != "") %>%
  mutate(dag_data = list(as_tibble(tidy_dagitty(dag))))

points_df <-
  tibble(name = as.factor(c("D", "X", "Y", "U")),
         x = c(1, 2, 2, 1),
         y = c(1, 2, 1, 2))
ends <-
  points_df %>%
  rename(to = name,
         xend = x,
         yend = y)

fix_no_edges <-
  tibble(
    var = "no edges",
    acyclic = TRUE,
    consistent_with_RA = TRUE,
    consistent_with_X_pretreatment = TRUE,
    XU = "none",
    YU = "none",
    YX = "none",
    DU = "none",
    DX = "none",
    DY = "none",
    name = "X",
    id_adjustment_fac = "Yes, regardless of conditioning"
  )

gg_df <-
  nested_data %>%
  unnest(cols = dag_data) %>%
  select(-x, -y, -xend, -yend) %>%
  left_join(points_df) %>%
  left_join(ends) %>%
  bind_rows(fix_no_edges) %>%
  mutate(
    XU_fac = factor(
      XU,
      levels =  c("X<-U", "X->U", "none"),
      labels =  c("X %<-% U", "X %->% U", "X~~~U")
    ),
    YU_fac = factor(
      YU,
      levels =  c("Y<-U", "Y->U", "none"),
      labels =  c("Y %<-% U", "Y %->% U", "Y~~~U")
    ),
    YX_fac = factor(
      YX,
      levels =  c("Y<-X", "Y->X", "none"),
      labels =  c("Y %<-% X", "Y %->% X", "Y~~~X")
    ),
    DU_fac = factor(
      DU,
      levels =  c("D<-U", "D->U", "none"),
      labels =  c("D %<-% U", "D %->% U", "D~~~U")
    ),
    DX_fac = factor(
      DX,
      levels =  c("D<-X", "D->X", "none"),
      labels =  c("D %<-% X", "D %->% X", "D~~~X")
    ),
    DY_fac = factor(
      DY,
      levels =  c("D<-Y", "D->Y", "none"),
      labels =  c("D %<-% Y", "D %->% Y", "D~~~Y")
    )
  ) %>%
  arrange(XU_fac, YU_fac, YX_fac, DU_fac, DX_fac, DY_fac)

gg_df <-
  gg_df %>%
  mutate(
    var_fac = factor(var, levels = c(possible_models$var, "no edges")),
    DY_DX = as.factor(paste0(DY, " ", DX)),
    U_relationship = paste0(XU, " ", YU, " ", DU),
    U_relationship_fac =
      factor(
        U_relationship,
        levels = c(
          'X<-U Y<-U D<-U',
          'none Y<-U D<-U',
          'X<-U none D<-U',
          'X<-U Y<-U none',
          'none none D<-U',
          'none Y<-U none',
          'X<-U none none',
          'none none none'
        ),
        labels = c(
          "U affects: D, X, Y",
          "U affects: D, Y",
          "U affects: D, X",
          "U affects: X, Y",
          "U affects: D",
          "U affects: Y",
          "U affects: X",
          "U affects: none"
        )
      ),
    ruled_out_by = as.factor(
      case_when(
        !acyclic ~ "Acyclicity",!consistent_with_RA ~ "Random assignment",!consistent_with_X_pretreatment ~ "Measuring X before treatment",
        TRUE ~ NA_character_
      )
    )
  )

gg_df <-
  gg_df %>%
  mutate(tile_fac = as.factor(if_else(
    !acyclic,
    "Ruled out by acyclicity",
    "Possible"
  ))
  )

fill_scale <- c(
  `Ruled out by acyclicity` = dd_light_gray,
  `Possible` = "transparent"
)

subplot_function <- function(data) {
  dag_df <-
    data %>%
    group_by(var, DX_fac, YX_fac, tile_fac) %>%
    summarize(x = 1.5, y = 1.5, n = n(), .groups = "drop")
  
  g <-
    ggplot(data, aes(x, y)) +
    geom_tile(data = dag_df, aes(fill = tile_fac), height = 1.75, width = 1.75, alpha = 0.5) +
    geom_text(data = points_df, aes(label = name), size = 5) +
    geom_dag_edges(aes(xend = xend, yend = yend),
                   edge_width = 0.4,
                   arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed")) +
    scale_fill_manual("Ruled out by acyclicity?", values = fill_scale, drop = FALSE, 
                      guide = guide_legend(nrow = 2, override.aes = list(color = c(rep("white", 1), gray(0.4))))) +
    facet_grid(YX_fac ~ DX_fac, switch = "both", labeller = label_parsed) +
    coord_fixed() +
    theme_void() +
    theme(plot.title = element_text(size = 30, hjust = 0.5),
          plot.subtitle = element_text(size = 30, hjust = 0.5),
          plot.margin = margin(10, 10, 25, 10, unit = "pt")) +
    labs(subtitle = parse(text = as.character(unique(data$DY_fac))))
  if(as.character(unique(data$DY_fac)) == "D %->% Y"){
    g <- g + labs(title = as.character(unique(data$U_relationship_fac)))
  }
  g
}

my_fun <- function(data){
  data %>%
    split(.$DY_fac) %>%
    map(~subplot_function(.)) %>%
    wrap_plots(nrow = 1)
}

gg <- gg_df %>%
  split(.$U_relationship_fac) %>%
  map(my_fun)

wgg <- wrap_plots(gg, ncol = 2, byrow = FALSE) + plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 35),
    legend.text = element_text(size = 35),
    legend.key.size = unit(35, "mm"),
    legend.margin = margin(t = 40, r = 0, b = 0, l = 0, unit = "mm"),
    strip.text = element_text(size = 30)
  )

wgg

possible_models %>% 
  ungroup %>% 
  sample_n(size = n(), replace = FALSE) %>% 
  mutate(theory_A = DY == "D->Y",
         theory_B = DY == "D Y") %>% 
  select(var, theory_A, theory_B) %>% 
  head() %>% 
  kable(caption = "DAGs coded by consistency with two theories.")

## model <-
##   declare_model(
##     N = 100,
##     U = rnorm(N),
##     D = rbinom(N, 1, prob = 0.5),
##     X = runif(N),
##     potential_outcomes(Y ~ DY * Z + YU * U + YX * X)
##   )
## 
## designs <- redesign(model, DY = c(0, 1), YU = c(0, 1), YX = c(0, 1))
## 
## diagnose_design(designs, sims = 5)
