# ---
# Choosing an answer strategy
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

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

dag1 <- dagitty("dag{
                D <- X -> Y; D -> Y
                }")

dag2 <- dagitty("dag{
                D -> X <- Y ; D -> Y
                }")

dag3 <- dagitty("dag{
                D -> X -> Y; D -> Y
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
  list(dag1, dag2, dag3) %>%
  map_df( ~ as_tibble(tidy_dagitty(.)), .id = "dag") %>%
  mutate(dag = factor(
    dag,
    levels = 1:3,
    labels = c("X is a confounder", "X is a collider", "X is a mediator")
  )) %>%
  select(-x, -y, -xend, -yend) %>%
  left_join(points_df_small) %>%
  left_join(ends_df_small)

ggplot(gg_df_small, aes(x, y)) +
  geom_text(data = points_df_small, aes(label = name)) +
  geom_dag_edges(aes(xend = xend, yend = yend)) +
  coord_fixed(ylim = c(0, 1.1), xlim = c(-.1, 1.1)) +
  facet_wrap(~dag) +
  theme_void()

design <-
  declare_population(
    N = 100,
    X = rnorm(N),
    U = rnorm(N)
  ) +
  declare_potential_outcomes(
    Y ~ 0.1 * D + X + U, assignment_variables = D
  ) +
  declare_assignment(D = if_else(U > 0.5, 1, 0), handler = mutate) +
  reveal_outcomes(outcome_variables = Y, assignment_variables = D)

simulated_df <- draw_data(design)

simulated_df %>% select(ID, X, D, Y) %>% 
head(5) %>% 
kable(caption = "Simulated data from a DAG with varaibles $X$, $D$, and $Y$.", digits = 3, booktabs = TRUE)

gg_df <-
  gg_df %>%
  mutate(tile_fac = as.factor(if_else(
    !acyclic,
    "Ruled out by acyclicity",
    as.character(id_adjustment_fac)
  ))
  )

fill_scale <- c(
  `Ruled out by acyclicity` = dd_light_gray,
  `Yes, regardless of conditioning` = "transparent",  
  `Only when NOT conditioning on X` = dd_purple,
  `Only when conditioning on X` = dd_light_blue,
  `No, regardless of conditioning` = dd_pink
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
    scale_fill_manual("Effect of D on Y identified?", values = fill_scale, drop = FALSE, 
                      guide = guide_legend(nrow = 2, override.aes = list(color = c(rep("white", 4), gray(0.4))))) +
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

gg_df <-
  gg_df %>%
  mutate(tile_fac = as.factor(
    case_when(
      !acyclic ~ "Ruled out by acyclicity",
      consistent_with_ignorability == 0 ~ "Ruled out by ignorability",
      TRUE ~ as.character(id_adjustment_fac)
    )
  ))

fill_scale <- c(
  `Ruled out by acyclicity` = dd_light_gray,
  `Yes, regardless of conditioning` = "transparent",  
  `Only when NOT conditioning on X` = dd_purple,
  `Only when conditioning on X` = dd_light_blue,
  `No, regardless of conditioning` = dd_pink,
  `Ruled out by ignorability` = dd_dark_blue_alpha
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
    scale_fill_manual("Effect of D on Y identified?", values = fill_scale, drop = FALSE, 
                      guide = guide_legend(nrow = 2, override.aes = list(color = c(rep("white", 5), gray(0.4))))) +
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

gg_df <-
  gg_df %>%
  mutate(tile_fac = as.factor(if_else(
    !acyclic,
    "Ruled out by acyclicity",
    as.character(id_adjustment_fac)
  )))
gg_df <-
  gg_df %>%
  mutate(
    tile_fac2 = as.factor(case_when(
      ruled_out_by == "Acyclicity" ~ "Ruled out by acyclicity",
      ruled_out_by == "Measuring X before treatment" ~ "Ruled out by pretreatment measurement",
      ruled_out_by == "Random assignment" ~ "Ruled out by random assignment",
      is.na(ruled_out_by) ~ "Effect of D on Y identified"
    ))
  )
fill_scale <- c(
  `Ruled out by acyclicity` = dd_light_gray,
  `Ruled out by pretreatment measurement` = dd_orange,  
  `Ruled out by random assignment` = dd_dark_blue,
  `Effect of D on Y identified` = "transparent"
)
subplot_function <- function(data) {
  dag_df <-
    data %>%
    group_by(var, DX_fac, YX_fac, tile_fac2) %>%
    summarize(x = 1.5, y = 1.5, n = n(), .groups = "drop")
  g <-
    ggplot(data, aes(x, y)) +
    geom_tile(data = dag_df, aes(fill = tile_fac2), height = 1.75, width = 1.75, alpha = 0.5) +
    geom_text(data = points_df, aes(label = name), size = 5) +
    geom_dag_edges(aes(xend = xend, yend = yend),
                   edge_width = 0.4,
                   arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed")) +
    scale_fill_manual("Situation", values = fill_scale, drop = FALSE, guide = guide_legend(nrow = 2, override.aes = list(color = c(gray(0.4), rep("white", 3))))) +
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

gg_df <-
  gg_df %>%
  filter(is.na(ruled_out_by)) %>%
  mutate(
    ruled_out_by_sig_test = as.factor(if_else(DY_fac == "D~~~Y", "Ruled out if estimate is statistically significant", "Cannot be ruled out, regardless of statistical significance")),
    fct_rows = paste0(YX_fac, YU_fac, XU_fac)
  )

fill_scale <- c(
  `Ruled out if estimate is statistically significant` = dd_dark_blue,
  `Cannot be ruled out, regardless of statistical significance` = "transparent"
)

dag_df <-
  gg_df %>%
  group_by(var, DX_fac, DY_fac, YX_fac, ruled_out_by_sig_test, fct_rows) %>%
  summarize(x = 1.5, y = 1.5, n = n(), .groups = "drop")

g <-
  ggplot(gg_df, aes(x, y)) +
  geom_tile(
    data = dag_df,
    aes(fill = ruled_out_by_sig_test),
    height = 1.75,
    width = 1.75,
    alpha = 0.5
  ) +
  geom_text(data = points_df, aes(label = name), size = 10) +
  geom_dag_edges(
    aes(xend = xend, yend = yend),
    edge_width = 1,
    arrow_directed = grid::arrow(length = grid::unit(12, "pt"), type = "closed")
  ) +
  scale_fill_manual("Situation", values = fill_scale, drop = FALSE, guide = guide_legend(override.aes = list(color = c(gray(0.4), "white")))) +
  facet_grid(DY_fac ~ fct_rows) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 40),
    legend.text = element_text(size = 40),
    legend.key.size = unit(25, "mm"),
    strip.text = element_blank()
  )

g
