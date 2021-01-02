
aes_df <-
  tibble(
    data_strategy = rep(c(
      "sampling",
      "assignment",
      "measurement",
      "unmanipulated"
    ), 2),
    answer_strategy = rep(c(
      "controlled",
      "uncontrolled"
    ), each = 4),
    fill = if_else(data_strategy == "unmanipulated", NA_character_, dd_light_blue),
    linetype = case_when(answer_strategy == "controlled" ~ "dashed", answer_strategy == "uncontrolled" ~ "solid"),
    size = case_when(answer_strategy == "controlled" ~ 0.2, answer_strategy == "uncontrolled" ~ 0),
    color_manipulated = if_else(data_strategy == "unmanipulated", NA_character_, dd_gray),
    color_controlled = if_else(answer_strategy == "uncontrolled", NA_character_, dd_gray),
    size_manipulated = if_else(data_strategy == "unmanipulated", 0, 0.3),
    sides = case_when(
      data_strategy == "sampling" ~ 3,
      data_strategy == "assignment" ~ 1000,
      data_strategy == "measurement" ~ 4,
      data_strategy == "unmanipulated" ~ 4
    ),
    angle = case_when(
      data_strategy == "sampling" ~ pi,
      data_strategy == "assignment" ~ 0,
      data_strategy == "measurement" ~ 0,
      data_strategy == "unmanipulated" ~ 0
    ),
    r = case_when(
      data_strategy == "sampling" ~ .35,
      data_strategy == "assignment" ~ .25,
      data_strategy == "measurement" ~ .35,
      data_strategy == "unmanipulated" ~ .35
    ),
    shape_nudge_y = case_when(
      data_strategy == "sampling" ~ 0.05,
      data_strategy == "assignment" ~ 0,
      data_strategy == "measurement" ~ 0,
      data_strategy == "unmanipulated" ~ 0
    )
  )

nudges_df <-
  expand_grid(x = unique(c(seq(1, 5, 0.5), 4/3, 5/3, 6/3, 7/3, 8/3, 9/3, 10/3, 11/3, 12/13, 13/3, 14/3, 15/3)), y = seq(1, 4, 0.5)) %>% 
  mutate(
    nudgex_N = x,
    nudgey_N = y + 0.5,
    nudgex_E = x + 0.75,
    nudgey_E = y,
    nudgex_S = x,
    nudgey_S = y - 0.5,
    nudgex_W = x - 0.75,
    nudgey_W = y
  ) %>%
  pivot_longer(cols = nudgex_N:nudgey_W) %>% 
  separate(name, into = c("var", "nudge_direction"), sep = "_") %>% 
  pivot_wider(id_cols = c("x", "y", "nudge_direction"), names_from = "var", values_from = "value") %>% 
  rename(text_x = nudgex, 
         text_y = nudgey)

make_dag_df <- 
  function(dag, nodes) {
    if(!all(nodes$data_strategy %in% c("assignment", "sampling", "measurement", "unmanipulated")))
      stop("I don't recognize some of your data strategy elements")
    if(!all(nodes$answer_strategy %in% c("controlled", "uncontrolled")))
      stop("I don't recognize some of your answer strategy elements")
      
    endnodes <-
      nodes %>%
      transmute(to = name, xend = x, yend = y)
    
    dag %>%
      tidy_dagitty() %>%
      select(name, direction, to, circular) %>%
      as_tibble %>%
      left_join(nodes, by = "name") %>%
      left_join(endnodes, by = "to") %>%
      left_join(nudges_df, by = c("x", "y", "nudge_direction")) %>%
      left_join(aes_df, by = c("data_strategy", "answer_strategy")) %>%
      mutate(shape_y = y + shape_nudge_y, exclusion_restriction = "no")
  }

family <- "Helvetica"

epsilon <- .29
base_dag_plot <- 
  ggplot(data = NULL, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
  )) +
  # edges
  geom_dag_edges(
    aes(edge_colour = exclusion_restriction, edge_linetype = exclusion_restriction),
    edge_width = 0.7,
    arrow_directed = grid::arrow(length = grid::unit(3, "pt"), type = "closed"),
  ) +
  scale_edge_color_manual(values = c(dd_gray, dd_light_gray)) + 
  scale_edge_linetype_manual(values = c("solid", "dotted")) + 
  # fill
  geom_regon(
    aes(
      x0 = x,
      y0 = shape_y,
      sides = sides,
      angle = angle,
      r = r * 0.89,
      fill = fill,
      color = color_manipulated,
      size = size_manipulated
    )
  ) +
  
  # lty
  geom_regon(
    aes(
      x0 = x,
      y0 = shape_y,
      sides = sides,
      angle = angle,
      linetype = linetype,
      size = size,
      color = color_controlled,
      r = r),
    fill = NA
  ) +
  
  # Letters in nodes
  geom_richtext(
    color = dd_gray,
    aes(label = label),
    family = family,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 7
  ) +
  # Annotations above (below) nodes
  geom_richtext(
    color = dd_gray,
    aes(label = annotation, x = text_x, y = text_y),
    family = family,
    size = 4,
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  # scales
  coord_fixed(xlim = c(0.25 - epsilon, 5.75 + epsilon), 
              ylim = c(0.5, 4.5)) +
  scale_fill_identity() +
  scale_linetype_identity() +
  scale_size_identity() +
  scale_color_identity() + 
  theme_dag() +
  theme(legend.position = "none",
        plot.margin = unit(rep(0, 4), "cm"))

