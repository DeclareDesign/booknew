
make_ggdd_df <- function(dag, nodes, design) {
  design_nodes <- get_design_nodes(design)
  
  endnodes <-
    nodes %>%
    transmute(to = name, xend = x, yend = y)
  
  
  dag %>%
    tidy_dagitty() %>%
    select(name, direction, to, circular) %>%
    as_tibble %>%
    left_join(design_nodes, by = "name") %>%
    left_join(nodes, by = "name") %>%
    left_join(endnodes, by = "to")
}


dd_dark_blue <- "#3564ED"
dd_light_blue <- "#72B4F3"
dd_gray <- gray(0.2)

shapes <-
  c(
    "sampling" = 25,
    "assignment" = 23,
    "measurement" = 22,
    "unmanipulated" = NA
  )
colors <-
  c(
    "sampling" = dd_light_blue,
    "assignment" = dd_light_blue,
    "measurement" = dd_light_blue,
    "unmanipulated" = NA
  )

base_dag_plot <- 
  ggplot(data = NULL, aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend,
  )) +
  geom_point(
    stroke = .5,
    color = dd_gray,
    aes(y = shape_y, shape = data_strategy, fill = data_strategy),
    size = 15
  ) +
  geom_dag_edges(
    edge_colour = dd_gray,
    edge_width = 0.7,
    arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed"),
  ) +
  geom_richtext(
    color = dd_gray,
    aes(label = label),
    fill = NA,
    label.color = NA,
    # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt"),
    size = 8
  ) +
  geom_richtext(
    color = dd_gray,
    aes(label = annotation, x = text_x, y = text_y),
    size = 4,
    fill = NA,
    label.color = NA,
    # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt")
  ) +
  scale_fill_manual(values = colors) +
  scale_shape_manual(values = shapes) +
  coord_cartesian(ylim = c(-150, 150),
                  xlim = c(-150, 150),
                  clip = "off") +
  theme_dag() +
  theme(legend.position = "none") +
  guides(fill = guide_legend(override.aes = list(size = 4)))

