
#source("~/Dropbox/declaredesign_AEC/allthemodels.R")
rm(list = ls())

gg_df <- read_rds("models_df.rds")

dd_dark_blue <- "#3564ED"
dd_light_blue <- "#72B4F3"
dd_orange <- "#F38672"
dd_purple <- "#7E43B6"
dd_gray <- gray(0.2)
dd_pink <- "#C6227F"
dd_light_gray <- gray(0.8)
dd_dark_blue_alpha <- "#3564EDA0"
dd_light_blue_alpha <- "#72B4F3A0"


gg_df <-
  gg_df %>%
  mutate(tile_fac = as.factor(
    case_when(
      !acyclic ~ "Ruled out by acyclicity",
      consistent_with_ignorability == 0 ~ "Ruled out by ignorability",
      TRUE ~ "Effect of D on Y identified"
    )
  )
  )

fill_scale <- c(
  `Ruled out by acyclicity` = dd_light_gray,
  `Effect of D on Y identified` = "transparent",  
  `Ruled out by ignorability` = dd_dark_blue_alpha
)


points_df <-
  tibble(name = as.factor(c("D", "X", "Y", "U")),
         x = c(1, 2, 2, 1),
         y = c(1, 2, 1, 2)) %>% 
  mutate(controlled_for = if_else(name == "X", "yes", "no")) 


subplot_function <- function(data) {
  dag_df <-
    data %>%
    group_by(var, DX_fac, YX_fac, tile_fac) %>%
    summarize(x = 1.5, y = 1.5, n = n())
  
  g <- 
  ggplot(data, aes(x, y)) +
    geom_tile(data = dag_df, aes(fill = tile_fac), height = 1.75, width = 1.75, alpha = 0.5) +
    geom_text(data = points_df, aes(label = name), size = 5) +
    geom_dag_edges(aes(xend = xend, yend = yend), 
                   edge_width = 0.4, 
                   arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed")) +
    scale_fill_manual("Effect of D on Y identified?", values = fill_scale, drop = FALSE) + 
    facet_grid(YX_fac ~ DX_fac, switch = "both", labeller = label_parsed) +
    coord_fixed() + 
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5)) + 
    labs(subtitle = parse(text = as.character(unique(data$DY_fac))))
  if(as.character(unique(data$DY_fac)) == "D %->% Y"){
    g <- g + labs(title = as.character(unique(data$U_relationship_fac)))
  }
  g
}


gg_df %>% filter(DY_fac == "D %<-% Y", U_relationship_fac == "U affects: D") %>% subplot_function()

my_fun <- function(data){
  data %>%
    split(.$DY_fac) %>% 
    map(~subplot_function(.)) %>% 
    wrap_plots(nrow = 1) 
}

gg <- gg_df %>% 
  split(.$U_relationship_fac) %>% 
  map(my_fun) 

g <- wrap_plots(gg, ncol = 2, byrow = FALSE) + plot_layout(guides = "collect") & theme(legend.position = "bottom") 

ggsave(filename = "~/Desktop/dag-ignorability.pdf", g, width = 30, height = 21, limitsize = FALSE)








# geom_tile(data = points_df, aes(linetype = controlled_for), 
#           fill = NA, size = 1, height = 0.3, width = 0.3, color = gray(0.1)) + 
# scale_linetype_manual("Conditioning", values = c(`yes` = "dotted", `no` = "blank"), 
#                       drop = FALSE, na.translate = FALSE) + 