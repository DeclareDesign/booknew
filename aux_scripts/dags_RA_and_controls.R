

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


points_df <-
  tibble(name = as.factor(c("D", "X", "Y", "U")),
         x = c(1, 2, 2, 1),
         y = c(1, 2, 1, 2)) 


subplot_function <- function(data) {
  dag_df <-
    data %>%
    group_by(var, DX_fac, YX_fac, tile_fac2) %>%
    summarize(x = 1.5, y = 1.5, n = n())
  
  g <- 
    ggplot(data, aes(x, y)) +
    geom_tile(data = dag_df, aes(fill = tile_fac2), height = 1.75, width = 1.75, alpha = 0.5) +
    geom_text(data = points_df, aes(label = name), size = 5) +
    geom_dag_edges(aes(xend = xend, yend = yend), 
                   edge_width = 0.4, 
                   arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed")) +
    scale_fill_manual("Situation", values = fill_scale, drop = FALSE) + 
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

ggsave(filename = "~/Desktop/dags-ra-control-x.pdf", g, width = 30, height = 21, limitsize = FALSE)



