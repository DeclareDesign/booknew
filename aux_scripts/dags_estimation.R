

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
  filter(is.na(ruled_out_by)) %>% 
  mutate(
    ruled_out_by_sig_test = as.factor(if_else(DY_fac == "D~~~Y", "Ruled out by rejection of null hypothesis of no effect", "Cannot rule out")),
    fct_rows = paste0(YX_fac, YU_fac, XU_fac)
  )

fill_scale <- c(
  `Ruled out by rejection of null hypothesis of no effect` = dd_dark_blue,
  `Cannot rule out` = "transparent"
)

points_df <-
  tibble(name = as.factor(c("D", "X", "Y", "U")),
         x = c(1, 2, 2, 1),
         y = c(1, 2, 1, 2)) 


dag_df <-
  gg_df %>%
  group_by(var, DX_fac, DY_fac, YX_fac, ruled_out_by_sig_test, fct_rows) %>%
  summarize(x = 1.5, y = 1.5, n = n()) 

g <- 
  ggplot(gg_df, aes(x, y)) +
  geom_tile(data = dag_df, aes(fill = ruled_out_by_sig_test), height = 1.75, width = 1.75, alpha = 0.5) +
  geom_text(data = points_df, aes(label = name), size = 5) +
  geom_dag_edges(aes(xend = xend, yend = yend), 
                 edge_width = 0.4, 
                 arrow_directed = grid::arrow(length = grid::unit(4, "pt"), type = "closed")) +
  scale_fill_manual("Situation", values = fill_scale, drop = FALSE) + 
  facet_grid(DY_fac ~ fct_rows) +
  coord_fixed() + 
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom",
        strip.text.y = element_blank()) 

g

ggsave(filename = "~/Desktop/dags-estimation.pdf", g, width = 21, height = 6, limitsize = FALSE)



