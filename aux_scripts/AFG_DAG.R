rm(list = ls())
library(dagitty)
library(ggdag)
library(ggraph)
library(tidyverse)
library(latex2exp)

dag <-
  dagify(Yt2 ~ Yt1 + U,
         Yt1 ~ D + C + Yt0 + U,
         Yt0 ~ U,
         Z ~ U,
         C ~ Z + U,
         D ~ C + U)

tidy_dagitty(dag) %>% as_tibble %>% pull(name) %>% unique

gg_df <-
  tidy_dagitty(
    dag,
    layout = "manual",
    x = c(3, 4, 3, 1, 5, 2, 6),
    y = c(1, 1, 2, 0, 0, 1, 0)
  )

gg_df <-
  gg_df %>%
  mutate(arced_left = (name == "U" & to == "Yt0"),
         arced_right = (name == "U" & to %in% c("Yt1", "Yt2"))) %>%
  arrange(name)


g <-
  ggplot(data = filter(gg_df, !(arced_left | arced_right)), aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_node(color = "gray") +
  geom_dag_text(color = "black",
                parse = TRUE,
                label = TeX(c("C", "D", "U", "Y_{t=0}", "Y_{t=1}", "Y_{t=2}", "Z")),
                size = 2) +
  geom_dag_edges() +
  geom_dag_edges_arc(data = filter(gg_df, arced_left), curvature = -.5) +
  geom_dag_edges_arc(data = filter(gg_df, arced_right), curvature = +.3) +
  theme_dag()
g

ggsave("figures/AFG.png", g, height = 3.5, width = 5.5)

