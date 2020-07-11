rm(list = ls())
library(dagitty)
library(ggdag)
library(ggraph)
library(tidyverse)
library(latex2exp)

dag <-
  dagify(m ~ M,
         aM ~ m + I,
         d ~ D + m,
         aA ~ A + d)

gg_df <-
  tidy_dagitty(
    dag,
    layout = "manual",
    x = c(4, 3, 2, 1, 3, 1, 4, 2),
    y = c(1.1, 1.1, 1.1, 1.1, 1, 1, 1, 1)
  )

gg_df <-
  gg_df %>%
  mutate(arced = (name == "m" & to == "d"),) %>%
  arrange(name)


g <-
  ggplot(data = filter(gg_df, !arced), aes(
    x = x,
    y = y,
    xend = xend,
    yend = yend
  )) +
  geom_dag_node(color = "gray") +
  geom_dag_text(color = "black",
                parse = TRUE,
                label = TeX(c(
                  "A",
                  "a^A = A(d)",
                  "a^M = I(m)",
                  "d = D(m)",
                  "D",
                  "I",
                  "m = M()",
                  "M"
                )),
                size = 2) +
  geom_dag_edges() +
  geom_dag_edges_arc(data = filter(gg_df, arced), curvature = -0.025) +
  theme_dag()
g

ggsave("figures/MIDAG.png", g, height = 3.5, width = 5.5)

