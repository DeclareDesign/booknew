# ---
# Formalizing MIDA
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R


dag <-
  dagify(aW ~ w + I,
         m ~ M,
         aM ~ m + I,
         d ~ D + w,
         aD ~ A + d)

dag_base <- tidy_dagitty(dag) %>%
  select(name, direction, to, circular) %>%
  as_tibble



nodes_df <-
  tibble(
    name = c("M", "I", "D", "A", "m", "aM", "aW", "d", "aD", "w"),
    label = TeX(c("M", "I", "D", "A", "m", "a^M", "a^W", "d", "a^D", "w")),
    x = c(1, 2, 4, 5, 1, 2, 3, 4, 5, 3),
    y = c(3, 3, 3, 3, 2, 2, 2.5, 2, 2, 1)
  )

endnodes_df <-
  nodes_df %>%
  transmute(to = name, xend = x, yend = y)

gg_df <-
  dag_base %>%
  left_join(nodes_df, by = "name") %>%
  left_join(endnodes_df, by = "to")

gg_df <-
  gg_df %>%
  mutate(arced = (name == "w" & to == "R")) %>%
  arrange(name)

rect_df <-
  tibble(
    xmin = c(.5, 3.5),
    xmax = c(2.5, 5.5),
    ymin = c(1.5, 1.5),
    ymax = c(3.5, 3.5)
  )


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
                aes(label = label),
                size = 4) +
  geom_dag_edges() +
  geom_dag_edges_arc(data = filter(gg_df, arced), curvature = -0.3) +
  geom_rect(data = rect_df, aes(x = NULL, y = NULL, 
                                xend = NULL, yend = NULL,
                                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha = 0.25) +
  annotate("text", x = 1.5, y = 1.75, label = "Theory") +
  annotate("text", x = 4.5, y = 1.75, label = "Empirics") +
  annotate("text", x = 3, y = 2.75, label = "Truth") +
  theme_dag()
g



