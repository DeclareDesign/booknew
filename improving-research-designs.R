# ---
# Improving research designs
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

gulzar_khan_design <- 
  declare_population(
    villages = add_level(),
    citizens = add_level()
  ) +
  
  declare_potential_outcomes(
    
  ) + 
  
  declare_estimand()

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
                  "a^A",
                  "a^M",
                  "d",
                  "D",
                  "I",
                  "m",
                  "M"
                )),
                size = 4) +
  geom_dag_edges() +
  geom_dag_edges_arc(data = filter(gg_df, arced), curvature = -0.025) +
  theme_dag()
g
