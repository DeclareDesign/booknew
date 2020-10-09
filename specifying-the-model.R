# ---
# Specifying the model
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R
library(dagitty)
library(dddag)
library(ggraph)

a_blue <- "#0072B2"
a_gray <- "grey80"

dag <-
  dagify(Y ~ X + Z + U,
         X ~ U,
         latent = "U")

gg_df <-
  tidy_dagitty(dag,
               layout = "manual",
               x = c(1, 0, -1, 1),
               y = c(1, 1, 0, 0))

gg_df <-
  gg_df %>%
  mutate(
    color = case_when(
      name == "U" ~ a_gray,
      name == "X" ~ a_blue,
      name == "Y" ~ a_blue,
      name == "Z" ~ a_blue
    )
  )


g <-
ggplot(gg_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(aes(color = color)) +
  scale_color_identity() +
  geom_dag_text(color = "black", family = "Helvetica") +
  geom_dag_edges() +
  theme_dag()

g

diff_in_cates <- 0.5
design <-
  declare_population(N = 100,
                     U = rnorm(N),
                     X = rbinom(N, 1, prob = pnorm(0.5 * U + rnorm(N)))) +
  declare_potential_outcomes(Y ~ 0.5 * X +
                               0.5 * Z +
                               diff_in_cates * X * Z +
                               0.5 * U)
