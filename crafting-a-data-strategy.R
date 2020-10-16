# ---
# Crafting a data strategy
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

library(tidyverse)
library(DeclareDesign)

set.seed(343)
gg_df <- fabricate(
  villages = add_level(N = 4, village_num = 1:4 + 1:4 * 0.1),
  households = add_level(N = 4,
                         household_num = 1:4 + 1:4 * 0.1),
  individuals = add_level(
    N = 4,
    X = village_num + c(0.25,-0.25, 0.25,-0.25),
    Y = household_num + c(0.25, 0.25,-0.25,-0.25),
    a = simple_rs(N),
    b = complete_rs(N),
    c = strata_rs(strata = villages),
    d = cluster_rs(clusters = households, simple = TRUE),
    e = cluster_rs(clusters = households),
    f = strata_and_cluster_rs(clusters = households, strata = villages),
    g = d * simple_rs(N),
    h = e * strata_rs(strata = households),
    i = f * strata_rs(strata = paste0(households, Y))
  )) %>%
  pivot_longer(cols = letters[1:9], names_to = "procedure", values_to = "sampled") %>%
  mutate(procedure = factor(
    procedure,
    levels = letters[1:9],
    labels = c(
      "Individual Random Sampling (simple)",
      "Individual Random Sampling (complete)",
      "Individual Random Sampling (stratified)",
      "Cluster Random Sampling (simple)",
      "Cluster Random Sampling (complete)",
      "Cluster Random Sampling (stratified)",
      "Multistage Random Sampling (simple)",
      "Multistage Random Sampling (complete)",
      "Multistage Random Sampling (stratified)"
    )
  ))



ggplot(gg_df, aes(X, Y)) + 
  geom_tile(aes(fill = sampled, alpha = 0.1), color = "grey", width = 0.48, height = 0.48) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none") +
  facet_wrap(~procedure)


