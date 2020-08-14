# ---
# Redesign
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

expand.grid(param_1 = seq(0, 1, length.out = 100),
            param_2 = seq(0, 1, length.out = 100)) %>%
  mutate(diagnosand = 1 - sqrt((param_1 - 0.3) ^ 2 + (param_2 - 0.7) ^ 2)) %>%
  ggplot(aes(param_1, param_2)) + 
  geom_tile(aes(fill = diagnosand)) +
  geom_point(x = 0.3, y = 0.7, size = 2) +
  geom_contour(aes(z = diagnosand)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Design Parameter 1",
       y = "Design Parameter 2",
       title = "Diagnosands change as design parameters change")

