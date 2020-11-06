# ---
# Partners
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R





rect_df <- 
  gg_df %>%
  filter(acceptable_power, acceptable_utility) %>%
  summarise(min_prob = min(prob),
            max_prob = max(prob))

lines_df <-
  tibble(
    diagnosand = c("Power", "Utility"),
    threshold = c(0.8, 2/3)
  )


annotations_df <-
  tibble(
    diagnosand = c("Power", "Utility"),
    label = c("Researcher constraint: Power must be above 0.80.",
                       "Partner constraint: Two-thirds of subjects must be treated."),
    prob = c(0.2, 0.1),
    value = c(0.725, 0.725)
  )

ggplot(gg_df) +
  geom_rect(data = rect_df, aes(xmin = min_prob, xmax = max_prob, ymin = 0, ymax = 1), alpha = 0.1) +
  geom_line(aes(prob, value)) +
  geom_text(data = annotations_df, aes(prob, value, label = label), hjust = 0) + 
  geom_hline(data = lines_df, aes(yintercept = threshold)) +
  annotate("text", x = 0.75, y = 0.50, label = "Zone of agreement", angle = 270, hjust = 0) + 
  facet_grid(diagnosand~., scales = "free") +
  labs(x = "Proportion treated") +
  dd_theme() + 
  theme(axis.title.y = element_blank())
