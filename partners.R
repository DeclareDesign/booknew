# ---
# Partners
# --- 

packages <- c("tidyverse", "DeclareDesign")
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
              "Partner constraint: At least two-thirds of subjects must be treated."),
    prob = c(0.2, 0.1),
    value = c(0.725, 0.725)
  )

ggplot(gg_df, aes(prob, value)) +
  geom_point() +
  geom_line(alpha = 0.6) +
  geom_text(data = annotations_df,
            aes(label = label),
            hjust = 0) +
  geom_rect(
    data = rect_df,
    aes(
      xmin = min_prob,
      xmax = max_prob,
      ymin = 0,
      ymax = 1
    ),
    alpha = 0.1,
    inherit.aes = FALSE
  ) +
  geom_hline(data = lines_df, aes(yintercept = threshold)) +
  annotate(
    "text",
    x = 17/24,
    y = 0.60,
    label = "Zone of agreement",
    angle = 270,
    hjust = 0
  ) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 6)) +
  scale_x_continuous(breaks = seq(0, 1, length.out = 7),
                     labels = c("0/6", "1/6", "2/6", "3/6", "4/6", "5/6", "6/6")) +
  facet_grid(diagnosand ~ ., scales = "free") +
  labs(x = "Proportion treated") +
  dd_theme() +
  theme(axis.title.y = element_blank(),
        panel.grid.minor.y = element_blank())
