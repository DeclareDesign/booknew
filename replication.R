# ---
# Replication
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R





gg_df <- 
  simulations %>%
  group_by(N) %>%
  summarise(`Original not replicated because:\n Replication outside of original ci` = mean(replication_outside),
            `Original not replicated because:\n Original outside of replication ci` = mean(original_outide),
            `Original not replicated because:\n Difference-in-SATEs is significant` = mean(dis_sig),
            `Original not replicated because:\n Difference-in-SATEs not affirmed equivalent within a difference of 0.2` = mean(dis_nonequiv)) %>%
  pivot_longer(cols = -N)

ggplot(gg_df, aes(N, value)) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ name) +
  theme_minimal() +
  ylim(0, 1.0) +
  labs(x = "Replication Study Sample Size",
       y = "Fraction of 'failures to replicate'",
       caption = "Original study N = 1000; True original SATE: 0.2; True replication SATE: 0.15")

