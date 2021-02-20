# ---
# Piloting
# --- 

packages <- c("tidyverse", "DeclareDesign")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

full_dist <-
  tibble(
    effect_size = c(seq(-0.25,-0.01, by = 0.01),
                    seq(0.01, 0.75, by = 0.01)),
    N_required = sapply(
      effect_size,
      FUN = function(x)
        power.t.test(
          delta = abs(x),
          sd = 1,
          power = 0.8
        )$n * 2
    ),
    density = dnorm(effect_size, 0.3, 0.1)
  )

label_df <-
  full_dist %>%
  filter(round(effect_size, 2) %in% c(0.10, 0.17, 0.3)) %>%
  mutate(label = c("3200 subjects required at 0.10",
                   "1100 subjects required at 0.17",
                   "350 subjects required at 0.30"),
         label2 = c("True effect size",
                   "10th percentile guess",
                   "Average (best) guess"))

g1 <- 
ggplot(label_df, aes(effect_size, N_required)) +
  geom_line(data = full_dist) +
  geom_point(aes(color = label2, shape = label2)) +
  geom_segment(aes(xend = effect_size, yend = 0, color = label2)) +
  geom_text(aes(label = label, color = label2), hjust = 0, nudge_x = 0.02) +
  coord_cartesian(xlim = c(-0.25, 0.75),
                  ylim = c(0, 5000)) +
  dd_theme() +
  theme(legend.position = "none") +
  labs(x = "Effect size in standard units",
       y = "Sample size needed for 80% power",
       title = "Minimum sample sizes for 80% power, by effect size")


g2 <-
ggplot(label_df, aes(effect_size, density)) +
    geom_point(aes(color = label2, shape = label2)) +
  geom_segment(aes(xend = effect_size, yend = 0, color = label2)) +
  geom_text(aes(label = label2, color = label2), hjust = 1, nudge_x = -0.02) +
  geom_line(data = full_dist) +
  coord_cartesian(xlim = c(-0.25, 0.75)) +
  dd_theme() +
    theme(legend.position = "none") +
  labs(x = "Effect size in standard units",
       y = "Density of prior belief distribution",
       title = "Researcher's prior beliefs about effect size")


g1 / g2


prior <- 0.3
prior_sd <- 0.1

calculate_n_required <- function(delta, sd = 1, power = 0.8) {
  sapply(
    delta,
    FUN = function(x)
      power.t.test(
        delta = x,
        sd = sd,
        power = power
      )$n * 2
  )
  
}

pilot <-
  declare_model(N = 100,
                U = rnorm(N),
                potential_outcomes(Y ~ 0.1 * Z + U)) +
  declare_inquiry(SATE = mean(Y_Z_1 - Y_Z_0)) +
  declare_assignment(Z = complete_ra(N), legacy = FALSE) +
  declare_measurement(Y = reveal_outcomes(Y ~ Z)) +
  declare_estimator(Y ~ Z, inquiry = "SATE")





pilot_sims <-
  pilot_sims %>%
  filter(N_required <= 7500, !is.na(N_required))
choices_df <-
  tibble(
    xintercept = c(calculate_n_required(qnorm(0.1, prior, prior_sd)),
                   calculate_n_required(0.1),
                   mean(pilot_sims$N_required)),
    label = c("Choice of N without pilot",
              "Best choice of N",
              "Average choice of N with pilot"),
    x = xintercept + 100,
    y = c(130, 110, 95)
  )

ggplot(pilot_sims, aes(N_required)) + 
  geom_histogram(bins = 40) +
  geom_segment(data = choices_df, aes(x = xintercept, xend = xintercept, y = 0, yend = y - 5), color = dd_dark_blue) +
  geom_text(data = choices_df, aes(x = x, y = y, label = label), color = dd_dark_blue, hjust = 0.2) +
  dd_theme() +
  theme(legend.position = "none") +
  labs(x = "Distribution of choice of N, depending on results of pilot")
