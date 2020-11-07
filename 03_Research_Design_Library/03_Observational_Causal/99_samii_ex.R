# Grasping positivity and overlap is easy, as the following shows.
# Suppose at time 1 we randomly assign households in one county in northern California
# either to receive pamphlets on income inequality or to receive nothing,
# and we want to estimate the effect of the pamphlets on household members' attitudes toward redistribution.
# Then, at time 2 we survey not only households in that one county,
# but in all counties in the United States, even though none of the other counties received pamphlets.
# Would this research design provide credible evidence
# on the average effect of the pamphlets for all US households?
# Clearly not, because the identifying variation is limited to
# but a small and specific segment of the US population.
# This example may seem contrived, but as I show below it resembles
# what occurs in conventional regression studies.

library(DeclareDesign)

design <-
  declare_population(
    county = add_level(
      20,
      county_shock = c(0.5, rnorm(N - 1)),
      place = factor(rep(c(
        "NorCal", "Elsewhere"
      ), c(1, N - 1)),
      levels = c("NorCal", "Elsewhere"))
    ),
    household = add_level(20, household_shock = rnorm(N)),
    individual = add_level(2, individual_shock = rnorm(N))
  ) +
  declare_potential_outcomes(Y ~ Z * county_shock + county_shock + household_shock + individual_shock) +
  declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0),
                   CATE_NorCal = mean(Y_Z_1[place == "NorCal"] - Y_Z_0[place == "NorCal"])) +
  
  declare_assignment(blocks = place,
                     clusters = household,
                     block_prob = c(0.5, 0.0)) +
  declare_estimator(
    Y ~ Z ,
    model = lm_robust,
    fixed_effects = ~ county,
    clusters = household,
    se_type = "stata",
    estimand = c("ATE", "CATE_NorCal"),
    label = "County Fixed Effects"
  ) +
  declare_estimator(
    Y ~ Z ,
    model = lm_robust,
    clusters = household,
    se_type = "stata",
    estimand = c("ATE", "CATE_NorCal"),
    label = "No controls"
  )

simulations <- simulate_design(design, sims = 500)
diagnose_design(simulations)

