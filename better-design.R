# ---
# Better Design
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

# load packages for this section here. note many (DD, tidyverse) are already available, see scripts/package-list.R

gulzar_khan_design <- 
  
  declare_population(
    # study is conducted in two districts in Pakistan, with 311 and 359 villages in them
    districts = add_level(N = 2, name = c("Haripur", "Abbottabad"), N_villages = c(311, 359)),
    
    # villages nested within districts
    villages = add_level(N = N_villages),
    
    # avg. 6500 citizens per village
    citizens = add_level(N = 6500)
  ) +
  
  # main outcome is whether a citizen filed papers to run for office
  # we define potential outcomes in response to being assigned to a social, personal, or neutral appeal to run
  declare_potential_outcomes(
    filed_papers ~ rbinom(N, 1, prob = 0.05 + 0.05 * (Z_appeal == "social") + 0.01 * (Z_appeal == "personal")),
    assignment_variable = Z_appeal, conditions = c("neutral", "social", "personal")
  ) + 
  
  # inquiry is the difference in rates of filing papers between the social and personal appeal conditions
  declare_estimand(ATE = mean(filed_papers_Z_appeal_social - filed_papers_Z_appeal_personal)) + 
  
  # sample 192 villages
  declare_sampling(clusters = villages, n = 192, sampling_variable = "S_villages") + 
  
  # sample 48 citizens in each village via random walk
  declare_sampling(strata = villages, n = 48, sampling_variable = "S_citizens") + 
  
  # assign villages to three arms with equal probabilities for three types of appeals to run for office
  declare_assignment(
    m_each = c(48, 72, 72),
    clusters = villages,
    conditions = c("neutral", "social", "personal"),
    assignment_variable = Z_appeal
  ) + 
  
  # recode treatment assignment for analysis into indicators for the two conditions of interest
  declare_step(
    Z_social_village = if_else(Z_appeal == "social", 1, 0),
    Z_personal_village = if_else(Z_appeal == "personal", 1, 0),
    handler = mutate
  ) +
  
  # 1. run a linear regression with condition indicators
  # 2. calculate the difference in effects between people in villages assigned to social appeals compared
  #    to those assigned to personal appeals
  # 3. calculate robust standard errors clustered on village
  declare_estimator(
    filed_papers ~ Z_social_village + Z_personal_village, 
    linear_hypothesis = "Z_social_village - Z_personal_village = 0",
    term = "Z_social_village - Z_personal_village = 0",
    clusters = villages,
    model = lh_robust
  )

design <- 
  declare_population(N = 100) + 
  declare_potential_outcomes(Y ~ Z) +
  declare_assignment(prob = 0.5) + 
  declare_estimator(Y ~ Z, model = difference_in_means)
