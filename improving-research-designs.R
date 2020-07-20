# ---
# Improving research designs
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = T)

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

dag <-
  dagify(m ~ M,
         I ~ M,
         aM ~ m + I,
         d ~ D + W,
         A ~ D,
         D ~ M,
         aA ~ A + d)

gg_df <-
  tidy_dagitty(
    dag,
    layout = "manual",
    x = c(4, 3, 2, 1, 3, 3, 1, 4, 2),
    y = c(1.1, 1.1, 1.1, 1.1, 0.9, 1, 1, 1, 1)
  )

gg_df <-
  gg_df %>%
  mutate(arced = (name == "M" & to == "D")) %>%
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
                  "a^D",
                  "a^M",
                  "d",
                  "D",
                  "I",
                  "m",
                  "M",
                  "W"
                )),
                size = 4) +
  geom_dag_edges() +
  geom_dag_edges_arc(data = filter(gg_df, arced), curvature = 0.025) +
  theme_dag()
g
