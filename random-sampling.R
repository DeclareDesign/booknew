# ---
# Random sampling
# --- 

packages <- c("knitr", "tidyverse", "DeclareDesign", "DesignLibrary")
lapply(packages, require, character.only = TRUE)

fixed_population <- declare_population(N = 500, Y = sample(1:7, N, replace = TRUE))()

design <- 
  declare_population(data = fixed_population) + 
  declare_estimand(Y_bar = mean(Y)) + 
  declare_sampling(n = 100) + 
  declare_estimator(Y ~ 1, model = lm_robust, estimand = "Y_bar")

dag <- dagify(Y ~ S)

nodes <-
  tibble(
    name = c("Y", "S"),
    label = c("Y", "S"),
    annotation = c("**Outcome**<br>measured only for sampled units",
                   "**Sampling indicator**<br>randomly set by designer"),
    x = c(5, 1),
    y = c(2.5, 2.5),
    nudge_direction = c("N", "N"),
    answer_strategy = "uncontrolled"
  )

ggdd_df <- make_dag_df(dag, nodes, design)

base_dag_plot %+% ggdd_df

fixed_population <- declare_population(N = 500, Y = sample(1:7, N, replace = TRUE))()

random_sampling_design <- 
  declare_population(data = fixed_population) + 
  declare_estimand(Ybar = mean(Y)) + 
  declare_sampling(n = 100) + 
  declare_estimator(Y ~ 1, model = lm_robust, estimand = "Ybar")

plot(random_sampling_design)

## diagnosis <- diagnose_design(
##   design, sims = sims, bootstrap_sims = b_sims, diagnosands = diagnosands)

N_blocks <- 1
N_clusters_in_block <- 1000
N_i_in_cluster <- 50
n_clusters_in_block <- 30
n_i_in_cluster <- 20
icc <- 0.402

# M: Model
fixed_pop <-
  declare_population(
    block = add_level(N = N_blocks),
    cluster = add_level(N = N_clusters_in_block),
    subject = add_level(N = N_i_in_cluster,
                        latent = draw_normal_icc(mean = 0, N = N, clusters = cluster, ICC = icc),
                        Y = draw_ordered(x = latent, breaks = qnorm(seq(0, 1, length.out = 8)))
    )
  )()

cluster_sampling_design <- declare_population(data = fixed_pop) +
  
  # I: Inquiry
  declare_estimand(Ybar = mean(Y)) +
  
  # D: Data Strategy
  declare_sampling(strata = block, 
                   clusters = cluster, n = n_clusters_in_block, 
                   sampling_variable = "Cluster_Sampling_Prob") +
  
  declare_sampling(strata = cluster,   n = n_i_in_cluster, 
                   sampling_variable = "Within_Cluster_Sampling_Prob") +
  
  # A: Answer Strategy
  declare_estimator(Y ~ 1,
                    model = lm_robust,
                    clusters = cluster,
                    estimand = "Ybar",
                    label = "Clustered Standard Errors")




kable(reshape_diagnosis(diagnosis), digits = 2)

new_design <- cluster_sampling_design + declare_estimator(Y ~ 1,
                                                          model = lm_robust,
                                                          estimand = "Ybar",
                                                          label = "Naive Standard Errors")




sims <- diagnosis$simulations
sims$covered <- factor(1 + (sims$conf.low < sims$estimand & sims$estimand < sims$conf.high), 1:2, labels = c("Estimand not covered by confidence interval", "Estimand covered by confidence interval"))
sims$estimator_label <- as.factor(sims$estimator_label)
sims$estimator_label <- factor(sims$estimator_label, levels = rev(levels(sims$estimator_label)))
sims$estimand_label  <- as.factor(sims$estimand_label)

ggplot(sims, aes(x=estimate)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color=covered), alpha=.4) +
  geom_hline(aes(yintercept=mean(estimand))) +
  geom_text(aes(x=x, y=y, label=label),
            data=function(df){
              data.frame(x=min(df$estimate),
                         y=mean(df$estimand),
                         label=sprintf('  Avg Estimand:\n  %4.3f', mean(df$estimand)),
                         stringsAsFactors = FALSE)
            }, hjust='left') +
  facet_wrap(estimand_label~estimator_label) +
  ylab("Estimate") +
  scale_x_continuous(labels=NULL, breaks = NULL, name='') +
  scale_color_discrete(drop=FALSE, name = '') +
  coord_flip() +
  dd_theme()
