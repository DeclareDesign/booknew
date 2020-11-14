#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(patchwork)
library(shiny)
library(tidyverse)
library(DeclareDesign)

source("vayr.R")
source("ggplot_dd_theme.R")

des <- 
    declare_population(N = 100, U = rnorm(N)) + 
    declare_potential_outcomes(Y ~ Z + U) + 
    declare_estimand(ATE = mean(Y_Z_1 - Y_Z_0)) + 
    declare_assignment(m = 50) + 
    declare_estimator(Y ~ Z)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    fluidRow(
            actionButton("runButton", "Run design"),
    ),
    fluidRow(
        mainPanel(
            plotOutput("diagnosis_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values <- reactiveValues(run_dat_df = NULL, run_est_df = NULL, sims_df = simulate_design(des, sims = 25))
    
    # observeEvent(input$runButton, {
    #     # # temp <- values$df_data[-input$Delete, ]
    #     # # values$df_data <- temp
    #     # 
    #     # values$run_dat_df <- draw_data(des)
    #     # values$run_est_df <- bind_cols(get_estimates(des, data = values$run_dat_df), draw_estimands(des))
    #     # 
    #     # values$sims_df <- bind_rows(values$sims_df, values$run_est_df)
    #     
    # })
    
    pp <- eventReactive(c(input$runButton), {
        
        values$run_dat_df <- draw_data(des)
        values$run_est_df <- bind_cols(get_estimates(des, data = values$run_dat_df), draw_estimands(des)) 
        
        values$sims_df <- bind_rows(values$sims_df, values$run_est_df) %>% 
            mutate(latest_estimate = (1:n()) == n()) %>% 
            mutate(order = fct_reorder(as.factor(1:n()), estimate))
        
        summary_df <-
            values$run_dat_df %>%
            group_by(Z) %>%
            do(tidy(lm_robust(Y ~ 1, data = .))) %>%
            mutate(Y = estimate)
        
        data_plot <-
            ggplot(summary_df, aes(Z, Y)) +
            geom_point(
                data = values$run_dat_df,
                stroke = 0,
                position = position_jitter_ellipse(width = 0.18, height = 0.1),
                alpha = 0.2
            ) +
            geom_point(size = 3) +
            geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
            dd_theme() +
            # scale_y_continuous(breaks = seq(0, 1, length.out = 5)) +
            coord_cartesian(ylim = c(-3, 3)) +
            scale_x_discrete(breaks = c(0, 1), labels = c(0, 1)) +
            theme(axis.title.x = element_blank()) +
            ggtitle("Data from single draw") + 
            ylab("Outcome values")
        # data_plot
        
        print(values$sims_df)
        
        gg_df <-     
        values$sims_df %>% 
            mutate(cover = estimand <= conf.high & estimand >= conf.low)
        
        ci_plot <- 
            ggplot(aes(x = estimate)) + 
            geom_errorbar(aes(x = order, ymax = conf.high, ymin = conf.low, fill = cover)) + 
            geom_hline(aes(yintercept = 1), lwd = 1.25) +
            scale_fill_manual(breaks = c(FALSE, TRUE), values = c("red", "black")) + 
            ggtitle("Confidence Intervals") +
            coord_cartesian(ylim = c(-3, 3)) +
            dd_theme() +
            theme(legend.position = "none") 
        
        diagnostic_stat_plot <- 
            values$sims_df %>% 
            mutate(cover = estimand <= conf.high & estimand >= conf.low) %>% 
            ggplot() + 
            geom_bar(aes(y = cover)) + 
            geom_vline(aes(xintercept = 1))
        
        data_plot + ci_plot + plot_layout(widths = c(1, 1))
        
    })
    
    output$diagnosis_plot <- renderPlot({
        pp()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
