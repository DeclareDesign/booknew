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
    declare_model(N = 100, U = rnorm(N)) + 
    declare_potential_outcomes(Y ~ Z + U) + 
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) + 
    declare_assignment(m = 50) + 
    declare_estimator(Y ~ Z)

sims_df <- tibble(
    estimator_label = character(), term = character(), estimate = numeric(), 
    std.error = numeric(), statistic = numeric(), p.value = numeric(), 
    conf.low = numeric(), conf.high = numeric(), df = numeric(), 
    outcome = character(), inquiry_label = character(), inquiry = numeric()
)

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
    
    values <- reactiveValues(run_dat_df = NULL, run_est_df = NULL, sims_df = NULL)
    
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
        
        # temp <- values$df_data[-input$Delete, ]
        # values$df_data <- temp
        
        values$run_dat_df <- draw_data(des)
        values$run_est_df <- bind_cols(get_estimates(des, data = values$run_dat_df), draw_estimands(des)) 
        
        values$sims_df <- bind_rows(values$sims_df, values$run_est_df) %>% 
            mutate(latest_estimate = (1:n()) == n())
        
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
        
        estimates_plot <- 
            ggplot(data = values$run_est_df) + 
            geom_errorbar(aes(x = 0, ymin = conf.low, ymax = conf.high), color = "blue", width = 0.1) + 
            geom_point(aes(x = 0, y = estimate), color = "blue") + 
            coord_cartesian(xlim = c(-1, 1), ylim = c(-3, 3)) + 
            ylab("Estimated ATE (95% confidence interval)") + 
            ggtitle("Estimate from data draw") + 
            dd_theme() + 
            theme(axis.title.x = element_blank(), axis.text.x = element_blank()) 
        # estimates_plot
        
        print(values$sims_df)
        
        diagnostic_stat_plot <- 
            ggplot(data = values$sims_df, aes(x = estimate, fill = latest_estimate)) + 
            geom_histogram(aes(y = ..density..), binwidth = 0.1) + 
            scale_fill_manual(breaks = c(FALSE, TRUE), values = c("black", "blue")) + 
            # coord_cartesian(ylim = c(0, 5)) + 
            ylab("Density") + 
            xlab("Estimated ATE") + 
            ggtitle("Histogram of ATE estimates") + 
            coord_flip(xlim = c(-3, 3), ylim = c(0, 5)) + 
            dd_theme() +
            theme(legend.position = "none") 
        
        data_plot + estimates_plot + diagnostic_stat_plot + plot_layout(widths = c(1, .5, 1))
        
    })
    
    output$diagnosis_plot <- renderPlot({
        pp()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
