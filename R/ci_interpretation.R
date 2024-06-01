library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Interpretation of confidence interval for means"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("lower_mu", "Lower population mean",
                  min = 12, max = 18, value = 12,
                  step = 0.01, round = 2),
      br(),
      textOutput("lower_p_value_output"),
      textOutput("lower_critical_value")
    ),
    mainPanel(
      plotOutput("obs_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Calculate upper-tail p-value for given confidence and selected level of mu
  observe({
    sample_mean <- 20
    sample_sd <- 15
    sample_size <- 27
    standard_error <- sample_sd / sqrt(sample_size)
    lower_p_value <- 2*round(
      pnorm(q = sample_mean, mean = input$lower_mu, sd = standard_error,
                     lower.tail = FALSE), digits = 3)
    lower_critical_value <- round(
      qnorm(p = 0.975, mean = input$lower_mu, sd = standard_error), digits = 3)
    output$lower_p_value_output <- renderText(
      paste("p-value of sample mean: ", lower_p_value)
      )
    output$lower_critical_value <- renderText(
      paste("Critical value: ", lower_critical_value)
    )
  })

  # plot the sampling distribution for the chosen slider input and with sample mean highlighted
  output$obs_plot <- renderPlot({
    sample_mean <- 20
    sample_sd <- 15
    sample_size <- 27
    standard_error <- sample_sd / sqrt(sample_size)
    lower_bound <- sample_mean - 1.96*standard_error
    upper_bound <- sample_mean + 1.96*standard_error
    # lower_p_value <- round(
    #   pnorm(mean = sample_mean, input$lower_mu, sd = standard_error,
    #         lower.tail = FALSE), digits = 3)
    ggplot(data = NULL) +
      geom_pointrange(aes(x = sample_mean, y = 0,
                          xmin = lower_bound,
                          xmax = upper_bound),
                      color = "black", linewidth = 1, size = 1) +
      geom_segment(aes(x = input$lower_mu, xend = input$lower_mu,
                       y = 0, yend = dnorm(x = input$lower_mu,
                                           mean = input$lower_mu,
                                           sd = standard_error)),
                   color = "blue", linetype = "dashed") +
      geom_vline(aes(xintercept = sample_mean), color = "black") +
      stat_function(
        fun = dnorm, geom = "line", color = "blue",
        xlim = c(9, qnorm(0.975, mean = input$lower_mu, sd = standard_error)),
        args = list(mean = input$lower_mu, sd = standard_error),
      ) +
      stat_function(
        fun = dnorm, geom = "area", fill = "blue", alpha = 0.5,
        xlim = c(qnorm(0.975, mean = input$lower_mu, sd = standard_error), 27),
        args = list(mean = input$lower_mu, sd = standard_error)
      ) +
      scale_x_continuous("Mean", limits = c(9, 27), breaks = seq(9, 27, 1)) +
      scale_y_continuous("Density",
                         labels = seq(0, 0.15, by = 0.05),
                         breaks = seq(0, 0.15, by = 0.05)) +
      labs(caption = paste("Lower bound: ", round(lower_bound, digits = 2),
                           "\nUpper bound: ", round(upper_bound, digits = 2))) +
      theme_classic()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
