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
      textOutput("lower_critical_value"),
      br(),
      sliderInput("upper_mu", "Upper population mean",
                  min = 22, max = 28, value = 28,
                  step = 0.01, round = 2),
      br(),
      textOutput("upper_p_value_output"),
      textOutput("upper_critical_value")
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
    p_value_calculation <- function(mu, lower_tail) {
      p_value <- 2*round(
        pnorm(q = sample_mean, mean = mu, sd = standard_error,
              lower.tail = lower_tail), digits = 3)
      return(p_value)
    }
    lower_p_value <- p_value_calculation(input$lower_mu, lower_tail = FALSE)
    upper_p_value <- p_value_calculation(input$upper_mu, lower_tail = TRUE)
    critical_value_calculation <- function(mu, lower_tail) {
      critical_value <- round(
        qnorm(p = 0.025, mean = mu, sd = standard_error, lower.tail = lower_tail),
        digits = 3)
      return(critical_value)
    }
    lower_critical_value <- critical_value_calculation(input$lower_mu,
                                                       lower_tail = FALSE)
    upper_critical_value <- critical_value_calculation(input$upper_mu,
                                                       lower_tail = TRUE)
    output$lower_p_value_output <- renderText(
      paste("p-value of sample mean for ", input$lower_mu, ": ", lower_p_value,
            sep = "")
      )
    output$upper_p_value_output <- renderText(
      paste("p-value of sample mean for ", input$upper_mu, ": ", upper_p_value,
            sep = "")
    )
    output$lower_critical_value <- renderText(
      paste("Critical value for ", input$lower_mu, ": ", lower_critical_value,
            sep = "")
    )
    output$upper_critical_value <- renderText(
      paste("Critical value for ", input$upper_mu, ": ", upper_critical_value,
            sep = "")
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
      geom_segment(aes(x = input$upper_mu, xend = input$upper_mu,
                       y = 0, yend = dnorm(x = input$upper_mu,
                                           mean = input$upper_mu,
                                           sd = standard_error)),
                   color = "red", linetype = "dashed") +
      stat_function(
        fun = dnorm, geom = "line", color = "blue",
        xlim = c(9, qnorm(0.975, mean = input$lower_mu, sd = standard_error)),
        args = list(mean = input$lower_mu, sd = standard_error),
      ) +
      stat_function(
        fun = dnorm, geom = "area", fill = "blue", alpha = 0.5,
        xlim = c(qnorm(0.975, mean = input$lower_mu, sd = standard_error), 31),
        args = list(mean = input$lower_mu, sd = standard_error)
      ) +
      stat_function(
        fun = dnorm, geom = "line", color = "red",
        xlim = c(qnorm(0.025, mean = input$upper_mu, sd = standard_error), 31),
        args = list(mean = input$upper_mu, sd = standard_error),
      ) +
      stat_function(
        fun = dnorm, geom = "area", fill = "red", alpha = 0.5,
        xlim = c(9, qnorm(0.025, mean = input$upper_mu, sd = standard_error)),
        args = list(mean = input$upper_mu, sd = standard_error)
      ) +
      scale_x_continuous("Mean", limits = c(9, 31), breaks = seq(9, 31, 1)) +
      scale_y_continuous("Density",
                         labels = seq(0, 0.15, by = 0.05),
                         breaks = seq(0, 0.15, by = 0.05)) +
      labs(caption = paste("Lower bound of confidence interval",
                           round(lower_bound, digits = 2),
                           "\nUpper bound of confidence interval: ",
                           round(upper_bound, digits = 2))) +
      theme_classic()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
