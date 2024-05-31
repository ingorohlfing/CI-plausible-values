library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Interpretation of confidence interval for means"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("lower_mu", "Mean in the population",
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
    sample_size <- 25
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
    sample_size <- 25
    standard_error <- sample_sd / sqrt(sample_size)
    # lower_p_value <- round(
    #   pnorm(mean = sample_mean, input$lower_mu, sd = standard_error,
    #         lower.tail = FALSE), digits = 3)
    ggplot(data = NULL) +
      geom_segment(aes(x = input$lower_mu, xend = input$lower_mu,
                       y = 0, yend = dnorm(x = input$lower_mu,
                                           mean = input$lower_mu,
                                           sd = standard_error)),
                   color = "blue", linetype = "dashed") +
      geom_vline(aes(xintercept = sample_mean), color = "black") +
      stat_function(
        fun = dnorm, geom = "line", color = "blue",
        xlim = c(12, qnorm(0.975, mean = input$lower_mu, sd = standard_error)),
        args = list(mean = input$lower_mu, sd = standard_error),
      ) +
      stat_function(
        fun = dnorm, geom = "area", fill = "blue", alpha = 0.5,
        xlim = c(qnorm(0.975, mean = input$lower_mu, sd = standard_error), 21),
        args = list(mean = input$lower_mu, sd = standard_error)
      ) +
      scale_x_continuous("Mean", limits = c(9, 21), breaks = seq(9, 21, 1))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
