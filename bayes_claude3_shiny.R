library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Graham's Bayes' Theorem Calculator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("prior_double_headed", "Prior probability of a double-headed coin:",
                   value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("num_tosses", "Number of tosses (all heads):",
                   value = 1, min = 1, step = 1),
      actionButton("calculate", "Calculate")
    ),
    mainPanel(
      verbatimTextOutput("prior_prob"),
      verbatimTextOutput("num_tosses_output"),
      verbatimTextOutput("posterior_prob")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  likelihood <- function(coin_type, num_tosses) {
    if (coin_type == "normal") {
      return(0.5^num_tosses)
    } else {
      return(1)
    }
  }
  
  posterior <- function(prior, likelihood, num_tosses) {
    likelihood_normal <- likelihood("normal", num_tosses)
    likelihood_double_headed <- likelihood("double_headed", num_tosses)
    
    posterior_normal <- prior[1] * likelihood_normal
    posterior_double_headed <- prior[2] * likelihood_double_headed
    
    posterior_normalized <- c(posterior_normal, posterior_double_headed) / sum(posterior_normal, posterior_double_headed)
    
    return(posterior_normalized)
  }
  
  observeEvent(input$calculate, {
    prior_double_headed <- input$prior_double_headed
    prior_normal <- 1 - prior_double_headed
    prior <- c(prior_normal, prior_double_headed)
    
    num_tosses <- input$num_tosses
    
    posterior_prob <- posterior(prior, likelihood, num_tosses)
    
    output$prior_prob <- renderText({
      paste("Prior probability:\n",
            "Normal coin:", prior[1], "\n",
            "Double-headed coin:", prior[2])
    })
    
    output$num_tosses_output <- renderText({
      paste("Number of tosses (all heads):", num_tosses)
    })
    
    output$posterior_prob <- renderText({
      paste("Posterior probability:\n",
            "Normal coin:", posterior_prob[1], "\n",
            "Double-headed coin:", posterior_prob[2])
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)