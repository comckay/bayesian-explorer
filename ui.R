library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Bayesian Explorer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      shiny::radioButtons(inputId = "prior_posterior",
                          label = "",
                          choices = c("Prior", "Posterior"),
                          selected = "Prior"),
      conditionalPanel(condition = "input.prior_posterior == 'Posterior'",
                       numericInput(inputId = "generative.mean",
                                    label = "True Mean",
                                    value = .5,
                                    min = 0,
                                    max = 1,
                                    step = .01),
                       numericInput(inputId = "generative.n",
                                    label = "N Trials",
                                    value = 1,
                                    min = 1,
                                    max = 100000,
                                    step = 1)),
      numericInput(inputId = "prior.mean",
                   label = "Prior Mean",
                   value = .5,
                   min = 0,
                   max = 1,
                   step = .01),
      numericInput(inputId = "prior.n",
                   label = "Prior N",
                   value = 7,
                   min = 2,
                   max = 100000,
                   step = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      splitLayout(
        cellwidths = c("25%", "25%", "25%", "25%"),
        shinydashboard::valueBoxOutput("plot.mean.valuebox"),
        shinydashboard::valueBoxOutput("sample.mean"),
        shinydashboard::valueBoxOutput("plot.variance.valuebox"),
        shinydashboard::valueBoxOutput("posterior.credint")
      ),
      plotOutput("beta.plot"),
      plotlyOutput("posterior.cdf")
    )
  )
))
