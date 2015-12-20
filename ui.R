library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  headerPanel("Calculating power for Gaussian Data"),
  sidebarPanel(
    p(strong('mu0 = 30')),
    sliderInput('mu', 'Mean',value = 32, min = 30, max = 38, step = 0.1),
    sliderInput('sigma', 'Variance',value = 4, min = 1, max = 10, step = 0.1),
    sliderInput('n', 'Sample size',value = 30, min = 10, max = 100, step = 5),
    sliderInput('alpha', 'alpha',value = 0.05, min = 0.01, max = 0.1, step = 0.01),
    checkboxInput("bilateral", "Bilateral", value = FALSE)
  ),
  
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Documentation",
                         htmlOutput("documentation")
                          ),
                tabPanel("Usage",
                         htmlOutput("usage")
                ),
                tabPanel("Plot", 
                         withMathJax(),
                         h2("Gaussian distributions"),
                         plotOutput('powerplot'),
                         h4('\nPower'),
                         uiOutput("formula"),
                         textOutput("power")) 
                        )
            )
  
))

  