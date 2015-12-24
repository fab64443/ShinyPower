library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  headerPanel("Calculating power for Gaussian Data"),
  sidebarPanel(
    p(strong('Null hypothesis mean : 30')),
    sliderInput('mu', 'True mean (mu_a)',value = 34, min = 30, max = 38, step = 0.1),
    sliderInput('sigma', 'Sample variance (sigma)',value = 10, min = 5, max = 20, step = 0.1),
    sliderInput('n', 'Sample size (n)',value = 30, min = 10, max = 100, step = 5),
    sliderInput('alpha', 'Type I error (alpha)',value = 0.05, min = 0.01, max = 0.1, step = 0.01),
    radioButtons("htype", "Test type:", c("Unilateral"="U","Bilateral"="B"), inline=T)
    
  ),
  
  # numericInput(inputId, label, value, min = NA, max = NA, step = NA, width = NULL)
  
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
                         h3(align="center","Gaussian distributions"),
                         uiOutput("hypothesis"),
                         plotOutput('powerplot'),
                         uiOutput("formula"),
                         tableOutput("values"))
                        )
            )
  
))

  