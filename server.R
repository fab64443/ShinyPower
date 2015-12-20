library(shiny)
library(ggplot2)

shinyServer(
  function(input, output) {
  mu0 = 30
  
  output$powerplot <- renderPlot({    
    
    mua <- input$mu
    sigma <-input$sigma
    n <- input$n
    bilateral <- input$bilateral
    alpha <- if(bilateral) input$alpha/2 else input$alpha
    
    xitc1 = mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
    
    g = ggplot(data.frame(mu = c(25,45)), aes(x=mu))
    g = g + stat_function(fun=dnorm, geom = "line",
                          args = list(mean=mu0, sd=sigma/sqrt(n)),
                          size = 1, col = "red")
    g = g + stat_function(fun=dnorm, geom = "line",
                          args = list(mean=mua, sd=sigma/sqrt(n)),
                          size = 1, col = "blue")
    g = g + geom_vline(xintercept = xitc1, size=2)
    
    if(bilateral) {
      xitc2 = mu0 + qnorm(alpha) * sigma / sqrt(n)
      g = g + geom_vline(xintercept = xitc2, size=2)
    }
    
    g    
  })
  
  output$power <- renderPrint({
    mua <- input$mu
    sigma <-input$sigma
    n <- input$n
    bilateral <- input$bilateral
    alpha <- if(bilateral) input$alpha/2 else input$alpha
    xitc1 = mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
    power = pnorm(xitc1,mean=mua,sd=sigma/sqrt(n),lower.tail=FALSE)
    
    if(bilateral) {    
      xitc2 = mu0 + qnorm(alpha) * sigma / sqrt(n)
      power = power + pnorm(xitc2,mean=mua,sd=sigma/sqrt(n))
    }
    
    power
    })
  
  output$formula <- renderUI({
    bilateral <- input$bilateral
    if (bilateral) {
      withMathJax(
        h6('$$P\\left(\\frac{\\bar{X}-\\mu_0}{\\sigma/\\sqrt{n}}<z_{\\alpha/2};\\mu=\\mu_a\\right)
           +P\\left(\\frac{\\bar{X}-\\mu_0}{\\sigma/\\sqrt{n}}>z_{1-\\alpha/2};\\mu=\\mu_a\\right)$$'))
    }
    else {
      withMathJax(
        h6('$$P\\left(\\frac{\\bar{X}-\\mu_0}{\\sigma/\\sqrt{n}}>z_{1-\\alpha};\\mu=\\mu_a\\right)$$'))
      }
    })
  
  output$documentation <- renderUI({ 
    tags$html(
      tags$head(tags$title('Power of a Hypothesis Test')),
      tags$body(
        h2('Power of a Hypothesis Test'),
        h4('Definition'),
        p('The probability of ',strong(em('not')),' committing a Type II error is called the ',strong('power'),
          'of a hypothesis test.'),
        h4('Effect Size'),
        p('To compute the power of the test, one offers an alternative view about the "true" value of the population parameter, assuming that the null hypothesis is false. The ', strong('effect size'), ' is the difference between the true value and the value specified in the null hypothesis.'),
        p(align="center",'Effect size = True value - Hypothesized value'),
        p('For example, suppose the null hypothesis states that a population mean is equal to 30. A researcher might ask: What is the probability of rejecting the null hypothesis if the true population mean is equal to 32 ? In this example, the effect size would be 32-30, which equals 2.'),
        h4('Factors That Affect Power'),
        p('The power of a hypothesis test is affected by four factors.'),
        tags$ul( 
          tags$li('Sample size (n). Other things being equal, the greater the sample size, the greater the power of the test.'),
          tags$li('The variance (sigma) of the sample. The lower the variance, the narrower the distribution and the greater the power of the test. If the distribution is narrower, the "true" value of the parameter and the value specified in the null hypothesis are more isolated.'),
          tags$li('Significance level (alpha). The higher the significance level, the higher the power of the test. If you increase the significance level, you reduce the region of acceptance. As a result, you are more likely to reject the null hypothesis. This means you are less likely to accept the null hypothesis when it is false; i.e., less likely to make a Type II error. Hence, the power of the test is increased.'),
          tags$li('The "true" value of the parameter being tested. The greater the difference between the "true" value of a parameter and the value specified in the null hypothesis, the greater the power of the test. That is, the greater the effect size, the greater the power of the test.')
        )
        
      )
    )  })
  
  
  output$usage <- renderUI({ 
    tags$html(
      tags$head(tags$title('Power of a Hypothesis Test')),
      tags$body(
        h2('App Usage')        
      )
    )
    })
  
  })
