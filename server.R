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

  })
