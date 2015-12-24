library(shiny)
library(ggplot2)

shinyServer(
  function(input, output) {
  mu0 = 30

  limitRange <- function(fun, min, max, mean=0, sd=1) {
    function(x) {
      y = ifelse(round(x+0.1,1)>min & round(x-0.1,1)<max, fun(x, mean=mean, sd=sd), NA)
      return(y)
    }
  }
    
  output$powerplot <- renderPlot({    
    
    mua <- input$mu
    sigma <-input$sigma
    n <- input$n
    htype <- input$htype == "B"
    alpha <- ifelse(htype,input$alpha/2,input$alpha)

    zcrit1 = mu0 + qnorm(alpha)*sigma/sqrt(n)        # left tail
    zcrit2 = mu0 + qnorm(1-alpha)*sigma/sqrt(n)      # right tail
    
    xmin = 20
    xmax = 40
    g = ggplot(data.frame(x=c(xmin,xmax)), aes(x=x))
    norm1 = stat_function(fun=dnorm, geom="line", args=list(mean=mu0, sd=sigma/sqrt(n)), size=1, col="red")
    norm2 = stat_function(fun=dnorm, geom="line", args=list(mean=mua, sd=sigma/sqrt(n)), size=1, col="blue")
    alpha1 = stat_function(fun=limitRange(dnorm, xmin, zcrit1, mu0, sigma/sqrt(n)), geom="area", fill="red", alpha=0.3)
    alpha2 = stat_function(fun=limitRange(dnorm, zcrit2, xmax, mu0, sigma/sqrt(n)), geom="area", fill="red", alpha=0.3)
    power1 = stat_function(fun=limitRange(dnorm, xmin, zcrit1, mua, sigma/sqrt(n)), geom="area", fill="blue", alpha=0.3)
    power2 = stat_function(fun=limitRange(dnorm, zcrit2, xmax, mua, sigma/sqrt(n)), geom="area", fill="blue", alpha=0.3)
    xline1 = geom_vline(xintercept=zcrit1, size=1)
    xline2 = geom_vline(xintercept=zcrit2, size=1)
    labels = labs(x="mu", y="density")

    p = g+norm1+norm2+alpha2+power2+xline2+labels
    if (htype) p = p+alpha1+power1+xline1
    p
    
  })
  
  output$values <- renderTable({
    mua <- input$mu
    sd <- input$sigma/sqrt(input$n)
    htype <- input$htype == "B"
    alpha <- ifelse(htype,input$alpha/2,input$alpha)
    zcrit1 = mu0 + qnorm(alpha)*sd
    zcrit2 = mu0 + qnorm(1-alpha)*sd
    power = pnorm(zcrit2,mua,sd,lower.tail=FALSE)
    power = power + ifelse(htype,pnorm(zcrit1,mua,sd),0)
    
    df = data.frame(
          Name = c("zcrit1","zcrit2","alpha","beta","power"),
          Value = as.character(round(c(zcrit1,zcrit2,alpha,1-power,power),3)), 
          stringsAsFactors=FALSE)
    if (!htype) df[1,2] = "--"
    df
  })
  
  output$hypothesis <- renderUI({
    htype <- input$htype == "B"
    if (htype) {
      withMathJax(
        h6('$$H_o:\\mu_0=30 \\qquad H_a:\\mu_0<>30$$'))
    }
    else {
      withMathJax(
        h6('$$H_o:\\mu_0=30 \\qquad H_a:\\mu_0>30$$'))
    }
  })
  
  output$formula <- renderUI({
    htype <- input$htype == "B"
    if (htype) {
      withMathJax(
        h6('$$Power = P\\left(\\frac{\\bar{X}-\\mu_0}{\\sigma/\\sqrt{n}}<z_{\\alpha/2};\\mu=\\mu_a\\right)
           +P\\left(\\frac{\\bar{X}-\\mu_0}{\\sigma/\\sqrt{n}}>z_{1-\\alpha/2};\\mu=\\mu_a\\right)$$'))
    }
    else {
      withMathJax(
        h6('$$Power = P\\left(\\frac{\\bar{X}-\\mu_0}{\\sigma/\\sqrt{n}}>z_{1-\\alpha};\\mu=\\mu_a\\right)$$'))
      }
    })
  
  output$documentation <- renderUI({ 
    tags$html(
      tags$head(tags$title('Power of a Hypothesis Test')),
      tags$body(
        h2('Power of a Hypothesis Test'),
        h4('Definition'),
        p('The ',strong('power'),' is the probability of correctly rejecting H0 when it is false.'),
        h4('Effect Size'),
        p('To compute the power of the test, one offers an alternative view about the "true" value of the population parameter, assuming that the null hypothesis is false. The ', strong('effect size'), ' is the difference between the true value and the value specified in the null hypothesis.'),
        p(align="center",'Effect size = True value - Hypothesized value'),
        p('For example, suppose the null hypothesis states that a population mean is equal to 30. A researcher might ask: What is the probability of rejecting the null hypothesis if the true population mean is equal to 32 ? In this example, the effect size would be 32-30, which equals 2.'),
        h4('Factors that affect Power'),
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
        h2('App Usage'),
        p('Use the control in the sidebar to change the four factors that affect the power of a hypothesis test. The plot is updated accordingly and the power and other parameters are recalculated.'),
        p('The red nromal distribution represent the null hypothesis (mu = 30) and the blue normal distribution the real value of mu.'),
        p('The vertical lines are the quantiles associated with the significance level.'),
        p('The colored area under the curves are the type I error (in red) and the power (in blue)')
        
      )
    )
    })
  
  })
