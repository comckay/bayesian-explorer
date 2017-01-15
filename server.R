library(shiny)

shinyServer(function(input, output) {
  affirm.vec <- reactive({
    rbinom(n = 100000, prob = input$generative.mean, size = 1)
  })
  
  prior <- reactive({
    alpha = input$prior.mean * input$prior.n
    beta = (1 - input$prior.mean) * input$prior.n
    domain = seq(0, 1, 0.005)
    val = dbeta(domain, alpha, beta) 
    data.frame("domain" = domain, 
               "prob_dens" = val)
  })
  
  posterior <- reactive({
    affirm.vec = affirm.vec()
    affirm.vec = affirm.vec[1:input$generative.n]
    betaPosterior(prior.mean = input$prior.mean,
                  prior.n = input$prior.n,
                  affirm.n = sum(affirm.vec),
                  sample.n = input$generative.n) 
  })
  
  posterior.mean <- reactive({
    affirm.vec = affirm.vec()
    affirm.vec = affirm.vec[1:input$generative.n]
    betaPosteriorMean(prior.mean = input$prior.mean,
                      prior.n = input$prior.n, 
                      affirm.n = sum(affirm.vec),
                      sample.n = input$generative.n)
  })
  
  credint <- reactive({
    if (input$prior_posterior == "Prior") {
      prior = prior()
      credint = emdbook::ncredint(pvec = prior$domain, npost = prior$prob_dens, 
                                  level = .95, tol = 0.01, verbose = FALSE)
    } else {
      posterior = posterior()
      credint = emdbook::ncredint(pvec = posterior$domain, npost = posterior$prob_dens, 
                                  level = .95, tol = 0.01, verbose = FALSE)
    }
  })
  
  sample.mean <- reactive({
    affirm.vec = affirm.vec() %>% 
      .[1:input$generative.n] 
    mean(affirm.vec)
  })
  
  beta.variance <- reactive({
    if (input$prior_posterior == "Prior") {
      betaVariance(prior.mean = input$prior.mean, 
                   prior.n = input$prior.n, 
                   prior = T)
    } else {
      affirm.vec = affirm.vec()
      affirm.vec = affirm.vec[1:input$generative.n]
      betaVariance(prior.mean = input$prior.mean, 
                   prior.n = input$prior.n, 
                   sample.n = input$generative.n, 
                   affirm.n = sum(affirm.vec), 
                   prior = F)
    }
  })
  
  output$plot.mean.valuebox <- renderValueBox({
    if (input$prior_posterior == "Prior") {
      valueBox(value = input$prior.mean, 
               subtitle = "Prior Mean",
               color = "black",  
               icon = icon("scale", lib = "glyphicon"))
    } else {
      posterior.mean = posterior.mean()
      valueBox(value = round(posterior.mean, digits = 3), 
               subtitle = "Posterior Mean",
               color = "black",  
               icon = icon("scale", lib = "glyphicon"))
    }
  })
  
  output$plot.variance.valuebox <- renderValueBox({
    if (input$prior_posterior == "Prior") {
      prior.variance = beta.variance()
      valueBox(value = round(prior.variance, digits = 4), 
               subtitle = "Prior Variance", 
               icon = icon("transfer", lib = "glyphicon"), 
               color = "black") 
    } else {
      posterior.variance = beta.variance()
      valueBox(value = round(posterior.variance, digits = 4), 
               subtitle = "Posterior Variance", 
               icon = icon("transfer", lib = "glyphicon"), 
               color = "black") 
    }
  })
  
  output$posterior.credint <- renderValueBox({
    credint = credint()
    valueBox(value = paste(credint[1], "-", credint[2]),
             subtitle = "95% Credible Interval", 
             icon = icon("thumbs-up", lib = "glyphicon"), 
             color = "black") 
  })
  
  output$sample.mean <- renderValueBox({
    sample.mean = sample.mean()
    valueBox(value = round(sample.mean, digits = 3),
             subtitle = "Sample Mean", 
             icon = icon("equalizer", lib = "glyphicon"),
             color = "black")
  })
  
  output$beta.plot <- renderPlot({
    if (input$prior_posterior == "Prior") {
      prior = prior()
      credint = credint()
      plot(prior, type = "l")
      abline(v = input$prior.mean, col = "black", lty = 3)
      abline(v = credint[1], col = "blue", lty = 3)
      abline(v = credint[2], col = "blue", lty = 3) 
      legend("right", c("Prior", "Sample Mean", "95% Credible Int"), 
             col = c("black", "black", "blue"), 
             lty = c(1, 3, 3))
    } else {
      posterior = posterior()
      posterior.mean = posterior.mean()
      credint = credint()
      sample.mean = sample.mean()
      prior = prior()
      plot(posterior, type = "l")
      lines(prior, col = "green", lty = 3)
      abline(v = posterior.mean, col = "black", lty = 3)
      abline(v = sample.mean, col = "grey60", lty = 3)
      abline(v = credint[1], col = "blue", lty = 3)
      abline(v = credint[2], col = "blue", lty = 3) 
      legend("right", c("Posterior", "Prior", "Posterior - Mean", "Sample Mean", "95% Credible Int"), 
             col = c("black", "green", "black", "grey60", "blue"), 
             lty = c(1, 3, 3, 3, 3))
    }
  })
  
  output$posterior.cdf <- renderPlotly({
    if (input$prior_posterior == "Prior") {
      x = prior()
      domain = seq(0, 1, 0.005)
      cdf = ecdf(x = x$prob_dens) 
      plt = cdf(domain) %>%
        data.frame(x = .) %>%
        ggplot(aes(x = x)) + 
        stat_ecdf() + 
        theme_bw() 
      print(ggplotly(plt))
    } else {
      x = posterior()
      domain = seq(0, 1, 0.005)
      cdf = ecdf(x = x$prob_dens) 
      plt = cdf(domain) %>%
        data.frame(x = .) %>%
        ggplot(aes(x = x)) + 
        stat_ecdf() + 
        theme_bw() 
      print(ggplotly(plt))
    }
  }) 
})
