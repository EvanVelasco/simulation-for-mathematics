
# inputs for user to adjust
inputPanel(
  sliderInput("alpha_adj", label = "Alpha:",
              min = 0.01, max = 0.99, value = 0.5, step = 0.01),
  
  numericInput("lambda_adj", label = "Lambda:", value = 1),
  
  numericInput("n_adj", label = "n",
               value = 1000),
  
  numericInput("seed_adj", label = "Seed", value = NA)
)

# create two tabs for app
tabsetPanel(
  tabPanel("Visual Analysis", value=1,
           br(),
           plotOutput("plot")), 
  tabPanel("Cov(X0,X1)", value=2,
           br(),
           plotOutput("plot2"))
)

# plot of EAR(1) process
output$plot <- renderPlot({
  
  # conditional that uses set seed if user inputs one
  if (is.na(input$seed_adj)!=TRUE){
    set.seed(input$seed_adj)
  }
  
  # create data set using ear1 function
  dat <- ear1(a=input$alpha_adj, lambda = input$lambda_adj, n = input$n_adj)
  
  # line plot for EAR(1) process
  plot(dat, type='l', xlab = "Index", ylab = 'Value',
       main = paste0("Stationary First-Order Exponential Autoregressive Process with Alpha = ",input$alpha_adj, " and Lambda = ", input$lambda_adj))
})

# plot of simulated results for Cov(X0,X1)
output$plot2 <- renderPlot({
  
  # conditional that uses set seed if user inputs one
  if (is.na(input$seed_adj)!=TRUE){
    set.seed(input$seed_adj)
  }
  
  # create dat set using covEarVec function + calc true value of Cov(X0,X1)
  dat <- covEarVec(a=input$alpha_adj, lambda = input$lambda_adj, n = input$n_adj)
  true_value = input$alpha_adj / (input$lambda_adj^2)
  
  # plot showing calculation by iteration compared to the true value
  plot(dat, type='l', xlab = "Simulation Iteration", ylab = 'Value', 
       main = paste0("Simulation of Cov(X0,X1) for EAR(1) Process with Alpha = ", input$alpha_adj, 
                     " and Lambda = ", input$lambda_adj))
  abline(h=true_value, lty=2)
  text(input$n_adj*0.9, true_value, paste0("True Value: ", round(true_value,2)), 
       pos = 3, col = 'red', font = 2)
})