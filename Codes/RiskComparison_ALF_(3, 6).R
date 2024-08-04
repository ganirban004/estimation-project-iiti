set.seed(36)


### CREATING THE SAMPLES ###

# CHARACTERISTICS OF SAMPLES
n1 <- 3
n2 <- 6
theta1 <- 10

theta <- c()
risk_N_1 <- c()
risk_N_2 <- c()
risk_N_3 <- c()
risk_N_1_I <- c()

for (j in 2:20)
{
  theta2 <- j
  theta[j] <- (theta2/theta1)
  
  loss_N_1 <- c()
  loss_N_2 <- c()
  loss_N_3 <- c()
  loss_N_1_I <- c()
  
  for (i in 1:25000)
  {
    # SIMULATING SAMPLES
    sample1 <- c(runif(n1, 0, theta1))
    sample2 <- c(runif(n2, 0, theta2))
    
    
    ### CALCULATION OF VARIOUS ESTIMATORS ###
    
    if (n1<=n2)
    {
      a_star <- (((n1+n2)/(2*n2))^(1/n1))
    } else
    {
      a_star <- (((2*n1)/(n1+n2))^(1/n2))
    }
    X1 <- max(sample1)
    X2 <- max(sample2)
    if (X1> (a_star*X2))
    {
      theta_selected <- theta1
      est_N_1 <- X1
      est_N_2 <- ((n1+1)*X1/n1)
      est_N_3 <- ((((n1+1)/(n1-1))^(1/2))*X1)
    } else
    {
      theta_selected <- theta2
      est_N_1 <- X2
      est_N_2 <- ((n2+1)*X2/n2)
      est_N_3 <- ((((n2+1)/(n2-1))^(1/2))*X2)
    }
    
    est_N_1_I <- (((n1+n2+1)/(n1+n2-1))^(1/2))*est_N_1
    
    
    ### CALCULATING LOSSES OF VARIOUS ESTIMATORS ###
    loss_N_1[i] <- ((est_N_1/theta_selected)+(theta_selected/est_N_1)-2)
    loss_N_2[i] <- ((est_N_2/theta_selected)+(theta_selected/est_N_2)-2)
    loss_N_3[i] <- ((est_N_3/theta_selected)+(theta_selected/est_N_3)-2)
    loss_N_1_I[i] <- ((est_N_1_I/theta_selected)+(theta_selected/est_N_1_I)-2)
  }
  
  
  ### CALCULATING RISKS ###
  risk_N_1[j] <- mean(loss_N_1)
  risk_N_2[j] <- mean(loss_N_2)
  risk_N_3[j] <- mean(loss_N_3)
  risk_N_1_I[j] <- mean(loss_N_1_I)
}

theta <- theta[-1]
risk_N_1 <- risk_N_1[-1]
risk_N_2 <- risk_N_2[-1]
risk_N_3 <- risk_N_3[-1]
risk_N_1_I <- risk_N_1_I[-1]

cat("\n",
    "theta= (", theta, ")\n",
    "Risks of N_1 estimator= (", risk_N_1, ")\n",
    "Risks of N_2 estimator= (", risk_N_2, ")\n",
    "Risks of N_3 estimator= (", risk_N_3, ")\n",
    "Risks of N_1_I estimator= (", risk_N_1_I, ")\n"
)


### PLOTTING COMPARISON ###
matplot(theta, cbind(risk_N_1, risk_N_2, risk_N_3, risk_N_1_I), xlim = c(0.2, 2), ylim = c(0, 0.25),
        xlab = expression(paste("Values of ", theta)), ylab = "Risk Values", pch = c(16, 5, 14, 2),
        col = c("blue3", "green4", "blueviolet", "red"))
legend(1.5, 0.25, legend = c("N_1 estimator", "N_2 estimator", "N_3 estimator","N_1_I estimator"),
       pch = c(16, 5, 14, 2), col = c("blue3", "green4", "blueviolet", "red"))
lines(theta, risk_N_1, col = "blue3", pch = 16)
lines(theta, risk_N_2, col = "green4", pch = 5)
lines(theta, risk_N_3, col = "blueviolet", pch = 14)
lines(theta, risk_N_1_I, col = "red", pch = 2)