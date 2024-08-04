set.seed(45)


### CREATING THE SAMPLES ###

# CHARACTERISTICS OF SAMPLES
n1 <- 4
n2 <- 5
theta1 <- 10

theta <- c()
risk_umru <- c()
risk_umru_dom <- c()
risk_natural_1 <- c()
risk_natural_1_dom <- c()
risk_natural_2 <- c()

for (j in 2:12)
{
  theta2 <- j
  theta[j] <- (theta2/theta1)
  
  loss_umru <- c()
  loss_umru_dom <- c()
  loss_natural_1 <- c()
  loss_natural_1_dom <- c()
  loss_natural_2 <- c()
  
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
      est_umru <- ((n1*X1)/((n1-1)+((a_star*X2/X1)^n1)))
      phi_star <- ((n1+n2)/(n1+n2-1))
      psi_U <- (n1/((n1-1)+((a_star*X2/X1)^n1)))
      est_natural_1 <- X1
      est_natural_2 <- (n1*X1/(n1-1))
    } else
    {
      theta_selected <- theta2
      est_umru <- ((n2*X2)/((n2-1)+((X1/(X2*a_star))^n2)))
      phi_star <- ((n1+n2)*(X2/X1)/(n1+n2-1))
      psi_U <- (n2*X2/X1)/((n2-1)+((X1/(a_star*X2))^n2))
      est_natural_1 <- X2
      est_natural_2 <- (n2*X2/(n2-1))
    }
    
    est_umru_dom <- (X1 * (max(phi_star, psi_U)))
    est_natural_1_dom <- ((n1+n2)*est_natural_1/(n1+n2-1))
    
    
    ### CALCULATING LOSSES OF VARIOUS ESTIMATORS ###
    loss_umru[i] <- ((theta_selected/est_umru)-(log(theta_selected/est_umru))-1)
    loss_umru_dom[i] <- ((theta_selected/est_umru_dom)-(log(theta_selected/est_umru_dom))-1)
    loss_natural_1[i] <- ((theta_selected/est_natural_1)-(log(theta_selected/est_natural_1))-1)
    loss_natural_1_dom[i] <- ((theta_selected/est_natural_1_dom)-(log(theta_selected/est_natural_1_dom))-1)
    loss_natural_2[i] <- ((theta_selected/est_natural_2)-(log(theta_selected/est_natural_2))-1)
  }
  
  
  ### CALCULATING RISKS ###
  risk_umru[j] <- mean(loss_umru)
  risk_umru_dom[j] <- mean(loss_umru_dom)
  risk_natural_1[j] <- mean(loss_natural_1)
  risk_natural_1_dom[j] <- mean(loss_natural_1_dom)
  risk_natural_2[j] <- mean(loss_natural_2)
}

theta <- theta[-1]
risk_umru <- risk_umru[-1]
risk_umru_dom <- risk_umru_dom[-1]
risk_natural_1 <- risk_natural_1[-1]
risk_natural_1_dom <- risk_natural_1_dom[-1]
risk_natural_2 <- risk_natural_2[-1]

cat("\n",
    "theta= (", theta, ")\n",
    "Risks of UMRU estimator= (", risk_umru, ")\n",
    "Risks of DOMINATED UMRU estimator= (", risk_umru_dom, ")\n",
    "Risks of FIRST NATURAL estimator= (", risk_natural_1, ")\n",
    "Risks of DOMINATED FIRST NATURAL estimator= (", risk_natural_1_dom, ")\n",
    "Risks of SECOND NATURAL estimator= (", risk_natural_2, ")\n"
)


### PLOTTING COMPARISON ###
matplot(theta, cbind(risk_umru, risk_umru_dom, risk_natural_1, risk_natural_1_dom, risk_natural_2),
        xlim = c(0.2, 1.2), ylim = c(0, 0.09), xlab = expression(paste("Values of ", theta)), ylab = "Risk Values",
        pch = c(16, 5, 14, 2), col = c("blue3", "green4", "blueviolet", "red", "burlywood3"))
legend(0.85, 0.09, legend = c("UMRU estimator", "dominated UMRU estimator", "first natural estimator",
                             "dominated first natural estimator", "second natural estimator"),
       pch = c(16, 5, 14, 2, 8), col = c("blue3", "green4", "blueviolet", "red", "burlywood3"))
lines(theta, risk_umru, xlim = range(theta), ylim = range(risk_umru), col = "blue3", pch = 16)
lines(theta, risk_umru_dom, xlim = range(theta), ylim = range(risk_umru), col = "green4", pch = 5)
lines(theta, risk_natural_1, xlim = range(theta), ylim = range(risk_umru), col = "blueviolet", pch = 14)
lines(theta, risk_natural_1_dom, xlim = range(theta), ylim = range(risk_umru), col = "red", pch = 2)
lines(theta, risk_natural_2, xlim = range(theta), ylim = range(risk_umru), col = "burlywood3", pch = 8)
