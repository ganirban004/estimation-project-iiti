set.seed(52)


### CREATING THE SAMPLES ###

# CHARACTERISTICS OF SAMPLES
n1 <- 5
n2 <- 2
theta1 <- 10

theta <- c()
risk_umvu <- c()
risk_umvu_dom <- c()
risk_natural_1 <- c()
risk_natural_1_dom <- c()
risk_natural_2 <- c()
risk_natural_3 <- c()

for (j in 2:20)
{
  theta2 <- j
  theta[j] <- (theta2/theta1)
  
  loss_umvu <- c()
  loss_umvu_dom <- c()
  loss_natural_1 <- c()
  loss_natural_1_dom <- c()
  loss_natural_2 <- c()
  loss_natural_3 <- c()
  
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
      est_umvu <- ((X1/n1)*((n1+1)-((a_star*X2/X1)^n1)))
      phi_star <- ((n1+n2+2)/(n1+n2+1))
      psi_U <- (((n1+1)-((a_star*X2/X1)^n1))/n1)
      est_natural_1 <- X1
      est_natural_2 <- ((n1+1)*X1/n1)
      est_natural_3 <- ((n1+2)*X1/(n1+1))
    } else
    {
      theta_selected <- theta2
      est_umvu <- ((X2/n2)*((n2+1)-((X1/(X2*a_star))^n2)))
      phi_star <- ((n1+n2+2)*(X2/X1)/(n1+n2+1))
      psi_U <- ((X2/(X1*n2))*((n2+1)-((X1/(a_star*X2))^n2)))
      est_natural_1 <- X2
      est_natural_2 <- ((n2+1)*X2/n2)
      est_natural_3 <- ((n2+2)*X2/(n2+1))
    }
    
    est_umvu_dom <- (X1 * (max(phi_star, psi_U)))
    est_natural_1_dom <- ((n1+n2+2)*est_natural_1/(n1+n2+1))
    
    
    ### CALCULATING LOSSES OF VARIOUS ESTIMATORS ###
    loss_umvu[i] <- (((est_umvu/theta_selected)-1)^2)
    loss_umvu_dom[i] <- (((est_umvu_dom/theta_selected)-1)^2)
    loss_natural_1[i] <- (((est_natural_1/theta_selected)-1)^2)
    loss_natural_1_dom[i] <- (((est_natural_1_dom/theta_selected)-1)^2)
    loss_natural_2[i] <- (((est_natural_2/theta_selected)-1)^2)
    loss_natural_3[i] <- (((est_natural_3/theta_selected)-1)^2)
  }
  
  
  ### CALCULATING RISKS ###
  risk_umvu[j] <- mean(loss_umvu)
  risk_umvu_dom[j] <- mean(loss_umvu_dom)
  risk_natural_1[j] <- mean(loss_natural_1)
  risk_natural_1_dom[j] <- mean(loss_natural_1_dom)
  risk_natural_2[j] <- mean(loss_natural_2)
  risk_natural_3[j] <- mean(loss_natural_3)
}

theta <- theta[-1]
risk_umvu <- risk_umvu[-1]
risk_umvu_dom <- risk_umvu_dom[-1]
risk_natural_1 <- risk_natural_1[-1]
risk_natural_1_dom <- risk_natural_1_dom[-1]
risk_natural_2 <- risk_natural_2[-1]
risk_natural_3 <- risk_natural_3[-1]

cat("\n",
    "theta= (", theta, ")\n",
    "Risks of UMVU estimator= (", risk_umvu, ")\n",
    "Risks of DOMINATED UMVU estimator= (", risk_umvu_dom, ")\n",
    "Risks of FIRST NATURAL estimator= (", risk_natural_1, ")\n",
    "Risks of DOMINATED FIRST NATURAL estimator= (", risk_natural_1_dom, ")\n",
    "Risks of SECOND NATURAL estimator= (", risk_natural_2, ")\n",
    "Risks of THIRD NATURAL estimator=(", risk_natural_3,")\n"
)


### PLOTTING COMPARISON ###
matplot(theta, cbind(risk_umvu, risk_umvu_dom, risk_natural_1, risk_natural_1_dom, risk_natural_2, risk_natural_3),
        xlim = c(0.2, 2), ylim = c(0, 0.1), xlab = expression(paste("Values of ", theta)), ylab = "Risk Values",
        pch = c(16, 5, 14, 2, 8, 4), col = c("blue3", "green4", "blueviolet", "red", "burlywood3", "chocolate2"))
legend(0.5, 0.1, legend = c("UMVU estimator", "dominated UMVU estimator", "first natural estimator",
                            "dominated first natural estimator", "second natural estimator", "third natural estimator"),
       pch = c(16, 5, 14, 2, 8, 4), col = c("blue3", "green4", "blueviolet", "red", "burlywood3", "chocolate2"))
lines(theta, risk_umvu, xlim = range(theta), ylim = range(risk_umvu), col = "blue3", pch = 16)
lines(theta, risk_umvu_dom, xlim = range(theta), ylim = range(risk_umvu), col = "green4", pch = 5)
lines(theta, risk_natural_1, xlim = range(theta), ylim = range(risk_umvu), col = "blueviolet", pch = 14)
lines(theta, risk_natural_1_dom, xlim = range(theta), ylim = range(risk_umvu), col = "red", pch = 2)
lines(theta, risk_natural_2, xlim = range(theta), ylim = range(risk_umvu), col = "burlywood3", pch = 8)
lines(theta, risk_natural_3, xlim = range(theta), ylim = range(risk_umvu), col = "chocolate2", pch = 4)