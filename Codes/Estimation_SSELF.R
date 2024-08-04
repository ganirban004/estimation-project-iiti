### CREATING THE SAMPLES ###

# SAMPLE 1
sample1 <- c()
n1 <- as.integer(readline("Enter the Size of Sample from 1st Population: "))
print("Enter 1st Sample:")
for ( i in 1:n1)
{
  x <- as.numeric(readline())
  sample1 <- append(sample1, x)
}

# SAMPLE 2
sample2 <- c()
n2 <- as.integer(readline("Enter the Size of Sample from 2nd Population: "))
print("Enter 2nd Sample:")
for ( i in 1:n2)
{
  x <- as.numeric(readline())
  sample2 <- append(sample2, x)
}

# SHOW ENTERED SAMPLE
cat("Sample from 1st population of size", n1, "is: ", sample1, "\n")
cat("Sample from 2nd population of size", n2, "is: ", sample2, "\n")


### CALCULATION OF a* ###

if (n1<=n2)
{
  a_star <- (((n1+n2)/(2*n2))^(1/n1))
} else
{
  a_star <- (((2*n1)/(n1+n2))^(1/n2))
}


### CALCULATION OF VARIOUS ESTIMATES ###

X1 <- max(sample1)
X2 <- max(sample2)

if (X1> (a_star*X2))
{
  p <- 1
  
  # UMVU ESTIMATOR  #
  est_umvu <- ((X1/n1)*((n1+1)-((a_star*X2/X1)^n1)))
  
  # DOMINATED UMVU ESTIMATOR #
  phi_star <- ((n1+n2+2)/(n1+n2+1))
  psi_U <- (((n1+1)-((a_star*X2/X1)^n1))/n1)
  
  # FIRST NATURAL ESTIMATOR #
  est_natural_1 <- X1
  
  # SECOND NATURAL ESTIMATOR #
  est_natural_2 <- ((n1+1)*X1/n1)
  
  # THIRD NATURAL ESTIMATOR #
  est_natural_3 <- ((n1+2)*X1/(n1+1))
  
} else
{
  p <- 2
  
  # UMVU ESTIMATOR  #
  est_umvu <- ((X2/n2)*((n2+1)-((X1/(X2*a_star))^n2)))
  
  # DOMINATED UMVU ESTIMATOR #
  phi_star <- ((n1+n2+2)*(X2/X1)/(n1+n2+1))
  psi_U <- ((X2/(X1*n2))*((n2+1)-((X1/(a_star*X2))^n2)))
  
  # FIRST NATURAL ESTIMATOR #
  est_natural_1 <- X2
  
  # SECOND NATURAL ESTIMATOR #
  est_natural_2 <- ((n2+1)*X2/n2)
  
  # THIRD NATURAL ESTIMATOR #
  est_natural_3 <- ((n2+2)*X2/(n2+1))
  
}

est_umvu_dom <- (X1 * (max(phi_star, psi_U)))
est_natural_1_dom <- ((n1+n2+2)*est_natural_1/(n1+n2+1))


### RESULTS ###

cat("*_*_*_*_*_*_*_*Results*_*_*_*_*_*_*_* \n",
  "Population", p, "is the selected population.", "\n",
  "Estimate obtained using UMVU estimator: ", est_umvu, "\n",
  "Estimate obtained using DOMINATED UMVU estimator: ", est_umvu_dom, "\n",
  "Estimate obtained using FIRST NATURAL estimator: ", est_natural_1, "\n",
  "Estimate obtained using DOMINATED FIRST NATURAL estimator: ", est_natural_1_dom, "\n",
  "Estimate obtained using SECOND NATURAL estimator: ", est_natural_2, "\n",
  "Estimate obtained using THIRD NATURAL estimator:", est_natural_3
  )