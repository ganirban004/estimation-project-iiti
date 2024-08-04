### KOLMOGROV SMIRNOV GOODNESS OF FIT TEST ###

# SOURCE
#Fundamentals of Statistics, Volume 1, Gun, Gupta, Dasgupta, 2017, Page- 410

# SAMPLES
batch1 <- c(1505, 1556, 1801, 1629, 1644, 1607, 1825, 1748)
sample1 <- batch1 - min(batch1)

batch2 <- c(1799, 1618, 1604, 1655, 1708, 1675, 1728)
sample2 <- batch2 - min(batch2)

ks.test(sample1, "punif", 0, max(sample1))
ks.test(sample2, "punif", 0, max(sample2))

cat("Shifted sample from population 1 is fitted to U(0,", max(sample1), ")\n")
cat("Shifted sample from population 2 is fitted to U(0,", max(sample2), ")\n")


### RESIZING THE SAMPLES ###

sample1 <- sample1[sample1 != 0]
sample2 <- sample2[sample2 != 0]
n1 <- as.integer(readline("Enter the Size of Sample from 1st Population: "))
n2 <- as.integer(readline("Enter the Size of Sample from 2nd Population: "))
sample1 <- sample1[1:n1]
sample2 <- sample2[1:n2]


### CALCULATION OF a* ###

if (n1<=n2)
{
  a_star <- (((n1+n2)/(2*n2))^(1/n1))
} else
{
  a_star <- (((2*n1)/(n1+n2))^(1/n2))
}


### CALCULATION OF VARIOUS ESTIMATES UNDER ELF ###

X1 <- max(sample1)
X2 <- max(sample2)

if (X1> (a_star*X2))
{
  p <- 1
  
  # UMRU ESTIMATOR  #
  est_umru <- ((n1*X1)/((n1-1)+((a_star*X2/X1)^n1)))
  
  # DOMINATED UMRU ESTIMATOR #
  phi_star <- ((n1+n2)/(n1+n2-1))
  psi_U <- (n1/((n1-1)+((a_star*X2/X1)^n1)))
  
  # FIRST NATURAL ESTIMATOR #
  est_natural_1 <- X1
  
  # SECOND NATURAL ESTIMATOR #
  est_natural_2 <- (n1*X1/(n1-1))
  
  
} else
{
  p <- 2
  
  # UMRU ESTIMATOR  #
  est_umru <- ((n2*X2)/((n2-1)+((X1/(X2*a_star))^n2)))
  
  # DOMINATED UMRU ESTIMATOR #
  phi_star <- ((n1+n2)*(X2/X1)/(n1+n2-1))
  psi_U <- (n2*X2/X1)/((n2-1)+((X1/(a_star*X2))^n2))
  
  # FIRST NATURAL ESTIMATOR #
  est_natural_1 <- X2
  
  # SECOND NATURAL ESTIMATOR #
  est_natural_2 <- (n2*X2/(n2-1))
  
}

est_umru_dom <- (X1 * (max(phi_star, psi_U)))
est_natural_1_dom <- ((n1+n2)*est_natural_1/(n1+n2-1))


### RESULTS UNDER ELF ###

cat("*_*_*_*_*_*Results of Estimation under Entropy Loss Function*_*_*_*_*_* \n",
    "Population", p, "is the selected population.", "\n",
    "Estimate obtained using UMRU estimator: ", est_umru, "\n",
    "Estimate obtained using DOMINATED UMRU estimator: ", est_umru_dom, "\n",
    "Estimate obtained using FIRST NATURAL estimator: ", est_natural_1, "\n",
    "Estimate obtained using DOMINATED FIRST NATURAL estimator: ", est_natural_1_dom, "\n",
    "Estimate obtained using SECOND NATURAL estimator: ", est_natural_2, "\n"
)



### CALCULATION OF VARIOUS ESTIMATES UNDER ALF ###

X1 <- max(sample1)
X2 <- max(sample2)

if (X1> (a_star*X2))
{
  p <- 1
  
  # N_1 ESTIMATOR  #
  est_N_1 <- X1
  
  # N_2 ESTIMATOR #
  est_N_2 <- ((n1+1)*X1/n1)
  
  # N_3 ESTIMATOR #
  est_N_3 <- ((((n1+1)/(n1-1))^(1/2))*X1)
  
} else
{
  p <- 2
  
  # N_1 ESTIMATOR  #
  est_N_1 <- X2
  
  # N_2 ESTIMATOR #
  est_N_2 <- ((n2+1)*X2/n2)
  
  # N_3 ESTIMATOR #
  est_N_3 <- ((((n2+1)/(n2-1))^(1/2))*X2)
  
}

est_N_1_I <- (((n1+n2+1)/(n1+n2-1))^(1/2))*est_N_1


### RESULTS ###

cat("*_*_*_*_*_*Results of Estimation under Asymmetric Loss Function*_*_*_*_*_* \n",
    "Population", p, "is the selected population.", "\n",
    "Estimate obtained using N_1 estimator: ", est_N_1, "\n",
    "Estimate obtained using N_2 estimator: ", est_N_2, "\n",
    "Estimate obtained using N_3 estimator: ", est_N_3, "\n",
    "Estimate obtained using N_1_I estimator: ", est_N_1_I, "\n"
)


### CALCULATION OF VARIOUS ESTIMATES UNDER SSELF ###

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

cat("*_*_*_*_*_*Results of Estimation under Scaled-Squared Error Loss Function*_*_*_*_*_* \n",
    "Population", p, "is the selected population.", "\n",
    "Estimate obtained using UMVU estimator: ", est_umvu, "\n",
    "Estimate obtained using DOMINATED UMVU estimator: ", est_umvu_dom, "\n",
    "Estimate obtained using FIRST NATURAL estimator: ", est_natural_1, "\n",
    "Estimate obtained using DOMINATED FIRST NATURAL estimator: ", est_natural_1_dom, "\n",
    "Estimate obtained using SECOND NATURAL estimator: ", est_natural_2, "\n",
    "Estimate obtained using THIRD NATURAL estimator:", est_natural_3
)