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
cat("Sample from 1st popuation of size", n1, "is: ", sample1, "\n")
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

cat("*_*_*_*_*_*_*_*Results*_*_*_*_*_*_*_* \n",
    "Population", p, "is the selected population.", "\n",
    "Estimate obtained using N_1 estimator: ", est_N_1, "\n",
    "Estimate obtained using N_2 estimator: ", est_N_2, "\n",
    "Estimate obtained using N_3 estimator: ", est_N_3, "\n",
    "Estimate obtained using N_1_I estimator: ", est_N_1_I
)