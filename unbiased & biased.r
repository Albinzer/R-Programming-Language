# Parameters
mu <- 5
sigma <- 2
n <- 30
N_sim <- 1000

# Initialize vectors
sample_vars <- numeric(N_sim)  # Unbiased variances
biased_vars <- numeric(N_sim)  # Biased variances

# Simulation
set.seed(123)  # For reproducibility
for (i in 1:N_sim) {
  data <- rnorm(n, mean = mu, sd = sigma)  # Generate random sample
  sample_vars[i] <- var(data)  # Unbiased variance
  biased_vars[i] <- sum((data - mean(data))^2) / n  # Biased variance
}

# Graphical Output
par(mfrow = c(1, 2))  # Set up a 1x2 grid for plots

# Histogram of unbiased variances
hist(sample_vars, breaks = 30, col = "lightblue", 
     main = "Unbiased Sample Variance", xlab = "Variance", border = "white")
abline(v = sigma^2, col = "red", lwd = 2)  # True variance

# Histogram of biased variances
hist(biased_vars, breaks = 30, col = "lightgreen", 
     main = "Biased Sample Variance", xlab = "Variance", border = "white")
abline(v = sigma^2, col = "red", lwd = 2)  # True variance

# Summary Statistics
cat("Mean of unbiased variances:", mean(sample_vars), "\n")
cat("Mean of biased variances:", mean(biased_vars), "\n")
cat("True variance (sigma^2):", sigma^2, "\n")
