# Lab 15: Compare the Power of Different Tests for the Same Hypothesis

# Parameters
mu0 <- 5  # Null hypothesis mean
mu1 <- 6  # Alternative hypothesis mean
sigma <- 2  # Population standard deviation
n <- 30  # Sample size
alpha <- 0.05  # Significance level
N_sim <- 1000  # Number of simulations

# Initialize power counters
power_t_test <- 0
power_z_test <- 0

# Simulation
set.seed(123)
for (i in 1:N_sim) {
  # Simulate data under H1
  data <- rnorm(n, mean = mu1, sd = sigma)
  
  # Perform t-test
  t_test <- t.test(data, mu = mu0, alternative = "greater")
  if (t_test$p.value < alpha) {
    power_t_test <- power_t_test + 1
  }
  
  # Perform z-test
  z_stat <- (mean(data) - mu0) / (sigma / sqrt(n))
  z_critical <- qnorm(1 - alpha)
  if (z_stat > z_critical) {
    power_z_test <- power_z_test + 1
  }
}

# Calculate power estimates
power_t_test <- power_t_test / N_sim
power_z_test <- power_z_test / N_sim

# Output
cat("Power of t-test:", power_t_test, "\n")
cat("Power of z-test:", power_z_test, "\n")

# Graphical Output
par(mfrow = c(1, 2))

# Barplot for Power Comparison
barplot(c(power_t_test, power_z_test), 
        names.arg = c("t-test", "z-test"), 
        col = c("lightblue", "lightgreen"),
        main = "Power Comparison",
        ylab = "Estimated Power")

# Effect of Sample Size on Power
sample_sizes <- seq(10, 100, by = 10)
power_t <- sapply(sample_sizes, function(n) {
  sum(replicate(N_sim, t.test(rnorm(n, mean = mu1, sd = sigma), 
                              mu = mu0, alternative = "greater")$p.value < alpha)) / N_sim
})

# Power vs. Sample Size Plot
plot(sample_sizes, power_t, type = "b", col = "blue", pch = 19, 
     main = "Power vs. Sample Size", xlab = "Sample Size", ylab = "Power")
abline(h = 0.8, col = "red", lty = 2)  # Add power threshold at 0.8
legend("bottomright", legend = c("Power", "80% Threshold"), col = c("blue", "red"), lty = c(1, 2), pch = c(19, NA))
