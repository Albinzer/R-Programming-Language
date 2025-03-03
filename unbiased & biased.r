mu <- 5
sigma <- 2
n <- 30
N_sim <- 1000

sample_vars <- numeric(N_sim)
biased_vars <- numeric(N_sim)

set.seed(123)
for (i in 1:N_sim) {
  data <- rnorm(n, mean = mu, sd = sigma)
  sample_vars[i] <- var(data)
  biased_vars[i] <- sum((data - mean(data))^2) / n
}

cat("Unbiased Variance Mean:", mean(sample_vars), "SD:", sd(sample_vars), "\n")
cat("Biased Variance Mean:", mean(biased_vars), "SD:", sd(biased_vars), "\n")

dev.new()  # Open a new plot window
par(mfrow = c(1, 2))

hist(sample_vars, breaks = 20, col = "lightblue", 
     main = "Unbiased Sample Variance", xlab = "Variance", border = "white",
     xlim = c(0, 35), ylim = c(0, 150))
abline(v = sigma^2, col = "red", lwd = 2)

hist(biased_vars, breaks = 20, col = "lightgreen", 
     main = "Biased Sample Variance", xlab = "Variance", border = "white",
     xlim = c(0, 35), ylim = c(0, 150))
abline(v = sigma^2, col = "red", lwd = 2)
