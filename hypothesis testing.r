# Lab 14: Hypothesis Testing Step-by-Step

# Parameters
mu0 <- 5  # Null hypothesis mean
mu1 <- 6  # True population mean
sigma <- 2  # Population standard deviation
n <- 30  # Sample size
alpha <- 0.05  # Significance level

# Generate sample data
set.seed(123)
sample_data <- rnorm(n, mean = mu1, sd = sigma)

# Step 1: State hypotheses
cat("H0: mu =", mu0, "\n")
cat("H1: mu >", mu0, "\n")

# Step 2: Choose significance level
cat("Significance level (alpha):", alpha, "\n")

# Step 3: Calculate test statistic
t_stat <- (mean(sample_data) - mu0) / (sd(sample_data) / sqrt(n))
cat("Test Statistic (t):", t_stat, "\n")

# Step 4: Determine critical value and p-value
critical_value <- qt(1 - alpha, df = n - 1)
p_value <- pt(t_stat, df = n - 1, lower.tail = FALSE)

cat("Critical Value:", critical_value, "\n")
cat("P-value:", p_value, "\n")

# Step 5: Make a decision
if (t_stat > critical_value) {
  decision <- "Reject H0"
} else {
  decision <- "Fail to reject H0"
}
cat("Decision:", decision, "\n")

# Graphical Output
par(mfrow = c(1, 2))

# Histogram with critical region
hist(sample_data, breaks = 15, col = "lightblue", main = "Sample Data", xlab = "Value", border = "white", probability = TRUE)
abline(v = mu0, col = "red", lwd = 2, lty = 2)  # Null hypothesis mean
abline(v = mean(sample_data), col = "blue", lwd = 2)  # Sample mean
legend("topright", legend = c("H0 Mean", "Sample Mean"), col = c("red", "blue"), lwd = 2, lty = c(2, 1))

# Density plot with critical region
plot(density(sample_data), col = "blue", lwd = 2, main = "Density Plot", xlab = "Value")
abline(v = critical_value, col = "red", lty = 2)  # Critical Value

# Rejection region shading
x_vals <- seq(min(sample_data) - 1, max(sample_data) + 1, length.out = 100)
curve(dnorm(x_vals, mean = mu0, sd = sigma / sqrt(n)), col = "blue", lwd = 2, add = TRUE)
polygon(x_vals[x_vals > critical_value], dnorm(x_vals[x_vals > critical_value], mean = mu0, sd = sigma / sqrt(n)), col = rgb(1, 0, 0, 0.3), border = NA)

