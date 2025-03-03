# Define the two groups
group1 <- c(85, 78, 92, 88, 76)
group2 <- c(80, 82, 91, 77, 89, 93, 86)

# Perform the Wilcoxon Rank-Sum Test
wilcox_test_result <- wilcox.test(group1, group2, exact = FALSE)

# Print the results
print(wilcox_test_result)

# Graphical Output: Boxplot to Compare Groups
par(mfrow = c(1, 2))  # Set the plot area to 1 row and 2 columns

# Boxplot to compare the two groups
boxplot(group1, group2, names = c("Group 1", "Group 2"), 
        col = c("lightblue", "lightgreen"),
        main = "Boxplot of Group 1 and Group 2", 
        ylab = "Values")

# Add a legend
legend("topright", legend = c("Group 1", "Group 2"), fill = c("lightblue", "lightgreen"))

# Density plot to compare the distributions of the groups
plot(density(group1), col = "blue", lwd = 2, main = "Density Plot of Groups", xlab = "Values", ylim = c(0, 0.05))
lines(density(group2), col = "green", lwd = 2)
legend("topright", legend = c("Group 1", "Group 2"), col = c("blue", "green"), lwd = 2)
