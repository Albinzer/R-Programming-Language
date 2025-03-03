# Lab 17: Perform Fisher's Exact Test on 2×2 Contingency Tables

# Create a 2x2 contingency table
data <- matrix(c(10, 5, 2, 8), nrow = 2, byrow = TRUE)
rownames(data) <- c("Group A", "Group B")
colnames(data) <- c("Success", "Failure")

# Perform Fisher's exact test
fisher_test_result <- fisher.test(data)

# Output results
cat("2x2 Contingency Table:\n")
print(data)
cat("\nFisher's Exact Test Result:\n")
print(fisher_test_result)

# Graphical Output
par(mfrow = c(1, 2))

# Barplot of the contingency table
barplot(data, beside = TRUE, col = c("lightblue", "lightgreen"), 
        main = "2x2 Contingency Table", xlab = "Outcome", ylab = "Count", 
        legend = rownames(data), args.legend = list(x = "topright"))

# Mosaic plot
mosaicplot(data, main = "Mosaic Plot", color = TRUE)
