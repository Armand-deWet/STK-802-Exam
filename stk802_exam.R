#install.packages("mixtools")
library(mixtools)
library(ggplot2)

exam_data <- read.csv("q22024.csv")
head(exam_data)

# Fit a mixture of logistic regression models
set.seed(42)  # For reproducibility
mix_model <- logisregmixEM(exam_data$y, exam_data$x, k = 7)

loglik <- mix_model$loglik  # Log-likelihood
n <- nrow(exam_data)  # Number of observations

# Number of parameters
# k-1 mixing proportions + k * (number of predictors + intercept)
k <- length(mix_model$lambda)  # Number of components
p <- (k - 1) + k * (1 + 1)  # Total number of parameters

BIC_7 <- -2 * loglik + p * log(n)
print(BIC_7)

BIC <- c(BIC_2, BIC_3, BIC_4, BIC_5, BIC_6, BIC_7)
number_of_clusters = c(2,3,4,5,6,7)
plot(number_of_clusters, BIC, type = "o")
ggsave('BIC.png', plot = last_plot())

chosen_mix_model <- logisregmixEM(exam_data$y, exam_data$x, k = 2)

chosen_mix_model$loglik

# Print summary of the fitted model
summary(chosen_mix_model)

# Mixing proportions (weights)
chosen_mix_model$lambda

# Coefficients for each component
chosen_mix_model$beta

# Posterior probabilities for each observation
head(chosen_mix_model$posterior)

# Assign each observation to the most probable component
predicted_clusters <- apply(chosen_mix_model$posterior, 1, which.max)

# Add cluster assignments to the exam_data
exam_data$Cluster <- predicted_clusters

# Add cluster assignments to the exam_data
exam_data$Cluster <- predicted_clusters

chosen_mix_model$posterior

ggplot(exam_data, aes(x = x, y = y, color = as.factor(y))) +
  geom_point(size = 2) +
  labs(color = "y") +
  theme_minimal()

logistic <- function(x, beta) {
  1 / (1 + exp(-(beta[1] + beta[2] * x)))
}

# Extract coefficients for each component
coefficients <- chosen_mix_model$beta

# Generate curve data
curve_data <- data.frame()
for (k in 1:ncol(coefficients)) {
  x_vals <- seq(min(exam_data$x), max(exam_data$x), length.out = 100)
  y_vals <- logistic(x_vals, coefficients[, k])
  curve_data <- rbind(curve_data, 
                      data.frame(X1 = x_vals, Prob = y_vals, Component = as.factor(k)))
}

ggplot(exam_data, aes(x = x, y = y, color = as.factor(Cluster))) +
  geom_point(size = 2, alpha = 0.6) +
  geom_line(data = curve_data, aes(x = X1, y = Prob, color = Component), size = 1) +
  labs(color = "Cluster") +
  theme_minimal() +
  ylab("Probability (y = 1)") +
  ggtitle("Mixture of Logistic Regression Curves (K=2)")

ggsave('overlayed_model.png', plot = last_plot())


