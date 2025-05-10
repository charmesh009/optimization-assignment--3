# Define sigmoid function
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# Define negative log-likelihood (NLL)
neg_log_likelihood <- function(beta, X, y) {
  z <- y * (X %*% beta)
  sum(log(1 + exp(-z)))
}

# Define gradient of the negative log-likelihood
neg_log_likelihood_grad <- function(beta, X, y) {
  preds <- sigmoid(X %*% beta)  # Predicted probabilities
  grad <- t(X) %*% (preds - y)  # Gradient computation
  return(grad)
}

# Load data
data <- read.csv("D:/synthetic data - problem-2.csv")
X <- as.matrix(cbind(1, data$x1, data$x2))  # Intercept, x1, x2
y <- data$y

# Initialize parameters
beta <- c(0, 0, 0)  # Initial beta values
eta <- 0.05         # Learning rate
epsilon <- 1e-5     # Convergence threshold

# Initialize loss history
loss_history <- c()

# Gradient Descent Loop
for (iter in 1:100000) {
  loss <- neg_log_likelihood(beta, X, y)        # Compute current loss
  loss_history <- c(loss_history, loss)         # Store it
  
  grad <- neg_log_likelihood_grad(beta, X, y)   # Compute gradient
  beta_new <- beta - eta * grad                 # Update beta
  
  if (sqrt(sum((beta_new - beta)^2)) < epsilon) {
    cat("Converged at iteration", iter, "\n")
    break
  }
  beta <- beta_new
}

# Final beta values
cat("Final beta:", beta, "\n")

# Plot convergence
plot(loss_history, type = "l", col = "blue",
     main = "Convergence of Negative Log-Likelihood",
     xlab = "Iteration", ylab = "Loss")

# Predict probabilities for training data
predict_proba <- function(X, beta) {
  sigmoid(X %*% beta)
}

# Example output
probs <- predict_proba(X, beta)
head(probs)

