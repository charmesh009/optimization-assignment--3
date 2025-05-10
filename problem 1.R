# question 1
# Set seed for reproducibility
set.seed(42)

# Generate synthetic data
n <- 100
x <- rnorm(n, mean = 1, sd = sqrt(2))
y <- rnorm(n, mean = 2 + 3 * x, sd = sqrt(5))

# Design matrix with intercept
X <- cbind(1, x)

# Loss function: Mean Squared Error (with 1/2n factor)
linear_loss <- function(beta, X, y) {
  n <- nrow(X)
  residuals <- y - X %*% beta
  return((1 / (2 * n)) * sum(residuals^2))
}

# Gradient of the loss function
linear_grad <- function(beta, X, y) {
  n <- nrow(X)
  grad <- -t(X) %*% (y - X %*% beta) / n
  return(grad)
}

# Initialization
beta <- c(0, 0)      # Initial β0
eta <- 0.01          # Step size
epsilon <- 1e-6      # Convergence threshold
max_iter <- 10000    # Safety cap
loss_history <- numeric()

# Gradient Descent Loop
for (iter in 1:max_iter) {
  grad <- linear_grad(beta, X, y)
  beta_new <- beta - eta * grad
  loss_val <- linear_loss(beta_new, X, y)
  loss_history <- c(loss_history, loss_val)
  
  if (sqrt(sum((beta_new - beta)^2)) < epsilon) {
    cat("Converged at iteration", iter, "\n")
    break
  }
  
  beta <- beta_new
}

# Final results
cat("Final beta:", beta, "\n")
cat("Final loss:", tail(loss_history, 1), "\n")
cat("Total iterations:", length(loss_history), "\n")

# Plot convergence
plot(loss_history, type = "l", col = "darkgreen",
     xlab = "Iteration", ylab = "Loss",
     main = "Convergence of Linear Regression Loss")
