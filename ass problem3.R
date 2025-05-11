#problem 3
# Define A and b
A <- matrix(c(2, 0, 0, 4), nrow = 2, byrow = TRUE)
b <- c(-4, -8)

# Function to compute f(x)
f <- function(x) {
  t(x) %*% A %*% x + t(b) %*% x
}

# Gradient function
grad_f <- function(x) {
  2 * A %*% x + b
}
# Initial point
x <- c(1, 1)

# Step size
eta <- 0.1

# Convergence threshold
epsilon <- 1e-6

# Storage for trajectory
trajectory <- data.frame(iter = 0, x1 = x[1], x2 = x[2], fval = f(x))
max_iter <- 10000

for (i in 1:max_iter) {
  grad <- grad_f(x)
  grad_norm <- sqrt(sum(grad^2))
  
  if (grad_norm < epsilon) {
    break
  }
  
  # Update
  x <- x - eta * grad
  
  # Save progress
  trajectory <- rbind(trajectory, data.frame(iter = i, x1 = x[1], x2 = x[2], fval = f(x)))
}
cat("Final solution: (", x[1], ",", x[2], ")\n")
cat("Final function value:", f(x), "\n")
cat("Total iterations:", nrow(trajectory) - 1, "\n")
#Optional Plot (base R)
plot(trajectory$iter, trajectory$fval, type = "l", col = "blue",
     main = "Convergence of Quadratic Function",
     xlab = "Iteration", ylab = "Function Value")
