#problem 4

# Load the data from CSV file (replace with the path or URL of your file)
# Assuming the data is a single column of numbers (x1, x2, ..., xn)
data <- read.csv("D:/synthetic data - problem-4.csv")$x
# Parameters
mu <- 0            # Initial value of μ
sigma <- 1         # Initial value of σ
eta <- 0.01        # Step size
epsilon <- 1e-5    # Convergence threshold
max_iter <- 10000  # Maximum number of iterations

# Negative Log-Likelihood function for Normal Distribution
nll <- function(mu, sigma, data) {
  n <- length(data)
  log_likelihood <- n * log(sigma) + sum((data - mu)^2) / (2 * sigma^2)
  return(log_likelihood)
}

# Gradient of the Negative Log-Likelihood
grad_mu <- function(mu, sigma, data) {
  return(-sum(data - mu) / sigma^2)  # Gradient w.r.t. mu
}

grad_sigma <- function(mu, sigma, data) {
  n <- length(data)
  return(n / sigma - sum((data - mu)^2) / sigma^3)  # Gradient w.r.t. sigma
}

# Store the trajectory of the parameters for later visualization
trajectory <- data.frame(iter = 0, mu = mu, sigma = sigma, nll_value = nll(mu, sigma, data))

# Run Gradient Descent
for (i in 1:max_iter) {
  grad_mu_val <- grad_mu(mu, sigma, data)
  grad_sigma_val <- grad_sigma(mu, sigma, data)
  
  # Update parameters
  mu <- mu - eta * grad_mu_val
  sigma <- sigma - eta * grad_sigma_val
  
  # Check convergence: if the gradients are very small, stop the algorithm
  grad_norm <- sqrt(grad_mu_val^2 + grad_sigma_val^2)
  if (grad_norm < epsilon) {
    break
  }
  
  # Store the results for plotting
  trajectory <- rbind(trajectory, data.frame(iter = i, mu = mu, sigma = sigma, nll_value = nll(mu, sigma, data)))
}

# Plot the convergence of the negative log-likelihood
plot(trajectory$iter, trajectory$nll_value, type = "l", col = "blue", 
     main = "Convergence of Negative Log-Likelihood", 
     xlab = "Iteration", ylab = "Negative Log-Likelihood")

# Print final results
cat("Final solution: μ =", mu, ", σ =", sigma, "\n")
cat("Final Negative Log-Likelihood:", nll(mu, sigma, data), "\n")
cat("Total iterations:", nrow(trajectory) - 1, "\n")


