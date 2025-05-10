
# Define the Rosenbrock Function and its Gradient
rosenbrock <- function(x, y) {
  (1 - x)^2 + 100 * (y - x^2)^2
}
gradient <- function(x, y) {
  df_dx <- -2 * (1 - x) - 400 * x * (y - x^2)
  df_dy <- 200 * (y - x^2)
  c(df_dx, df_dy)
}

#setup parameters for gradient descent 
x<- -1
y<-1
#step size
eta<-0.001
#threshold for convergence
epsilon<-1e-6

# History storage
trajectory <- data.frame(iter = 0, x = x, y = y, fval = rosenbrock(x, y))

#run gradient descent loop
max_iter<-100000
for ( i in 1:max_iter){
  grad<-gradient(x,y)
  grad_norm<-sqrt(sum(grad^2))
  
  if (grad_norm<epsilon){
    break
  }
  #update variables
  x<-x-eta*grad[1]
  y<-y-eta*grad[2]
  # Store history
  trajectory <- rbind(trajectory, data.frame(iter = i, x = x, y = y, fval = rosenbrock(x, y)))
  
}


# Base R plot for convergence
plot(trajectory$iter, trajectory$fval, type = "l", col = "blue",
     main = "Convergence of Rosenbrock Function",
     xlab = "Iteration", ylab = "Function Value")


#Report final results
cat("Final solution: (", x, ",", y, ")\n")
cat("Final function value:", rosenbrock(x, y), "\n")
cat("Total iterations:", nrow(trajectory) - 1, "\n")




