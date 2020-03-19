# Reading the data into memory.
data_set <- read.csv("hw04_data_set.csv")

train_set <- data_set[1:150,]
test_set <- data_set[151:272,]

# Getting the x and y values.
x_train <- train_set$eruptions
y_train <- train_set$waiting

x_test <- test_set$eruptions
y_test <- test_set$waiting

# Getting the number of samples.
N <- length(y_train)

# Setting the data interval, and minimum-maximum values.
minimum_value <- 1.5 # Origin.
maximum_value <- 5.2 # Max value is 5.033 by obervance, preserving that value.
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)
# Setting the bin width and generating right-left border data to proceed with the regressogram.
bin_width <- 0.37
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)
# Learning with regressogram.
p_head <- sapply(1:length(left_borders), 
                 function(b) {
                   num <- sum(y_train[left_borders[b] < x_train & x_train <= right_borders[b]])
                   denom <- sum(left_borders[b] < x_train & x_train <= right_borders[b])
                   return(num/denom)})
# Plotting the generated data.
plot(x_train, y_train, type = "p", pch = 19, 
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "Waiting time to next eruption (min)", xlab = "Eruption time (min)", las = 1, main = sprintf("h = %g", bin_width),col="blue")
for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}
# Plotting the test data on top.
points(x_test, y_test,
       col = "red", pch = 19)
# Generating the legend.
legend("topleft", legend=c("Training", "Test"),
       col=c("blue", "red"), pch= 19, cex=0.7)
# Calculating and printing RMSE for regressogram.
error <- 0
for (i in 1:length(y_test)) {
  ind <- ceiling((x_test[i] - minimum_value)/bin_width)
  error <- error + sum((y_test[i] - p_head[ind])^2)
}
error <- sqrt(error/length(y_test))
sprintf("Regressogram: RMSE is %s when h is %s", error, bin_width)

# Learning with running mean smoother.
p_head <- sapply(data_interval, 
                 function(x) {
                   num <- sum(y_train[(x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width)])
                   denom <- sum((x - 0.5 * bin_width) < x_train & x_train <= (x + 0.5 * bin_width))
                   return(num/denom)})
# Plotting the generated data.
plot(x_train, y_train, type = "p", pch = 19, 
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "Waiting time to next eruption (min)", xlab = "Eruption time (min)", las = 1, main = sprintf("h = %g", bin_width),col="blue")
lines(data_interval, p_head, type = "l", lwd = 2, col = "black")
# Plotting the test data on top.
points(x_test, y_test,
       col = "red", pch = 19)
# Generating the legend.
legend("topleft", legend=c("Training", "Test"),
       col=c("blue", "red"), pch= 19, cex=0.7)
# Calculating and printing RMSE for running mean smoother.
for (b in 1:length(y_test)) {
  ind = ceiling((x_test[b] - minimum_value)/0.01)
  error <- error + sum((y_test[b] - p_head[ind])^2)
}
error <- sqrt(error/length(y_test))
sprintf("Running Mean Smoother: RMSE is %s when h is %s", error, bin_width)

# Learning with kernel smoother.
p_head <- sapply(data_interval, 
                 function(x) {
                   num <- sum(1/sqrt(2*pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2) * y_train)
                   denom <- sum(1/sqrt(2*pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))
                   return(num/denom)})
# Plotting the generated data.
plot(x_train, y_train, type = "p", pch = 19, 
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "Waiting time to next eruption (min)", xlab = "Eruption time (min)", las = 1, main = sprintf("h = %g", bin_width),col="blue")
lines(data_interval, p_head, type = "l", lwd = 2, col = "black")
# Plotting the test data on top.
points(x_test, y_test,
       col = "red", pch = 19)
# Generating the legend.
legend("topleft", legend=c("Training", "Test"),
       col=c("blue", "red"), pch= 19, cex=0.7)
# Calculating and printing RMSE for kernel smoother.
for (b in 1:length(y_test)) {
  ind = ceiling((x_test[b] - minimum_value)/0.01)
  error <- error + sum((y_test[b] - p_head[ind])^2)
}
error <- sqrt(error/length(y_test))
sprintf("Kernel Smoother: RMSE is %s when h is %s", error, bin_width)
