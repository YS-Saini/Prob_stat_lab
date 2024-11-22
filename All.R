#assignment 5 (Continuous Probability Distributions)
#q1

# Parameters
a <- 0  # Lower bound
b <- 60 # Upper bound

# (a) P(X > 45)
P_X_greater_45 <- 1 - punif(45, min = a, max = b)
P_X_greater_45

# (b) P(20 ≤ X ≤ 30)
P_X_between_20_and_30 <- punif(30, min = a, max = b) - punif(20, min = a, max = b)
P_X_between_20_and_30

#assignment 5 q2
# Parameter
lambda <- 1/2  # Rate parameter

# (a) Density at x = 3
density_at_3 <- dexp(3, rate = lambda)
density_at_3

# (b) Plot exponential PDF for 0 ≤ x ≤ 5
x_vals <- seq(0, 5, by = 0.1)
pdf_vals <- dexp(x_vals, rate = lambda)
plot(x_vals, pdf_vals, type = "l", main = "Exponential PDF", xlab = "x", ylab = "Density")

# (c) P(X ≤ 3)
P_X_less_equal_3 <- pexp(3, rate = lambda)
P_X_less_equal_3

# (d) Plot cumulative probabilities (CDF)
cdf_vals <- pexp(x_vals, rate = lambda)
plot(x_vals, cdf_vals, type = "l", main = "Exponential CDF", xlab = "x", ylab = "Cumulative Probability")

# (e) Simulate 1000 random numbers
simulated_data <- rexp(1000, rate = lambda)
hist(simulated_data, breaks = 30, main = "Simulated Exponential Data", xlab = "x", col = "lightblue")

#assignment 5 q3
# Parameters
alpha <- 2  # Shape parameter
beta <- 1/3 # Rate parameter (1/scale)

# (a.i) P(X = 3) - use the PDF
P_X_equals_3 <- dgamma(3, shape = alpha, rate = beta)
P_X_equals_3

# (a.ii) P(X ≥ 1) = 1 - P(X < 1)
P_X_greater_equal_1 <- 1 - pgamma(1, shape = alpha, rate = beta)
P_X_greater_equal_1

# (b) Find c such that P(X ≤ c) ≥ 0.70
c_value <- qgamma(0.70, shape = alpha, rate = beta)
c_value

#assignment 6  (Joint probability mass and density functions)
install.packages("pracma")
library("pracma")

##Q1. density fn (continuous data) : integration

##1.1  check that it is a joint density function or not? (Use integral2())
f <- function(x,y)(2*(2*x+3*y)/5)
integral2(f,0,1,0,1)

##1.2  find marginal distribution g(x) at x = 1
g <- function(y)f(1,y)
integrate(g,0,1)

##1.3  find the marginal distribution h(y) at y = 0
h <- function(x)f(x,0)
integrate(h,0,1)

##1.4  find the expected value of g(xy) = xy.
t <- function(x,y)x*y*f(x,y)
integral2(t,0,1,0,1)

##Q2 mass fn (discrete data) : summation

##2.1  display the joint mass function in rectangular (matrix) form.
m <- function(x,y)((x+y)/30)
M1 <- matrix(c(m(0:3,0),m(0:3,1),m(0:3,2)),nrow = 4,ncol = 3,byrow=FALSE)
M1

##2.2  check that it is joint mass function or not? (use: Sum())
sum(M1)

##2.3  find the marginal distribution g(x) for x = 0123. (Use:apply())
g_x <- apply(M1,1,sum)  #1 : for row wise
g_x

##2.4  find the marginal distribution h(y) for y = 012. (Use:apply())
h_y <- apply(M1,2,sum)  #2 : for col wise

##2.5  find the conditional probability at x = 0 given y = 1. ie P(x=0|y=1)
#position wise : 1,2,... not from 0 jo R read krega
M1[1,2]/h_y[2]

##2.6  find E(x)E(y)E(xy)Var(x)Var(y)Cov(xy) and its correlation coeficient.
x = c(0:3)
y = c(0:2)

E_x <- sum(x*g_x)
E_x

E_y <- sum(y*h_y)
E_y

l <- function(x,y)x*y*m(x,y)
M2 <- matrix(c(l(0:3,0),l(0:3,1),l(0:3,2)),nrow = 4,ncol = 3,byrow=FALSE)
E_xy <- sum(M2)
E_xy

var_x <- sum(x^2*g_x) - (E_x)^2
var_x

var_y <- sum(y^2*h_y) - (E_y)^2
var_y

cov_xy <- E_xy - E_x*E_y
cov_xy

#assignment 7 (Chi-square, t-distribution, F-distribution)

#ques 1 Investigating the t-distribution and plotting a histogram
# Parameters
n <- 100
df <- n - 1

# Generate random values from t-distribution
t_values <- rt(n, df)

# Plot the histogram
hist(t_values, breaks = 20, main = "Histogram of t-distribution", xlab = "t-values", col = "skyblue", border = "white")

#assgn 7, ques2  Investigating the chi-square distribution
# Parameters
n <- 100
df_values <- c(2, 10, 25)

# Generate and plot histograms for each df
par(mfrow = c(1, 3)) # Set layout for 3 plots in one row
for (df in df_values) {
  chisq_values <- rchisq(n, df)
  hist(chisq_values, breaks = 20, main = paste("Chi-Square (df =", df, ")"), 
       xlab = "Values", col = "lightcoral", border = "white")
}
par(mfrow = c(1, 1)) # Reset layout

# assgn 7, q 3 Generating a vector of values for t-distribution and plotting density 

# Vector of values between -6 and 6
x <- seq(-6, 6, length.out = 100)

# Compute t-distribution densities for different degrees of freedom
df_values <- c(1, 4, 10, 30)
densities <- sapply(df_values, function(df) dt(x, df))

# Plot density for df = 30
plot(x, densities[, 4], type = "l", col = "blue", lwd = 2, 
     main = "Student's t-distribution Density (df = 30)",
     xlab = "x", ylab = "Density")

# Comparison of probability density functions
matplot(x, densities, type = "l", lty = 1, lwd = 2, 
        col = c("red", "green", "blue", "purple"),
        main = "Comparison of t-distribution Densities",
        xlab = "x", ylab = "Density")
legend("topright", legend = paste("df =", df_values), col = c("red", "green", "blue", "purple"), lwd = 2)

#assgn 7, ques 4 (F-distribution calculations and histogram)
# Script for F-Distribution Analysis

# Set parameters for F-distribution
v1 <- 10  # Degrees of freedom 1
v2 <- 20  # Degrees of freedom 2

# (i) Calculate the 95th percentile of the F-distribution
f_95th <- qf(0.95, v1, v2)
cat("The 95th percentile of the F-distribution is:", f_95th, "\n\n")

# (ii) Calculate the area under the curve for specified intervals
# Area under the curve for interval [0, 1.5]
area_0_1.5 <- pf(1.5, v1, v2)
cat("The area under the curve for [0, 1.5] is:", area_0_1.5, "\n")

# Area under the curve for interval [1.5, +∞)
area_1.5_inf <- 1 - area_0_1.5
cat("The area under the curve for [1.5, ∞) is:", area_1.5_inf, "\n\n")

# (iii) Calculate quantiles for given probabilities
q_values <- c(0.25, 0.5, 0.75, 0.999)  # Specified probabilities
quantiles <- qf(q_values, v1, v2)      # Compute quantiles
cat("Quantiles for probabilities", q_values, "are:", quantiles, "\n\n")

# (iv) Generate 1000 random values from F-distribution and plot histogram
set.seed(42)  # Set seed for reproducibility
f_random_values <- rf(1000, v1, v2)  # Generate random values

# Plot histogram of the random values
hist(
  f_random_values, 
  breaks = 30, 
  main = "Histogram of F-distribution (v1=10, v2=20)", 
  xlab = "F-values", 
  col = "lightblue", 
  border = "white"
)

# End of Script

# assignment 8
file_path <- file.choose()
data <- read.csv(file_path)

str(data)

# Validate the data
cat("Number of rows in the dataset:", nrow(data), "\n")
cat("Top 10 rows of the dataset:\n")
print(head(data, 10))

# Population Analysis
population_mean <- mean(data$wall_thickness)  # Assuming the column is 'wall_thickness'
cat("Population Mean:", population_mean, "\n")

# Plot histogram of population data
hist(
  data$wall_thickness,
  breaks = 30,
  main = "Histogram of Wall Thickness",
  xlab = "Wall Thickness",
  col = "lightblue",
  border = "white"
)
abline(v = population_mean, col = "red", lwd = 2)  # Mark the mean
cat("Check the histogram for normal distribution characteristics.\n")

# Sampling Distributions
sample_sizes <- c(10, 50, 500, 9000)

for (size in sample_sizes) {
  sample_means <- replicate(1000, mean(sample(data$wall_thickness, size, replace = TRUE)))
  
  # Plot histogram of sample means
  hist(
    sample_means,
    breaks = 30,
    main = paste("Histogram of Sample Means (Sample Size =", size, ")"),
    xlab = "Sample Means",
    col = "lightgreen",
    border = "white"
  )
  cat("Check the histogram for sample size =", size, "\n")
}

cat("Observation: As sample size increases, the sampling distribution approaches a normal distribution.\n")


