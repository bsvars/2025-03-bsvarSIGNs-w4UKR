set.seed(123)

library(bsvarSIGNs)

# simulate data
B <- matrix(c(1, -1, 1, 1), 2, 2)
invB <- solve(B)
T <- 1000

y <- matrix(0, 2, T)
for (t in 2:T) {
  y[, t] <- invB %*% y[, t - 1] + invB %*% rnorm(2)
}
y <- t(y)

# estimate the model
sign_structural <- B
spec <- specify_bsvarSIGN$new(y, sign_structural = sign_structural)
post <- estimate(spec, S = 1000)

# check the results
apply(post$posterior$B, 1:2, mean)
apply(post$posterior$A, 1:2, mean)
