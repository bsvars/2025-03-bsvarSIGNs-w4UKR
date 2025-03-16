set.seed(2025)

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

##################################################
################# EDIT THIS PART #################
sign_structural <- matrix(NA, 2, 2)
##################################################

# estimate the model
spec <- specify_bsvarSIGN$new(y, sign_structural = sign_structural)
post <- estimate(spec, S = 1000)

# check the result
apply(post$posterior$B, 1:2, mean)
