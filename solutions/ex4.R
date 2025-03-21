set.seed(2025)

library(bsvarSIGNs)

# investigate the effects of the optimism shock
data(optimism)

##################################################
################# EDIT THIS PART #################
sign_irf <- matrix(NA, 5, 5)
sign_irf[1, 1] <- 0
sign_irf[2, 1] <- 1
narrative <- specify_narrative(start = 211, sign = -1, shock = 1)
##################################################

# specify the model
spec <- specify_bsvarSIGN$new(optimism * 100,
  p = 4,
  sign_irf = sign_irf,
  sign_narrative = list(narrative)
)

# disable dummy observation priors
spec$prior$Ysoc <- matrix(NA, nrow(spec$prior$Ysoc), 0)
spec$prior$Xsoc <- matrix(NA, nrow(spec$prior$Xsoc), 0)
spec$prior$Ysur <- matrix(NA, nrow(spec$prior$Ysur), 0)
spec$prior$Xsur <- matrix(NA, nrow(spec$prior$Xsur), 0)

# estimate the model
post <- estimate(spec, S = 5000)

# compute and plot impulse responses
irf <- compute_impulse_responses(post, horizon = 40)
rownames(irf) <- colnames(optimism)
plot(irf, probability = 0.68)
