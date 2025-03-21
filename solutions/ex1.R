set.seed(2025)

library(bsvarSIGNs)

spec <- specify_bsvarSIGN$new(optimism, p = 4)
post <- estimate(spec, S = 5000)

fit <- compute_fitted_values(post)
plot(fit)

forecast <- forecast(post, horizon = 4)
plot(forecast, data_in_plot = .2)
