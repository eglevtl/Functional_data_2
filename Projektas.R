library(readxl)
library(dplyr)
library(tidyr)
library(fda)

# 1) Load data ----------------------------
path <- "Yfinance_close_prices.xlsx"
px <- read_excel(path, sheet = "Close_prices") %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

# 2) Convert to matrices -------------------------------------------
Prices <- as.matrix(px %>% dplyr::select(-Date))
colnames(Prices) <- names(px)[-1]

# 3) Transform: log-prices to see percentage change
logP <- log(Prices)

#Use daily returns because they measure market reactions, remove price trends,
#make commodities comparable, and provide better statistical properties for analysis

# log returns: r_t = log(P_t) - log(P_{t-1})
returns <- apply(logP, 2, diff)

dates_return <- px$Date[-1]     # vector of return dates

# standardize returns to compare “safe vs cyclical” on same scale
scaled_returns <- scale(returns)      # mean 0, sd 1 per commodity (column-wise)

#time axis centered on the tariff announcement -----------------
#announcement date: United States President Donald Trump announced a broad package of import duties on April 2, 2025—a date he called "Liberation Day".
event_date <- as.Date("2025-04-02")

#relative time to event: t = 0 on event date, negative before, positive after
t_rel <- as.numeric(dates_return - event_date)
range_t <- range(t_rel)

# 4) GCV for basis and lambda selection:
nbasis_grid <- seq(10,45,5)
lambda_grid <- 10^seq(-2,6,length.out=60)

gcv_mat <- matrix(NA,length(nbasis_grid),length(lambda_grid))

for(i in seq_along(nbasis_grid)){
  
  basis <- create.bspline.basis(range_t, nbasis_grid[i], norder=4)
  
  for(j in seq_along(lambda_grid)){
    
    fdPar_obj <- fdPar(basis, int2Lfd(2), lambda_grid[j])
    
    sm <- smooth.basis(t_rel, scaled_returns, fdPar_obj)
    
    gcv_mat[i,j] <- sum(sm$gcv)
    
  }
}
which(gcv_mat == min(gcv_mat), arr.ind = TRUE)

best_nbasis <- nbasis_grid[3]
best_lambda <- lambda_grid[60]

best_nbasis
best_lambda

nbasis_grid

matplot(log10(lambda_grid), t(gcv_mat),
        type = "l", lty = 1,
        xlab = "log10(lambda)",
        ylab = "GCV",
        main = "GCV curves for different nbasis")
legend("topright",
       legend = paste("nbasis =", nbasis_grid),
       col = 1:length(nbasis_grid),
       lty = 1)

# 5) Smooth into functional data objects ------------------------------

# Use B-splines: 1) non-periodic data, 2) local flexibiloity to shocks, 3) computationally efficient
nbasis <- best_nbasis
norder <- 4  # cubic splines
basis_obj <- create.bspline.basis(rangeval = range_t, nbasis = nbasis, norder = norder)
plot(basis_obj)

# Roughness penalty: penalize curvature (2nd derivative)
Lfd_obj <- int2Lfd(2)

# 6) Smooth standardized returns ----------------------------

fdPar_obj <- fdPar(basis_obj, Lfd_obj, best_lambda)

sm <- smooth.basis(argvals = t_rel, y = scaled_returns, fdParobj = fdPar_obj)
ret_fd <- sm$fd
plot(ret_fd)

ret_fd$fdnames <- list(
  "Days relative to event (t)",
  "Commodity" = colnames(scaled_returns),
  "Std. log return"
)

# 7) Pre/post windows --------------------
pre_days  <- -20:-1
post_days <-  1:20

idx_pre  <- which(t_rel %in% pre_days)
idx_post <- which(t_rel %in% post_days)

# average pre vs post standardized returns
pre_mean  <- colMeans(scaled_returns[idx_pre, , drop = FALSE])
post_mean <- colMeans(scaled_returns[idx_post, , drop = FALSE])
print(data.frame(commodity = names(pre_mean), pre_mean, post_mean))

# 8) Plot of smoothed return functions --------------------------------
plot(ret_fd, lwd = 2,
     xlab = "Days relative to tariff announcement",
     ylab = "Standardized log return (smoothed)")

# Vertical line at event
abline(v = 0, col = "red", lty = 2, lwd = 2)