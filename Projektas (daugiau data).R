rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(fda)
library(fda.usc)
library(fdaoutlier)
library(RColorBrewer)
library(refund)
library(ggplot2)
library(gridExtra)
library(reshape2)

#\begin{lstlisting}[caption={R code for functional data preparation}]

#\end{lstlisting}

###########################################################################################
#                                     DATASET PREPARATION
###########################################################################################

# 1) Load data ----------------------------
path <- "Yfinance_close_prices_V2.xlsx"
px <- read_excel(path, sheet = "Close_prices") %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

# remove rows with any missing prices
px <- px %>%
  filter(if_all(-Date, ~ !is.na(.)))

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

tt=1:length(t_rel)
fdataobj_not_normalized<-fdata(t(scaled_returns),tt)
plot(
  fdataobj_not_normalized,
  xlab = "Time",
  ylab = "Return",
  main = ""
)

event_index <- which(t_rel == 0)
abline(v = event_index, col = "red", lwd = 2)

Lfd_obj <- int2Lfd(2)

###########################################################################################
#                                     SMOOTHING
###########################################################################################

############################################
#BASIS AND LAMBDA SELECTION WITH GCV
############################################

# 4) GCV for lambda and basis selection:
nbasis_grid <- seq(7,20,1)
lambda_grid <- 10^seq(-2, 2, length.out = 100)

gcv_mat <- matrix(NA,length(nbasis_grid),length(lambda_grid))

for(i in seq_along(nbasis_grid)){
  
  basis <- create.bspline.basis(range_t, nbasis_grid[i], norder=4)
  
  for(j in seq_along(lambda_grid)){
    
    fdPar_obj <- fdPar(basis, int2Lfd(2), lambda_grid[j])
    
    sm <- smooth.basis(t_rel, scaled_returns, fdPar_obj)
    
    gcv_mat[i,j] <- sum(sm$gcv)
    
  }
}
best_idx    <- which(gcv_mat == min(gcv_mat), arr.ind = TRUE)

best_nbasis <- nbasis_grid[best_idx[1, 1]]
best_lambda <- lambda_grid[best_idx[1, 2]]

best_nbasis
best_lambda

matplot(log10(lambda_grid), t(gcv_mat),
        type = "l", lty = 1,
        xlab = "log10(lambda)",
        ylab = "GCV",
        main = "")
legend("topright",
       legend = paste("nbasis =", nbasis_grid),
       col = 1:length(nbasis_grid),
       lty = 1)


nbasis <- best_nbasis
norder <- 4  # cubic splines
basis_obj <- create.bspline.basis(rangeval = range_t, nbasis = nbasis, norder = norder)
plot(basis_obj)

lambda  <- best_lambda

fdPar_obj <- fdPar(basis_obj, Lfd_obj, lambda) #smoothing settings

sm <- smooth.basis(argvals = t_rel, y = scaled_returns, fdParobj = fdPar_obj)
ret_fd <- sm$fd

ret_fd$fdnames <- list(
  "Days relative to event (t)",
  "Commodity" = colnames(scaled_returns),
  "Scaled log return"
)

plot(ret_fd, lwd = 2,
     main = "",
     xlab = "Days relative to tariff announcement",
     ylab = "Scaled log return (smoothed)")

# Vertical line at event
abline(v = 0, col = "red", lty = 2, lwd = 2)



# 7) Pre/post windows -------------------- (20 days before and 20 days after the event)
pre_days  <- -20:-1
post_days <-  1:20

idx_pre  <- which(t_rel %in% pre_days)
idx_post <- which(t_rel %in% post_days)

# average pre vs post scaled returns
pre_mean  <- colMeans(scaled_returns[idx_pre, , drop = FALSE])
post_mean <- colMeans(scaled_returns[idx_post, , drop = FALSE])
print(data.frame(pre_mean, post_mean))

###########################################################################################
#                             EXPLOARATORY DATA ANALYSIS
###########################################################################################

#elementary pointwise mean and standard deviation

plot(ret_fd,
     xlab = "Days relative to tariff announcement",
     ylab = "Scaled log return (smoothed)")

mean_ret = mean.fd(ret_fd)
stddev_ret = std.fd(ret_fd)

lines(mean_ret, lwd=4, lty=2, col=2)
lines(stddev_ret, lwd=4, lty=2, col=4)

lines(mean_ret-stddev_ret, lwd=4, lty=2, col=6)
lines(mean_ret+stddev_ret, lwd=4, lty=2, col=6)


#Compute the functional variance–covariance surface

var_cov_ret = var.fd(ret_fd)

days        = seq(min(t_rel), max(t_rel),length=60)
var_mat  = eval.bifd(days, days,
                     var_cov_ret)

# Corelation graph 

daytime = seq(min(t_rel), max(t_rel),1)
varmat = eval.bifd(daytime, daytime,
                   var_cov_ret)
rdblue <- colorRampPalette(rev(brewer.pal(11, "RdBu")))

filled.contour(daytime, daytime, varmat,
               color.palette = rdblue,
               xlab = "Day",
               ylab = "Day",
               main = "",
               key.title = title(main = "Cov"))

############################################################
# FUNCTIONAL DEPTH ANALYSIS, OUTLIERS, BOXPLOTS
############################################################

# depth
tt <- t_rel

fdataobj <- fdata(t(eval.fd(tt, ret_fd)), tt)
plot(fdataobj, lwd = 2,
     xlab = "Days relative to tariff announcement",
     ylab = "Standardized log return (smoothed)")

layout(matrix(c(1,1,2,2,
                0,3,3,0), 
              nrow = 2, byrow = TRUE))

par(mar = c(4,4,2,1))
# Fraiman-Muniz depth
out.FM <- depth.FM(fdataobj, trim = 0.1, draw = TRUE)

# Modal depth
out.mode <- depth.mode(fdataobj, trim = 0.1, draw = TRUE)

# Random projection depth
out.RP <- depth.RP(fdataobj, trim = 0.1, draw = TRUE)
layout(1)
par(mfrow = c(1, 1))

################functional depth and outlier detection####################
# functional boxplot
boxplot(ret_fd)

#evaluate functions
lgp <- eval.fd(tt, ret_fd)

#Compute Modified Band Depth scores
mbd <- modified_band_depth(t(lgp))
names(mbd) <- colnames(lgp)
plot(mbd, type = "l")

#Use MBD to detect functional outliers
fbplot_obj <- functional_boxplot(t(lgp), depth_method = "mbd")
fbplot_obj$outliers

#flagged outlier is curve: 16

#MUOD outlier detection
m <- muod(t(lgp), cut_method = "boxplot")
m$outliers

# Plot all smoothed return curves
matplot(tt, lgp, type = "l", lty = 1, col = "grey",
        xlab = "Days relative to tariff announcement",
        ylab = "Standardized log return (smoothed)")

# Highlight functional boxplot outliers
lines(tt, lgp[, 15], col = "red", lwd = 3)       # Wheat: functional boxplot outlier
lines(tt, lgp[, 18], col = "orange", lwd = 3)    # Coffee: functional boxplot outlier

# Highlight functional median
lines(tt, lgp[, 3], col = "blue", lwd = 3)       # Palladium: functional median

# Highlight MUOD magnitude outlier
lines(tt, lgp[, 4], col = "purple", lwd = 3)     # Platinum: MUOD magnitude outlier

legend("topright",
       legend = c("Wheat: functional boxplot outlier",
                  "Coffee: functional boxplot outlier",
                  "Palladium: functional median",
                  "Platinum: MUOD magnitude outlier"),
       col = c("red", "orange", "blue", "purple"),
       lwd = 3,
       cex = 0.8)

data.frame(
  index = seq_len(ncol(lgp)),
  commodity = colnames(lgp)
)

m$outliers$magnitude
colnames(lgp)[m$outliers$magnitude]

#Functional boxplot visualization using MBD
fbplot(lgp, method = "MBD",
       xaxt = "n",
       main = "Functional boxplot using Modified Band Depth",
       xlab = "Days relative to event (t)",
       ylab = "Standardized log returns")

ticks <- c(-20, 0, 20, 40, 60)
tick_pos <- sapply(ticks, function(x) which.min(abs(t_rel - x)))
axis(1,
     at = tick_pos,
     labels = ticks)


###############################################
#PRINCIPAL COMPONENTS ANALYSIS
###############################################

nharm = 4
pcalist = pca.fd(ret_fd, nharm, centerfns = TRUE)
pcalist$varprop
cumsum(pcalist$varprop) #the choice of 4 harmonics is good because the cumulative variation is around 72.6%, which means that 
#most of variation is explained, another harmonic would probably catch mostely noise and only 3 harmonics would
#explain too little of teh total variance.

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(pcalist)
plot(pcalist$harmonics)

#Rotation
varmx <- varmx.pca.fd(pcalist)
plot(varmx)

par(mfrow = c(1, 1))

plot(varmx$harmonics)

###########################################################################################
#                         AMPLITUDE & PHASE DECOMPOSITION
###########################################################################################

# Registration (needed to run AmpPhaseDecomp)
wbasis_CR <- create.bspline.basis(range_t, norder = 3,
                                  breaks = c(range_t[1], 0, range_t[2])) #Creates basis functions that will be used to describe how time gets stretched. The given time range and noted special knot at t = 0 (the announcement day), allowes the warping to behave differently before and after the event.
Wfd0_CR   <- fd(matrix(0, wbasis_CR$nbasis, ncol(scaled_returns)), wbasis_CR) #Creates a starting point for the warping — a flat function, one per commodity, all set to zero.
WfdPar_CR <- fdPar(Wfd0_CR, 1, lambda = 1e-1) #Wraps the warping function in a smoothness constraint. The lambda = 1e-1 controls how flexible the time-warping is allowed to be — higher lambda means more rigid (closer to no warping), lower means more flexible

reg_CR     <- register.fd(y0fd = mean.fd(ret_fd), yfd = ret_fd, WfdParobj = WfdPar_CR) #Takes smoothed commodity curves (ret_fd) and iteratively warps each one's time axis until it aligns as closely as possible to the mean curve (mean.fd(ret_fd)).
ret_fd_reg <- reg_CR$regfd #Extracts the registered curves from the result — these are original commodity curves but with their time axes adjusted so they all align to the mean shape.
warpfd_reg <- reg_CR$warpfd #Extracts the warping functions — one per commodity, describing exactly how that commodity's time axis was stretched or compressed during registration. These are what AmpPhaseDecomp uses to separate timing variation from magnitude variation.

# AmpPhaseDecomp
decomp_interval <- c(range_t[1] + 2, range_t[2] - 2) #trimming because warping functions can behave erratically near the boundaries

amp_phase <- AmpPhaseDecomp(xfd    = ret_fd,
                            yfd    = ret_fd_reg,
                            hfd = warpfd_reg,
                            rng    = decomp_interval)

cat(sprintf("Amplitude MS : %.4f (%.1f%%)\n", amp_phase$MS.amp, 100 * amp_phase$MS.amp / (amp_phase$MS.amp + amp_phase$MS.pha)))
cat(sprintf("Phase MS     : %.4f (%.1f%%)\n", amp_phase$MS.pha, 100 * amp_phase$MS.pha / (amp_phase$MS.amp + amp_phase$MS.pha)))
cat(sprintf("R-squared    : %.4f\n", amp_phase$RSQR))


###########################################################################################
#                         CLUSTERING
###########################################################################################

scores_mat <- pcalist$scores   # 20 commodities x 4 harmonics

# Hierarchical clustering on first 2 FPC scores
dist_mat  <- dist(scores_mat[, 1:2], method = "euclidean")
hc        <- hclust(dist_mat, method = "ward.D2")

plot(hc,
     labels = colnames(scaled_returns),
     main   = "Hierarchical clustering of commodities (FPC1 & FPC2)",
     xlab   = "",
     sub    = "",
     ylab   = "Height")

# Cut into 2 clusters
hc_clusters <- cutree(hc, k = 2)

# Scatter plot
plot(scores_mat[, 1], scores_mat[, 2],
     col  = hc_clusters,
     pch  = 19,
     cex  = 1.4,
     xlab = "FPC1 (reaction intensity)",
     ylab = "FPC2 (direction divergence)",
     main = "Commodities clustered by FPCA scores (hierarchical)")
text(scores_mat[, 1], scores_mat[, 2],
     labels = colnames(scaled_returns),
     pos    = 3, cex = 0.8)
abline(h = 0, v = 0, lty = 3, col = "grey60")

# Cluster membership
cluster_df <- data.frame(
  Commodity = colnames(scaled_returns),
  Cluster   = factor(hc_clusters),
  FPC1      = scores_mat[, 1],
  FPC2      = scores_mat[, 2]
)

split(cluster_df$Commodity, cluster_df$Cluster)

# Indices 
cluster1_idx <- which(hc_clusters == 1)
cluster2_idx <- which(hc_clusters == 2)

plot_days <- seq(min(t_rel), max(t_rel), by = 1)

mat_c1 <- eval.fd(plot_days, ret_fd[cluster1_idx])
mat_c2 <- eval.fd(plot_days, ret_fd[cluster2_idx])

# common y-axis
ylim_common <- range(c(mat_c1, mat_c2), na.rm = TRUE)

par(mfrow = c(1, 2))

plot(ret_fd[cluster1_idx],
     ylim = ylim_common,
     main = "Smoothed curves: Cluster 1",
     xlab = "Days relative to tariff announcement",
     ylab = "Scaled log return")
abline(h = 0, v = 0, lty = 3, col = "grey60")

plot(ret_fd[cluster2_idx],
     ylim = ylim_common,
     main = "Smoothed curves: Cluster 2",
     xlab = "Days relative to tariff announcement",
     ylab = "Scaled log return")
abline(h = 0, v = 0, lty = 3, col = "grey60")

ret_fd[cluster2_idx]$fdnames$Commodity
ret_fd[cluster1_idx]$fdnames$Commodity


########################################################################
#### One sample pointwise-bootstrap-test                            ####
########################################################################

source("items/Zboottest.R")

mu0 <- fd(
  matrix(0,
         nrow = ret_fd$basis$nbasis,
         ncol = 1),
  ret_fd$basis
)

t.seq <- seq(min(t_rel), max(t_rel), by = 1)

#H0: Commodity markets show no abnormal reaction to the tariff announcement 
#(returns are consistent with normal fluctuations).

#H1: There exists at least one time point where returns are significantly 
#different from zero, indicating a market reaction.

stat_all <- Z.boot(x = ret_fd, t.seq = t.seq, mu = mu0, 
  replication = 500, alpha = 0.05)

stat_all$statistics
stat_all$critical.value

# Extract Z-statistics and critical values
z <- as.numeric(stat_all$statistics[, 1])
crit <- as.numeric(stat_all$critical.value)

# Significant time points
sig_idx <- which(abs(z) > crit)

sig_results <- data.frame(
  index = sig_idx,
  time = t.seq[sig_idx],
  date = event_date + t.seq[sig_idx],
  z_statistic = z[sig_idx],
  critical_value = crit[sig_idx],
  direction = ifelse(z[sig_idx] > 0,
                     "Positive abnormal return",
                     "Negative abnormal return")
)

sig_results

if (length(sig_idx) > 0) {
  cat("Reject H0: significant abnormal reaction detected at one or more time points.\n")
} else {
  cat("Fail to reject H0: no significant abnormal reaction detected.\n")
}

sig_ranges <- sig_results %>%
  arrange(time) %>%
  mutate(group = cumsum(c(TRUE, diff(time) != 1))) %>%
  group_by(group, direction) %>%
  summarise(
    start_day = min(time),
    end_day = max(time),
    start_date = min(date),
    end_date = max(date),
    .groups = "drop"
  )

sig_ranges

########################################################################
#### One sample L2_norm_based_test                                  ####
########################################################################

source("items/trace.R")
source("items/L2stat.R")

# Test hypothesis, that the mean functional return is equal to zero
# H0: mu(returns) = 0
# H1: mu(returns) != 0
# 
stat <- L2.stat(x=ret_fd, t.seq = t.seq, mu0=mu0, replication = 500, method = 2)
stat
stat$pvalue

########################################################################
#### One sample F-type-test                                         ####
########################################################################

source("items/trace.R")
source("items/Fstat.R")

# Test hypothesis that the mean functional return is equal to zero
# H0: mu(returns) = 0
# H1: mu(returns) != 0

stat <- F.stat(x=ret_fd, t.seq = t.seq, mu0=mu0, replication = 500, method=2)
stat
stat$pvalue

########################################################################
#### Two sample pointwise-test                                      ####
########################################################################

source("items/Ztwosample.R")

# Test hypothesis, that  Metals and Agricultural Commodities and Currencies and Soft Commodities have the same mean functional response to the tariff event

# H0: mu(cluster 1) = mu(cluster 2)
# H1: mu(cluster 1) != mu(cluster 2)

stat_ztwosample <- Ztwosample(x = ret_fd[cluster1_idx], 
                              y = ret_fd[cluster2_idx], 
                              t.seq = t.seq)

stat_ztwosample

cluster1_idx
cluster2_idx

ret_fd[cluster1_idx]$fdnames$Commodity
ret_fd[cluster2_idx]$fdnames$Commodity


z2 <- as.numeric(stat_ztwosample$statistics.pointwise[, 1])
crit2 <- as.numeric(stat_ztwosample$params$critical.value)

sig_idx_2sample <- which(abs(z2) > crit2)

sig_2sample_results <- data.frame(
  index = sig_idx_2sample,
  time = t.seq[sig_idx_2sample],
  date = event_date + t.seq[sig_idx_2sample],
  z_statistic = z2[sig_idx_2sample],
  critical_value = crit2,
  direction = ifelse(z2[sig_idx_2sample] > 0,
                     "Cluster 1 > Cluster 2",
                     "Cluster 1 < Cluster 2")
)

sig_2sample_results

sig_2sample_ranges <- sig_2sample_results %>%
  arrange(time) %>%
  mutate(group = cumsum(c(TRUE, diff(time) != 1))) %>%
  group_by(group, direction) %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    .groups = "drop"
  )

sig_2sample_ranges

########################################################################
#### Two sample L2-norm-based-test                                  ####
########################################################################

source("items/L2stattwosample.R")

# Test hypothesis, that Metals and Agricultural Commodities and Currencies and Soft Commodities have the same mean functional response (globally)

# H0: mu(cluster 1) = mu(cluster 2)
# H1: mu(cluster 1) != mu(cluster 2)

stat_l2twosample <- L2.stat.twosample(x = ret_fd[cluster1_idx], 
                          y = ret_fd[cluster2_idx], 
                          t.seq = t.seq, 
                          method = 1, 
                          replications = 500)
stat_l2twosample
stat_l2twosample$pvalue

########################################################################
#### Two sample F-type-test                                         ####
########################################################################
source("items/Fstattwosample.R")

stat <- F.stat.twosample(x = ret_fd[cluster1_idx], 
                         y = ret_fd[cluster2_idx], 
                         t.seq = t.seq, 
                         method = 1, 
                         replications = 500)
stat
stat$pvalue

########################################################################
#### Functional regression                                          ####
########################################################################
###########################################################################################

# load data for volatility calculations
path2 <- "Yfinance_close_prices_volatility.xlsx"

px2 <- read_excel(path2, sheet = "Sheet1") %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date)

# Commodity names used in scaled_returns
existing_cols <- intersect(colnames(scaled_returns), colnames(px2))

# Keep only Date + matching commodities
px2 <- px2 %>%
  dplyr::select(Date, all_of(existing_cols))

# compute log returns from original prices
returns_vol <- px2 %>%
  arrange(Date) %>%
  mutate(across(-Date,
                ~ c(NA, diff(log(as.numeric(.))))))

# Remove first NA row
returns_vol <- returns_vol %>%
  slice(-1)

# create date vector
dates_return <- returns_vol$Date

# create return matrix
returns_mat <- as.matrix(returns_vol[, -1])

# define volatility estimation window
vol_window_start <- event_date - 365
vol_window_end   <- event_date - 21

idx_vol <- which(
  dates_return >= vol_window_start &
  dates_return <= vol_window_end
)

cat(sprintf(
  "Volatility window: %s to %s (%d trading days)\n",
  vol_window_start,
  vol_window_end,
  length(idx_vol)
))

################################################################################
# HISTORICAL VOLATILITY
################################################################################

# Historical annualised volatility
hist_vol <- apply(
  returns_mat[idx_vol, , drop = FALSE],
  2,
  sd,
  na.rm = TRUE
) * sqrt(252)

cat("\nHistorical annualised volatility per commodity:\n")
print(round(sort(hist_vol, decreasing = TRUE), 4))

# Standardised volatility
hist_vol_z <- as.numeric(scale(hist_vol))
names(hist_vol_z) <- names(hist_vol)

################################################################################
# SCALAR PREDICTOR DATA FRAME
################################################################################

n_comm <- length(colnames(scaled_returns))

scalar_df <- data.frame(
  commodity = colnames(scaled_returns),
  cluster   = factor(
    hc_clusters,
    levels = c(1, 2),
    labels = c("Metals_Agri", "Currencies_Soft")
  ),
  vol_z     = hist_vol_z[colnames(scaled_returns)],
  stringsAsFactors = FALSE
)

cat("\nScalar predictor data frame:\n")
print(scalar_df)

################################################################################
# EVALUATE FUNCTIONAL RESPONSES ON COMMON GRID
################################################################################

yindex <- seq(min(t_rel), max(t_rel), by = 1)

n_time <- length(yindex)

# Functional data matrix
Y_mat <- t(eval.fd(yindex, ret_fd))

rownames(Y_mat) <- colnames(scaled_returns)

scalar_df$Y <- Y_mat

################################################################################
# PENALISED FUNCTION-ON-SCALAR REGRESSION (pffr)
################################################################################

# Pull scalar predictors as plain vectors for pffr
cluster_dummy <- as.integer(scalar_df$cluster == "Currencies_Soft")
cluster       <- scalar_df$cluster
vol_z         <- scalar_df$vol_z
Y             <- Y_mat

pffr_data <- list(
  Y             = Y,
  cluster       = cluster,
  vol_z         = vol_z,
  cluster_dummy = cluster_dummy
)

# Store cluster_dummy in scalar_df for reuse in Bayes / SoFR sections
scalar_df$cluster_dummy <- cluster_dummy

# Main model
fosr_fit <- pffr(
  Y ~ cluster + vol_z,
  yind = yindex,
  data = pffr_data
)

cat("\nModel summary:\n")
print(summary(fosr_fit))

# Plot coefficient functions
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(
  fosr_fit,
  pages = 1,
  scale = 0,
  main = "pffr: functional coefficient functions"
)

par(mfrow = c(1, 1))

################################################################################
# BAYESIAN FUNCTION-ON-SCALAR REGRESSION
# NOTE: 'non-list contrasts argument ignored' is an internal bug in refund's
# bayes_fosr — suppressWarnings() silences it without affecting results.
################################################################################

# Default VB
cat("Fitting default Bayesian FoSR...\n")

bayes_default <- suppressWarnings(
  bayes_fosr(
    Y ~ cluster_dummy + vol_z,
    data = scalar_df
  )
)

# Explicit basis dimensions
cat("Fitting VB model (Kp=4, Kt=10)...\n")

bayes_VB <- suppressWarnings(
  bayes_fosr(
    Y ~ cluster_dummy + vol_z,
    data = scalar_df,
    Kp = 4,
    Kt = 10
  )
)

# OLS approximation
cat("Fitting OLS approximation...\n")

bayes_OLS <- suppressWarnings(
  bayes_fosr(
    Y ~ cluster_dummy + vol_z,
    data = scalar_df,
    Kt = 10,
    est.method = "OLS"
  )
)

# compare estimated coefficient functions
models_bayes <- list(
  default = bayes_default,
  VB      = bayes_VB,
  OLS     = bayes_OLS
)

intercepts_b <- sapply(models_bayes, function(m) m$beta.hat[1, ])
slopes_clust <- sapply(models_bayes, function(m) m$beta.hat[2, ])
slopes_vol   <- sapply(models_bayes, function(m) m$beta.hat[3, ])

# helper function to convert the coefficient matrices into a long-format
make_plot_df <- function(mat, time_grid) {

  df <- as.data.frame(mat)

  df$time <- time_grid

  reshape2::melt(
    df,
    id.vars = "time",
    variable.name = "method",
    value.name = "beta"
  )
}

# internal grid which defines the time points corresponding to the
# estimated coefficient functions from the Bayesian FoSR model
n_kt <- nrow(intercepts_b)

internal_grid <- seq(
  min(yindex),
  max(yindex),
  length.out = n_kt
)

# plots
p_intercept <- ggplot(
  make_plot_df(intercepts_b, internal_grid),
  aes(x = time, y = beta, color = method)
) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, lty = 2, colour = "grey50") +
  geom_vline(xintercept = 0, lty = 2, colour = "red") +
  labs(
    title = expression(beta[0](t) ~ "– Functional Intercept"),
    x = "Days relative to tariff announcement",
    y = expression(hat(beta)[0](t))
  ) +
  theme_bw()

p_cluster <- ggplot(
  make_plot_df(slopes_clust, internal_grid),
  aes(x = time, y = beta, color = method)
) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, lty = 2, colour = "grey50") +
  geom_vline(xintercept = 0, lty = 2, colour = "red") +
  labs(
    title = expression(beta[1](t) ~ "– Cluster effect (Currencies_Soft dummy)"),
    x = "Days relative to tariff announcement",
    y = expression(hat(beta)[1](t))
  ) +
  theme_bw()

p_vol <- ggplot(
  make_plot_df(slopes_vol, internal_grid),
  aes(x = time, y = beta, color = method)
) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 0, lty = 2, colour = "grey50") +
  geom_vline(xintercept = 0, lty = 2, colour = "red") +
  labs(
    title = expression(beta[2](t) ~ "– Historical volatility effect"),
    x = "Days relative to tariff announcement",
    y = expression(hat(beta)[2](t))
  ) +
  theme_bw()

gridExtra::grid.arrange(
  p_intercept,
  p_cluster,
  p_vol,
  ncol = 1
)

################################################################################
# SCALAR-ON-FUNCTION REGRESSION
################################################################################

post_mean_vec <- colMeans(
  scaled_returns[idx_post, , drop = FALSE]
)

scalar_df$post_mean <- as.numeric(post_mean_vec)

scalar_df$hist_vol_z <- hist_vol_z[colnames(scaled_returns)]

################################################################################
# USE ONLY PRE-EVENT FUNCTIONAL INFORMATION
################################################################################

pre_idx <- which(yindex < 0)

cca_mat_pre <- Y_mat[, pre_idx]

yindex_pre <- yindex[pre_idx]

################################################################################
# PREDICT POST-EVENT RETURN
################################################################################

cat("\nResponse: post-event mean return\n")

sofr_post <- pfr(
  post_mean ~
    lf(
      cca_mat_pre,
      k = min(15, length(yindex_pre) - 1),
      argvals = yindex_pre
    ) +
    cluster_dummy +
    vol_z,
  data = scalar_df
)

cat("\nSummary:\n")
print(summary(sofr_post))

plot(
  sofr_post,
  ylab = expression(hat(beta)(t)),
  xlab = "Days relative to tariff announcement",
  main = "SoFR: pre-event functional predictor"
)

abline(v = 0, col = "red", lty = 2)

################################################################################
# PREDICT HISTORICAL VOLATILITY
################################################################################

cat("\nResponse: historical volatility (standardised)\n")

sofr_vol <- pfr(
  hist_vol_z ~
    lf(
      cca_mat_pre,
      k = min(15, length(yindex_pre) - 1),
      argvals = yindex_pre
    ) +
    cluster_dummy,
  data = scalar_df
)

cat("\nSummary:\n")
print(summary(sofr_vol))

plot(
  sofr_vol,
  ylab = expression(hat(beta)(t)),
  xlab = "Days relative to tariff announcement",
  main = "SoFR: predicting historical volatility"
)

abline(v = 0, col = "red", lty = 2)

################################################################################
# DIAGNOSTICS
################################################################################

vol_table <- scalar_df[, c(
  "commodity",
  "cluster",
  "vol_z"
)]

vol_table$hist_vol_annualised <-
  round(hist_vol[scalar_df$commodity] * 100, 2)

vol_table <- vol_table[
  order(
    vol_table$cluster,
    -vol_table$hist_vol_annualised
  ),
]

cat("\n\n========== VOLATILITY SUMMARY TABLE ==========\n")

print(vol_table)

################################################################################
# BOXPLOT
################################################################################

boxplot(
  hist_vol ~ factor(
    hc_clusters,
    labels = c("Metals & Agri", "Currencies & Soft")
  ),
  ylab = "Annualised historical volatility",
  xlab = "",
  main = "Historical volatility by cluster",
  col  = c("steelblue", "tomato")
)

stripchart(
  hist_vol ~ factor(hc_clusters),
  method = "jitter",
  pch = 19,
  col = "black",
  add = TRUE,
  vertical = TRUE
)

################################################################################
#                                                                              #
#   SENSITIVITY ANALYSIS – EXCLUDING NATURAL GAS                               #
#                                                                              #
#   Natural Gas sits in the Currencies_Soft cluster with vol_z = 3.26,         #
#   an extreme outlier identified by MUOD (magnitude outlier).                 #
#   This script re-runs pffr and SoFR without Natural Gas but keeps the        #
#   original cluster labels, no re-clustering.                                 #
#   Cluster 2 retains 5 members. The goal is to check whether Natural Gas      #
#   drives the main conclusions rather than to rebuild the clustering structure#
#                                                                              #
################################################################################

cat("  SENSITIVITY ANALYSIS: EXCLUDING NATURAL GAS\n")

################################################################################
# 1. SUBSET – drop Natural Gas, keep original cluster labels
################################################################################

ng_name  <- "Natural_Gas"
keep_idx <- which(colnames(scaled_returns) != ng_name)
keep_nms <- colnames(scaled_returns)[keep_idx]

cat(sprintf("Commodities retained: %d  (dropped: %s)\n", length(keep_nms), ng_name))
cat("Note: original cluster labels are preserved — no re-clustering.\n\n")

# Subset functional object and scaled returns
ret_fd_sub     <- ret_fd[keep_idx]
scaled_ret_sub <- scaled_returns[, keep_idx]

# Original cluster assignments minus Natural Gas
orig_clusters_sub <- hc_clusters[keep_idx]

cat("Cluster membership (original labels, Natural Gas excluded):\n")
print(split(keep_nms, factor(orig_clusters_sub,
                              levels = c(1,2),
                              labels = c("Metals_Agri","Currencies_Soft"))))
cat("\n")

################################################################################
# 2. SCALAR PREDICTOR DATA FRAME (sub-sample, original clusters)
################################################################################

hist_vol_sub   <- hist_vol[keep_nms]

# Rescale vol_z without Natural Gas so it has mean 0 / sd 1 in sub-sample
hist_vol_z_sub <- as.numeric(scale(hist_vol_sub))
names(hist_vol_z_sub) <- keep_nms

scalar_df_sub <- data.frame(
  commodity     = keep_nms,
  cluster       = factor(orig_clusters_sub,
                         levels = c(1, 2),
                         labels = c("Metals_Agri", "Currencies_Soft")),
  vol_z         = hist_vol_z_sub[keep_nms],
  stringsAsFactors = FALSE
)

scalar_df_sub$cluster_dummy <- as.integer(
  scalar_df_sub$cluster == "Currencies_Soft"
)
scalar_df_sub$cluster_x_vol <- scalar_df_sub$cluster_dummy *
                                scalar_df_sub$vol_z

cat("Scalar predictor data frame (sub-sample):\n")
print(scalar_df_sub[, c("commodity", "cluster", "vol_z")])
cat("\n")

################################################################################
# 3. FUNCTIONAL RESPONSE MATRIX (sub-sample)
################################################################################

yindex_sub  <- seq(min(t_rel), max(t_rel), by = 1)
Y_mat_sub   <- t(eval.fd(yindex_sub, ret_fd_sub))
rownames(Y_mat_sub) <- keep_nms
scalar_df_sub$Y <- Y_mat_sub

################################################################################
# 4. pffr – FUNCTION-ON-SCALAR REGRESSION (sub-sample)
################################################################################

cat("--- pffr (Natural Gas excluded, original clusters) ---\n\n")

pffr_data_sub <- list(
  Y             = Y_mat_sub,
  cluster       = scalar_df_sub$cluster,
  vol_z         = scalar_df_sub$vol_z,
  cluster_dummy = scalar_df_sub$cluster_dummy
)

# Main model (no interaction — sensitivity analysis confirmed it is unstable)
fosr_sub <- pffr(
  Y ~ cluster + vol_z,
  yind = yindex_sub,
  data = pffr_data_sub
)

cat("Model summary:\n")
print(summary(fosr_sub))

cat(sprintf(
  "pffr R²(adj): full = %.4f  |  sub (no NatGas) = %.4f\n\n",
  summary(fosr_fit)$r.sq,
  summary(fosr_sub)$r.sq
))

# Plot
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
plot(fosr_sub, pages = 1, scale = 0,
     main = "pffr (Natural Gas excluded, original clusters)")
par(mfrow = c(1, 1))

################################################################################
# 5. SoFR – PREDICT POST-EVENT RETURN (sub-sample)
################################################################################

cat("--- SoFR: predicting post-event return (Natural Gas excluded) ---\n\n")

idx_post_sub    <- which(t_rel %in% 1:20)
post_mean_sub   <- colMeans(scaled_ret_sub[idx_post_sub, , drop = FALSE])
scalar_df_sub$post_mean  <- as.numeric(post_mean_sub)
scalar_df_sub$hist_vol_z <- hist_vol_z_sub[keep_nms]

pre_idx_sub     <- which(yindex_sub < 0)
cca_mat_pre_sub <- Y_mat_sub[, pre_idx_sub]
yindex_pre_sub  <- yindex_sub[pre_idx_sub]

# Simplified model: drop cluster_x_vol
sofr_post_sub <- pfr(
  post_mean ~
    lf(cca_mat_pre_sub,
       k       = min(15, length(yindex_pre_sub) - 1),
       argvals = yindex_pre_sub) +
    cluster_dummy +
    vol_z,
  data = scalar_df_sub
)

cat("SoFR summary (post-event return, Natural Gas excluded):\n")
print(summary(sofr_post_sub))

plot(sofr_post_sub,
     ylab = expression(hat(beta)(t)),
     xlab = "Days relative to tariff announcement",
     main = "SoFR – pre-event predictor (Natural Gas excluded)")
abline(v = 0, col = "red", lty = 2)

################################################################################
# 6. COEFFICIENT COMPARISON: full vs sub-sample SoFR
################################################################################

cat("\n--- Parametric coefficient comparison: full vs sub-sample ---\n\n")

param_full <- as.data.frame(summary(sofr_post)$p.table)
param_sub  <- as.data.frame(summary(sofr_post_sub)$p.table)

# Common terms between the two models
common_terms <- intersect(rownames(param_full), rownames(param_sub))

comparison <- data.frame(
  Term      = common_terms,
  Est_full  = round(param_full[common_terms, "Estimate"],  4),
  pval_full = round(param_full[common_terms, "Pr(>|t|)"],  4),
  Est_sub   = round(param_sub[common_terms,  "Estimate"],  4),
  pval_sub  = round(param_sub[common_terms,  "Pr(>|t|)"],  4),
  row.names = NULL
)

print(comparison)

# Flag whether sign and significance are preserved
cat("\nRobustness check (same sign AND p < 0.05 in both?):\n")
for (i in seq_len(nrow(comparison))) {
  sig_full  <- comparison$pval_full[i] < 0.05
  sig_sub   <- comparison$pval_sub[i]  < 0.05
  same_sign <- sign(comparison$Est_full[i]) == sign(comparison$Est_sub[i])
  robust    <- sig_full & sig_sub & same_sign
  cat(sprintf("  %-30s: %s\n",
              comparison$Term[i],
              ifelse(robust,
                     "ROBUST",
                     ifelse(same_sign,
                            "same sign, significance changed",
                            "CHANGED"))))
}

################################################################################
# 7. SUMMARY
################################################################################

cat("  SENSITIVITY SUMMARY\n")

cat(sprintf("pffr  R²: full = %.3f  |  sub (no NatGas) = %.3f\n",
            summary(fosr_fit)$r.sq, summary(fosr_sub)$r.sq))
cat(sprintf("SoFR  R²: full = %.3f  |  sub (no NatGas) = %.3f\n",
            summary(sofr_post)$r.sq, summary(sofr_post_sub)$r.sq))
cat("Clustering: original labels retained — no reassignment.\n")
cat(sprintf("Cluster sizes: Metals_Agri = %d, Currencies_Soft = %d\n",
            sum(orig_clusters_sub == 1),
            sum(orig_clusters_sub == 2)))
