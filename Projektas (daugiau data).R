rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(fda)
library(fda.usc)
library(fdaoutlier)
library(RColorBrewer)

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
lambda_grid <- 10^seq(-2, 10, length.out = 100)

gcv_mat <- matrix(NA,length(nbasis_grid),length(lambda_grid))

for(i in seq_along(nbasis_grid)){
  
  basis <- create.bspline.basis(range_t, nbasis_grid[i], norder=4)
  
  for(j in seq_along(lambda_grid)){
    
    fdPar_obj <- fdPar(basis, int2Lfd(0), lambda_grid[j])
    
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
#                           GRAPH SPLITTING BY HAND MADE GROUP
###########################################################################################





# group1 <- c(
#   "Gold", "Silver", "US_Dollar_Index", "Japanese_Yen",
#   "Swiss_Franc", "Euro", "10-Year_Treasury_Note","Palladium", "Platinum"
# )
# 
# group2 <- c(
#   "Copper", "Aluminum", "Gasoline",
#   "Crude_Oil", "Natural_Gas", "Wheat", "Corn", "Soybeans",
#   "Coffee", "Sugar", "Cotton"
# )
# 
# all_names <- colnames(scaled_returns)
# 
# idx1 <- match(group1, all_names)
# idx2 <- match(group2, all_names)
# 
# par(mfrow = c(1, 2))
# 
# plot(ret_fd[idx1],
#      main = "Smoothed curves: Group 1",
#      xlab = "Days relative to tariff announcement",
#      ylab = "Scaled log return")
# abline(h = 0, v = 0, lty = 3, col = "grey60")
# 
# plot(ret_fd[idx2],
#      main = "Smoothed curves: Group 2",
#      xlab = "Days relative to tariff announcement",
#      ylab = "Scaled log return")
# abline(h = 0, v = 0, lty = 3, col = "grey60")
# 
# idx1 <- match(group1, all_names)
# idx2 <- match(group2, all_names)
# 
# # evaluate both groups on a common grid to get one shared y-scale
# plot_days <- seq(min(t_rel), max(t_rel), by = 1)
# 
# mat_g1 <- eval.fd(plot_days, ret_fd[idx1])
# mat_g2 <- eval.fd(plot_days, ret_fd[idx2])
# 
# ylim_common <- range(c(mat_g1, mat_g2), na.rm = TRUE)
# 
# par(mfrow = c(1, 2))
# 
# plot(ret_fd[idx1],
#      ylim = ylim_common,
#      main = "Smoothed curves: Group 1 (Safe-haven)",
#      xlab = "Days relative to tariff announcement",
#      ylab = "Scaled log return")
# abline(h = 0, v = 0, lty = 3, col = "grey60")
# 
# plot(ret_fd[idx2],
#      ylim = ylim_common,
#      main = "Smoothed curves: Group 2 (Cyclical)",
#      xlab = "Days relative to tariff announcement",
#      ylab = "Scaled log return")
# abline(h = 0, v = 0, lty = 3, col = "grey60")











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

#Plot all curves and highlight Corn
matplot(tt, lgp, type = "l", lty = 1, col = "grey",
        xlab = "Time",
        ylab = "Standardized log returns")

lines(tt, lgp[,16], col = "red", lwd = 3)    # Corn: MBD outlier
lines(tt, lgp[,17], col = "blue", lwd = 3)   # Soybeans: functional median

legend("topright",
       legend = c("Corn: MBD outlier", "Soybeans: functional median"),
       col = c("red", "blue"),
       lwd = 3,
       cex = 0.8)

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
#PCA1: This component captures the general intensity of the response to tariffs.
#PCA2: This component likely separates commodities reacting in opposite directions.Captures the divergence 
#between commodities benefiting from geopolitical uncertainty and those sensitive to trade restrictions.
#PCA3: This component reflects when commodities reacted. Some commodities react immediately, 
#others adjust several days after the announcement.
#PCA4: Explains little of the total variance and is probably related to short-trem fluctuations and comodity specific noise.

#Rotation
varmx <- varmx.pca.fd(pcalist)
plot(varmx)
#PCA1: mainly reflects pre-announcement vs post-announcement movements - mainly magnitude. From the shape: 
#moderate change before event, stronger movement afterward
#PCA2: reflects differences in the timing of reactions. Some react right when tariffs are announced, some have delayed reaction, or more gradual reaction.
#PCA3: capture commodities most sensitive to trade policy. Like: industrial commodities (copper), energy (oil).
#PCA4: this explains only a small fraction of variation. Small commodity-specific fluctuations, minor delayed reactions.
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

crit2 <- stat_ztwosample$params$critical.value
z2 <- as.numeric(stat_ztwosample$statistics.pointwise[, 1])

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
####??????????????? Two sample permutation test    ????????????????                                ####
########################################################################
#HAND MADE CLUSTERS
stat <- tperm.fd(ret_fd[idx1], ret_fd[idx2])
stat
stat$pval

#pvalue 0.22, not rejected, no diff, graph looks weird, at 0 very low observed statistic

#DOING WITH HIERRARCHICAL CLUSTERING CLUSTERS
stat <- tperm.fd(ret_fd[cluster1_idx], ret_fd[cluster2_idx])
stat
stat$pval

#pvalue 0
