rm(list=ls())
library(readxl)
library(dplyr)
library(tidyr)
library(fda)
library(fda.usc)
library(fdaoutlier)

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


Lfd_obj <- int2Lfd(0)

###########################################################################################
#                                     SMOOTHING
###########################################################################################

############################################
#BASIS AND LAMBDA SELECTION WITH GCV
############################################

# 4) GCV for lambda and basis selection:
nbasis_grid <- seq(6,21,1)
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

lims <- quantile(varmat, c(0.02, 0.98), na.rm = TRUE)
varmat_clip <- pmin(pmax(varmat, lims[1]), lims[2])

filled.contour(daytime, daytime, varmat_clip,
               color.palette = terrain.colors,
               xlab = "Day",
               ylab = "Day")

############################################################
# FUNCTIONAL DEPTH ANALYSIS, OUTLIERS, BOXPLOTS
############################################################

# depth
tt <- t_rel

fdataobj <- fdata(t(eval.fd(tt, ret_fd)), tt)
plot(fdataobj, lwd = 2,
     xlab = "Days relative to tariff announcement",
     ylab = "Standardized log return (smoothed)")

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
# Fraiman-Muniz depth
out.FM <- depth.FM(fdataobj, trim = 0.1, draw = TRUE)

# Modal depth
out.mode <- depth.mode(fdataobj, trim = 0.1, draw = TRUE)

# Random projection depth
out.RP <- depth.RP(fdataobj, trim = 0.1, draw = TRUE)
par(mfrow = c(1, 1))

# functional boxplot
boxplot(ret_fd)

# evaluate functions
lgp <- eval.fd(tt, ret_fd)

# band depth
bd <- band_depth(t(lgp))
names(bd) <- colnames(lgp)
plot(bd, type = "l")

# modified band depth
mbd <- modified_band_depth(t(lgp))
names(mbd) <- colnames(lgp)
plot(mbd, type = "l")

#Curve 8 is the most central function
#The other curves are less central but very similar to each other. 
#So the data appears very symmetric, with one curve clearly sitting in the middle of the band.

# functional boxplot + outliers
fbplot_obj <- functional_boxplot(t(lgp), depth_method = "mbd")
fbplot_obj$outliers

#flagged outliers are curves: 5, 6, 7, 9

# MUOD outlier detection
m <- muod(t(lgp), cut_method = "boxplot")
m$outliers
matplot(tt, lgp, type="l", lty=1)
lines(tt, lgp[,5], col="red", lwd=3)   # shape/amplitude outlier
lines(tt, lgp[,2], col="blue", lwd=3)  # magnitude outlier

legend("topright",
       legend = colnames(lgp)[c(5, 2)],
       col = c("red", "blue"),
       lwd = 3,
       cex = 0.8)

# functional boxplot visualization
fbplot(lgp, method = "MBD",
       xaxt = "n",
       main = "Functional boxplot using Modified Band Depth",
       xlab = "Days relative to event (t)",
       ylab = "Standardized log returns")

# norimos dienos grafike
ticks <- c(-20, 0, 20, 40, 60)

# artimiausi grafikai
tick_pos <- sapply(ticks, function(x) which.min(abs(t_rel - x)))

#asis
axis(1,
     at = tick_pos,
     labels = ticks)

colnames(lgp)
colnames(lgp)[5]
colnames(lgp)[2]

###############################################
#PRINCIPAL COMPONENTS ANALYSIS
###############################################

nharm = 4
pcalist = pca.fd(ret_fd, nharm, centerfns = TRUE)
pcalist$varprop
cumsum(pcalist$varprop) #the choice of 4 harmonics is good because the cumulative variation is around 72.6%, which means that 
#most of variation is explained, another harmonic would probably catch mostely noise and only 3 harmonics would
#explain too little of teh total variance.


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

plot(varmx$harmonics)


###########################################################################################
#                         AMPLITUDE & PHASE DECOMPOSITION
###########################################################################################
# Amplitude variation — did commodities react with different magnitudes? 
#(gold up a lot, copper down a lot)

#Phase variation — did they react at different times? 
#(some spiked on day −3 anticipating the news, others lagged to day +5)

#Warping meaning:
#A simple example: suppose copper and gold both have a dip after the announcement, 
#but copper's dip happens at day +2 and gold's at day +4. If you warp gold's time 
#axis — squeezing it slightly — gold's dip shifts to day +2 and now both curves 
#align. The warping function records exactly how much you had to squeeze or 
#stretch each commodity's time axis to achieve that alignment.

#In the results this turned out to matter very little (only 15% phase variation), 
#meaning the curves were already well aligned in time and barely needed any warping — 
#makes intuitive sense, since all commodities reacted to the same single public 
#announcement on the same day.

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

#Result: 
#98.6% amplitude, 1.4% phase — the dominant source of variation across commodity curves 
#is how much they moved, not when they moved. Commodities reacted to the tariff 
#announcement at roughly similar times, but with very different magnitudes. 
#Some shot up strongly, others dropped sharply, others barely moved — but they all 
#did it at approximately the same moment.

#The R-squared of 0.15 measures how much of the total variation registration actually 
#managed to remove by time-warping. A value of 0.15 means registration explained very 
#little — the curves were already well-aligned in time before registration, 
#so there wasn't much phase variation to remove in the first place. 

#In Conclusion: the tariff shock hit all commodity markets at the same time 
#but the size and direction of each market's response varied substantially
#across commodities — and that magnitude variation is what distinguishes 
#the assets between one another.

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

#The one-sample pointwise bootstrap test shows that mean returns are not different from zero for most of the period. 
#However, a significant negative deviation appears around the event date, indicating a short-lived adverse market reaction
#that quickly dissipates.

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

sig_ranges <- sig_timepoints_dates %>%
  arrange(time_point) %>%
  mutate(group = cumsum(c(TRUE, diff(time_point) != 1))) %>%
  group_by(group, direction) %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    .groups = "drop"
  )

sig_ranges

#The most important result for your hypothesis is group 3:
#The mean commodity return function shows a statistically significant negative 
#deviation from zero from one day before to five days after the tariff announcement, 
#covering 2025-04-01 to 2025-04-07. This indicates a short-term adverse abnormal 
#reaction around the announcement date.

#The pre-event positive periods may suggest anticipation, information leakage, 
#or broader market repricing before the official announcement.

########################################################################
#### One sample L2_norm_based_test                                  ####
########################################################################
########################################################################

source("items/trace.R")
source("items/L2stat.R")

# Test hypothesis, that the mean functional return is equal to zero
# H0: mu(returns) = 0
# H1: mu(returns) != 0
# 
stat <- L2.stat(x=ret_fd, t.seq = t.seq, mu0=mu0, replication = 500, method = 2)
stat$pvalue

# despite the short-lived nature of the effect observed in pointwise tests, the tariff announcement had a statistically 
# significant overall impact on commodity returns.


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

#p- value 0.004<0.05, therefore null hypothesis is rejected, 
#mean functional returns are not equal to 0

########################################################################
#### Two sample pointwise-test                                      ####
########################################################################

source("items/Ztwosample.R")

# Test hypothesis, that safe-haven and cyclical commodities 
# have the same mean functional response to the tariff event

# H0: mu(safe-haven) = mu(cyclical)
# H1: mu(safe-haven) != mu(cyclical)

#DOING WITH HAND MADE CLUSTERS

stat_ztwosample <- Ztwosample(x = ret_fd[idx1], 
                   y = ret_fd[idx2], 
                   t.seq = t.seq)

stat_ztwosample

#DOING WITH HIERRARCHICAL CLUSTERING MADE CLUSTERS

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

#Around the tariff announcement, the two groups behaved differently.
#On April 1–5, Cluster 1 did worse than Cluster 2.
#Cluster 1 is mostly physical commodities like gold, silver, copper, oil, wheat, 
#corn, soybeans, and cotton. So this means that these commodity-style assets reacted 
#more negatively right when the tariff news came out.

#Cluster 2, which includes currencies and the Treasury note, was more stable or 
#performed better during those same days.

#Then, from April 8–13, the pattern flipped.
#Cluster 1 started doing better than Cluster 2. This suggests that after the 
#first negative shock, the commodity group recovered or bounced back.

########################################################################
#### Two sample L2-norm-based-test                                  ####
########################################################################

source("items/L2stattwosample.R")

# Test hypothesis, that safe-haven and cyclical commodities 
# have the same mean functional response (globally)

# H0: mu(safe-haven) = mu(cyclical)
# H1: mu(safe-haven) != mu(cyclical)

#DOING WITH HAND MADE CLUSTERS

stat_l2twosample <- L2.stat.twosample(x = ret_fd[idx1], 
                                      y = ret_fd[idx2], 
                                      t.seq = t.seq, 
                                      method = 1, 
                                      replications = 500)

stat_l2twosample$pvalue

#Both pointwise and L2 two-sample tests fail to reject the null hypothesis, indicating no statistically significant 
#difference between safe-haven and cyclical commodities in their mean functional responses to the tariff announcement. 
#This suggests that, despite observable differences in individual trajectories, the average behavior of the two groups 
#is not significantly distinct.

#DOING WITH HIERRARCHICAL CLUSTERING MADE CLUSTERS

stat_l2twosample <- L2.stat.twosample(x = ret_fd[cluster1_idx], 
                          y = ret_fd[cluster2_idx], 
                          t.seq = t.seq, 
                          method = 1, 
                          replications = 500)

stat_l2twosample$pvalue

#with method 1 - p value 0.0002, hypothesis rejected, means noy equal
#The two clusters do not behave the same over the event window. Their average return 
#patterns are statistically different.
#The pointwise Z-test tells you when the clusters differed, such as April 2–3 and April 8–12.
#The L2 two-sample test tells you that, overall, the two clusters have different return curves.



########################################################################
#### Two sample F-type-test                                         ####
########################################################################
source("items/Fstattwosample.R")
#DOING WITH HAND MADE CLUSTERS
# H0: mu(safe-haven) = mu(cyclical)
# H1: mu(safe-haven) != mu(cyclical)

stat_l2twosample <- F.stat.twosample(x = ret_fd[idx1], 
                                      y = ret_fd[idx2], 
                                      t.seq = t.seq, 
                                      method = 1, 
                                      replications = 500)

stat_l2twosample$pvalue

#pvalue 0.94.. not rejected null hypothesis no difference.

#DOING WITH HIERRARCHICAL CLUSTERING CLUSTERS


stat <- F.stat.twosample(x = ret_fd[cluster1_idx], 
                         y = ret_fd[cluster2_idx], 
                         t.seq = t.seq, 
                         method = 1, 
                         replications = 500)
stat
stat$pvalue

#p-value 0.0006<0.05, null hypothesis rejected, the mean returns of both groups differ


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
