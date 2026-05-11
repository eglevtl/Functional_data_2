"""
Functional Data Analysis of Commodity Returns around Trump's "Liberation Day" Tariff Announcement
(2025 April 2)
"""

# ============================================================
# 0. IMPORTS
# ============================================================
from scipy.stats import t
from scipy.stats import f
from scipy.stats import chi2
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from matplotlib import cm
from scipy.interpolate import BSpline, make_lsq_spline
from scipy.linalg import lstsq
from scipy.spatial.distance import pdist, squareform
from scipy.cluster.hierarchy import linkage, dendrogram, fcluster
from scipy.stats import norm
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import warnings
warnings.filterwarnings("ignore")

# skfda
from skfda import FDataGrid
from skfda.representation.basis import BSplineBasis
from skfda.representation import FDataBasis
from skfda.preprocessing.smoothing import BasisSmoother
from skfda.exploratory.visualization import FPCAPlot
from skfda.preprocessing.dim_reduction import FPCA

from scipy.interpolate import BSpline
from scipy.linalg import solve

# ============================================================
# HELPER: B-SPLINE SMOOTHER (mirrors smooth.basis in R)
# ============================================================

def make_bspline_basis_matrix(t, n_basis, order, domain):
    t_min, t_max = domain
    n_interior = n_basis - order
    interior_knots = np.linspace(t_min, t_max, n_interior + 2)[1:-1]
    knots = np.concatenate([
        np.repeat(t_min, order),
        interior_knots,
        np.repeat(t_max, order)
    ])
    B = np.zeros((len(t), n_basis))
    for i in range(n_basis):
        c = np.zeros(n_basis)
        c[i] = 1.0
        B[:, i] = BSpline(knots, c, order - 1)(t)
    return B, knots

def make_penalty_matrix(knots, n_basis, order, domain, n_quad=500):
    t_min, t_max = domain
    t_quad, w_quad = np.polynomial.legendre.leggauss(n_quad)
    t_mapped = 0.5*(t_max - t_min)*t_quad + 0.5*(t_max + t_min)
    w_mapped = 0.5*(t_max - t_min)*w_quad
    h = (t_max - t_min) * 1e-5
    B2 = np.zeros((n_quad, n_basis))
    for i in range(n_basis):
        c = np.zeros(n_basis); c[i] = 1.0
        spl = BSpline(knots, c, order - 1)
        B2[:, i] = (spl(t_mapped + h) - 2*spl(t_mapped) + spl(t_mapped - h)) / h**2
    return (B2 * w_mapped[:, None]).T @ B2

def smooth_bspline(t_grid, Y, n_basis, order=4, lam=0.0):
    n, p     = Y.shape
    domain   = (t_grid.min(), t_grid.max())
    B, knots = make_bspline_basis_matrix(t_grid, n_basis, order, domain)
    D2       = make_penalty_matrix(knots, n_basis, order, domain)
    A        = B.T @ B + lam * D2
    coef     = np.linalg.solve(A, B.T @ Y)
    fitted   = B @ coef
    resid    = Y - fitted
    H        = B @ np.linalg.solve(A, B.T)
    df       = np.trace(H)
    denom    = (1 - df/n)**2
    gcv      = np.sum(resid**2) / (n * denom) if denom > 1e-12 else np.inf
    return coef, knots, gcv, B, np.linalg.inv(A)

def eval_fd(t_eval, coef, knots, order=4):
    t_eval  = np.atleast_1d(t_eval)
    n_basis = coef.shape[0]
    domain  = (knots[order-1], knots[-order])
    B, _    = make_bspline_basis_matrix(t_eval, n_basis, order, domain)
    return B @ coef


# ============================================================
# 1. LOAD DATA
# ============================================================
path = r"C:\Users\liepa\Documents\VU\Mokslai 2025_2027\Functional data analysis\Projektas (geras)\Python\Yfinance_close_prices_V2.xlsx"
px   = pd.read_excel(path, sheet_name="Close_prices")
px["Date"] = pd.to_datetime(px["Date"])
px   = px.sort_values("Date").reset_index(drop=True)

# Drop rows with any missing prices
px = px.dropna().reset_index(drop=True)

dates       = px["Date"].values
commodities = px.columns[1:].tolist()
Prices      = px[commodities].values.astype(float)   # (T, p)

# ============================================================
# 2. LOG PRICES → LOG RETURNS → STANDARDISE
# ============================================================
logP    = np.log(Prices)
returns = np.diff(logP, axis=0)         # (T-1, p)
dates_return = pd.to_datetime(dates[1:])

scaler         = StandardScaler()
scaled_returns = scaler.fit_transform(returns)   # (T-1, p)

# ============================================================
# 3. TIME AXIS RELATIVE TO EVENT DATE
# ============================================================
event_date = pd.Timestamp("2025-04-02")
t_rel      = np.array([(d - event_date).days for d in dates_return])
range_t    = (t_rel.min(), t_rel.max())

# ============================================================
# 4. RAW PLOT
# ============================================================
fig, ax = plt.subplots(figsize=(14, 6))
tt_idx  = np.arange(len(t_rel))
for j in range(scaled_returns.shape[1]):
    ax.plot(tt_idx, scaled_returns[:, j], lw=0.8, alpha=0.6)
matches = np.where(t_rel == 0)[0]
if len(matches) == 0:
    # fallback: find the index closest to 0
    event_index = int(np.argmin(np.abs(t_rel)))
else:
    event_index = int(matches[0])
ax.axvline(event_index, color="red", lw=2, label="Tariff announcement")
ax.set_xlabel("Time (index)")
ax.set_ylabel("Scaled log return")
ax.set_title("Raw scaled returns")
ax.legend()
plt.tight_layout()
plt.savefig("plot_01_raw_returns.png", dpi=120)
plt.show()

# ============================================================
# 5. GCV GRID SEARCH FOR n_basis AND lambda
# ============================================================
nbasis_grid = np.arange(7, 21)
lambda_grid = np.logspace(-2, 10, 100)

gcv_mat = np.full((len(nbasis_grid), len(lambda_grid)), np.nan)

for i, nb in enumerate(nbasis_grid):
    for j, lam in enumerate(lambda_grid):
        try:
            _, _, gcv_val, _, _ = smooth_bspline(t_rel, scaled_returns, n_basis=nb, order=4, lam=lam)
            gcv_mat[i, j] = gcv_val
        except Exception:
            pass

best_i, best_j = np.unravel_index(np.nanargmin(gcv_mat), gcv_mat.shape)
best_nbasis    = int(nbasis_grid[best_i])
best_lambda    = float(lambda_grid[best_j])
print(f"Best n_basis : {best_nbasis}")
print(f"Best lambda  : {best_lambda:.6e}")

# GCV plot
fig, ax = plt.subplots(figsize=(10, 5))
for i, nb in enumerate(nbasis_grid):
    ax.plot(np.log10(lambda_grid), gcv_mat[i], lw=1.2, label=f"nbasis={nb}")
ax.set_xlabel("log10(lambda)")
ax.set_ylabel("GCV")
ax.set_title("GCV surface – basis & lambda selection")
ax.legend(fontsize=7, ncol=2)
plt.tight_layout()
plt.savefig("plot_02_gcv.png", dpi=120)
plt.show()
print("Saved plot_02_gcv.png")

# ============================================================
# 6. FIT SMOOTHED FUNCTIONAL OBJECTS
# ============================================================
coef, knots, _, B_mat, A_inv = smooth_bspline(
    t_rel, scaled_returns, n_basis=20, order=4, lam=14.17474
)
# coef shape: (n_basis, p)

# Helper: evaluate all curves on a fine grid
plot_days = np.arange(t_rel.min(), t_rel.max() + 1, dtype=float)
lgp       = eval_fd(plot_days, coef, knots)   # (n_days, p)

# Smoothed curves plot
fig, ax = plt.subplots(figsize=(14, 6))
for j in range(lgp.shape[1]):
    ax.plot(plot_days, lgp[:, j], lw=1.2, alpha=0.7)
ax.axvline(0, color="red", lw=2, ls="--", label="Tariff announcement (t=0)")
ax.set_xlabel("Days relative to tariff announcement")
ax.set_ylabel("Scaled log return (smoothed)")
ax.set_title("Smoothed functional curves")
ax.legend()
plt.tight_layout()
plt.savefig("plot_03_smoothed_curves.png", dpi=120)
plt.show()
print("Saved plot_03_smoothed_curves.png")

# ============================================================
# 7. PRE / POST MEANS  (window ±20 days)
# ============================================================
pre_days  = np.arange(-20, 0)
post_days = np.arange(1, 21)

idx_pre  = np.isin(t_rel, pre_days)
idx_post = np.isin(t_rel, post_days)

pre_mean  = scaled_returns[idx_pre].mean(axis=0)
post_mean = scaled_returns[idx_post].mean(axis=0)

pre_post_df = pd.DataFrame({"pre_mean": pre_mean, "post_mean": post_mean},
                            index=commodities)
print("\nPre / Post means:\n", pre_post_df.to_string())

# ============================================================
# 8. EXPLORATORY: POINTWISE MEAN & STD DEV
# ============================================================
mean_curve = lgp.mean(axis=1)   # (n_days,)
std_curve  = lgp.std(axis=1)

fig, ax = plt.subplots(figsize=(14, 6))
for j in range(lgp.shape[1]):
    ax.plot(plot_days, lgp[:, j], lw=0.8, alpha=0.4, color="steelblue")
ax.plot(plot_days, mean_curve, lw=3, color="red",    ls="--", label="Mean")
ax.plot(plot_days, std_curve,  lw=3, color="blue",   ls="--", label="Std dev")
ax.plot(plot_days, mean_curve + std_curve, lw=3, color="purple", ls="--", label="Mean ± SD")
ax.plot(plot_days, mean_curve - std_curve, lw=3, color="purple", ls="--")
ax.axvline(0, color="red", lw=1.5, ls=":", label="t=0")
ax.set_xlabel("Days relative to tariff announcement")
ax.set_ylabel("Scaled log return (smoothed)")
ax.set_title("Functional mean and standard deviation")
ax.legend()
plt.tight_layout()
plt.savefig("plot_04_mean_std.png", dpi=120)
plt.show()
print("Saved plot_04_mean_std.png")

# ============================================================
# 9. COVARIANCE SURFACE
# ============================================================
cov_mat = np.cov(lgp.T)  # (p, p) — covariance across commodities at each time...
# For the time-domain covariance surface C(s,t) = E[X(s)X(t)]:
cov_surface = (lgp - mean_curve[:, None]).T  # (p, n_days) centered
# C(s,t) approximated as: cov_surface.T @ cov_surface / p  → (n_days, n_days)
n_days_arr = len(plot_days)
C_st = (lgp - mean_curve[:, None]) @ (lgp - mean_curve[:, None]).T / lgp.shape[1]
# C_st shape: (n_days, n_days)

from matplotlib.colors import TwoSlopeNorm
fig, ax = plt.subplots(figsize=(8, 7))
vmax  = np.abs(C_st).max()
norm  = TwoSlopeNorm(vmin=-vmax, vcenter=0, vmax=vmax)
cf    = ax.contourf(plot_days, plot_days, C_st, levels=30,
                    cmap="RdBu_r", norm=norm)
fig.colorbar(cf, ax=ax, label="Cov")
ax.set_xlabel("Day")
ax.set_ylabel("Day")
ax.set_title("Variance-Covariance Surface")
ax.axvline(0, color="black", lw=0.8, ls=":")
ax.axhline(0, color="black", lw=0.8, ls=":")
plt.tight_layout()
plt.savefig("plot_05_cov_surface.png", dpi=120)
plt.show()

# ============================================================
# 10. FUNCTIONAL DEPTH & OUTLIER DETECTION  (Modified Band Depth)
# ============================================================

def modified_band_depth(X):
    """
    MBD for functional data.
    X : (n_curves, n_timepoints)
    Returns MBD score per curve (higher = more central).
    """
    n, T = X.shape
    mbd  = np.zeros(n)
    for i in range(n):
        count = 0
        for j in range(n):
            for k in range(j + 1, n):
                lo = np.minimum(X[j], X[k])
                hi = np.maximum(X[j], X[k])
                inside = np.mean((X[i] >= lo) & (X[i] <= hi))
                count += inside
        mbd[i] = count / (n * (n - 1) / 2)
    return mbd

X_curves = lgp.T   # (p, n_days)
mbd_scores = modified_band_depth(X_curves)

mbd_df = pd.Series(mbd_scores, index=commodities).sort_values()
print("\nMBD scores (ascending):\n", mbd_df.to_string())

# Outlier: below Q1 - 1.5*IQR
q1, q3 = np.percentile(mbd_scores, [25, 75])
iqr     = q3 - q1
outlier_thresh = q1 - 1.5 * iqr
outlier_mask   = mbd_scores < outlier_thresh
outliers_mbd   = [c for c, o in zip(commodities, outlier_mask) if o]
print(f"\nMBD outliers: {outliers_mbd}")

# Functional median = curve with highest MBD
median_idx = int(np.argmax(mbd_scores))
outlier_idx_list = [i for i, o in enumerate(outlier_mask) if o]

fig, ax = plt.subplots(figsize=(14, 6))
for j in range(lgp.shape[1]):
    ax.plot(plot_days, lgp[:, j], lw=0.8, color="grey", alpha=0.5)
for oi in outlier_idx_list:
    ax.plot(plot_days, lgp[:, oi], lw=2.5, label=f"MBD outlier: {commodities[oi]}")
ax.plot(plot_days, lgp[:, median_idx], lw=2.5, color="blue",
        label=f"Functional median: {commodities[median_idx]}")
ax.axvline(0, color="red", lw=1.5, ls="--")
ax.set_xlabel("Days relative to tariff announcement")
ax.set_ylabel("Standardized log returns")
ax.set_title("Functional curves with MBD outlier highlighted")
ax.legend()
plt.tight_layout()
plt.savefig("plot_06_outliers.png", dpi=120)
plt.show()

# ============================================================
# 11. FUNCTIONAL BOXPLOT  (envelope-based, mirrors fbplot)
# ============================================================
# Sort curves by MBD; central 50% form the envelope
median_curve = lgp[:, median_idx]
sorted_idx  = np.argsort(mbd_scores)[::-1]  # descending: most central first
n_central   = max(1, int(np.floor(0.5 * len(commodities))))
central_idx = sorted_idx[:n_central]
outer_idx   = sorted_idx[n_central:]

env_lo  = lgp[:, central_idx].min(axis=1)
env_hi  = lgp[:, central_idx].max(axis=1)
fence_lo = median_curve - 1.5 * (env_hi - env_lo)
fence_hi = median_curve + 1.5 * (env_hi - env_lo)

fbp_outliers = [commodities[i] for i in range(len(commodities))
                if np.any(lgp[:, i] < fence_lo) or np.any(lgp[:, i] > fence_hi)]
print(f"\nFunctional boxplot outliers: {fbp_outliers}")

fig, ax = plt.subplots(figsize=(14, 6))
ax.fill_between(plot_days, env_lo, env_hi, alpha=0.4, color="gold", label="50% central envelope")
ax.fill_between(plot_days, fence_lo, fence_hi, alpha=0.15, color="green", label="1.5×IQR fences")
ax.plot(plot_days, mean_curve, color="black", lw=2, label="Mean")
for j in range(lgp.shape[1]):
    c = "red" if commodities[j] in fbp_outliers else "grey"
    lw = 2 if commodities[j] in fbp_outliers else 0.6
    ax.plot(plot_days, lgp[:, j], color=c, lw=lw, alpha=0.7)
ax.axvline(0, color="red", lw=1.5, ls="--")
ax.set_xlabel("Days relative to tariff announcement")
ax.set_ylabel("Standardized log returns")
ax.set_title("Functional Boxplot (Modified Band Depth)")
ax.legend()
plt.tight_layout()
plt.savefig("plot_07_functional_boxplot.png", dpi=120)
plt.show()
print("Saved plot_07_functional_boxplot.png")

# ============================================================
# 12. FUNCTIONAL PCA  (FPCA)
# ============================================================
# Use skfda FDataGrid
fd_grid = FDataGrid(data_matrix=lgp.T,        # (p, n_days)
                    grid_points=plot_days)

n_harm = 4
fpca   = FPCA(n_components=n_harm)
scores = fpca.fit_transform(fd_grid)          # (p, n_harm)

explained = fpca.explained_variance_ratio_
print("\nVariance explained per harmonic:", np.round(explained, 4))
print("Cumulative:", np.round(np.cumsum(explained), 4))

# Plot harmonics
fig, axes = plt.subplots(2, 2, figsize=(14, 8))
for k, ax in enumerate(axes.flat):
    ax.plot(plot_days, fpca.components_.data_matrix[k, :, 0], lw=2)
    ax.axhline(0, color="grey", lw=0.8, ls="--")
    ax.axvline(0, color="red",  lw=0.8, ls="--")
    ax.set_title(f"FPC {k+1}  ({explained[k]*100:.1f}% var)")
    ax.set_xlabel("Day")
    ax.set_ylabel("Loading")
plt.suptitle("FPCA Harmonics", fontsize=13)
plt.tight_layout()
plt.savefig("plot_08_fpca_harmonics.png", dpi=120)
plt.show()

# ============================================================
# 13. CLUSTERING ON FPC1 & FPC2 SCORES
# ============================================================
dist_mat = squareform(pdist(scores[:, :2], metric="euclidean"))
Z_link   = linkage(scores[:, :2], method="ward")

fig, ax = plt.subplots(figsize=(14, 5))
dendrogram(Z_link, labels=commodities, ax=ax, leaf_rotation=45)
ax.set_title("Hierarchical clustering of commodities (FPC1 & FPC2)")
ax.set_ylabel("Height")
plt.tight_layout()
plt.savefig("plot_09_dendrogram.png", dpi=120)
plt.show()


hc_labels = fcluster(Z_link, t=2, criterion="maxclust")   # k=2 clusters

fig, ax = plt.subplots(figsize=(9, 7))
colors = ["red", "blue"]
for j, (s, label) in enumerate(zip(scores, hc_labels)):
    ax.scatter(s[0], s[1], color=colors[label - 1], s=80, zorder=3)
    ax.annotate(commodities[j], (s[0], s[1]), textcoords="offset points",
                xytext=(5, 3), fontsize=8)
ax.axhline(0, color="grey", lw=0.8, ls=":")
ax.axvline(0, color="grey", lw=0.8, ls=":")
ax.set_xlabel(f"FPC1 ({explained[0]*100:.1f}% var) – reaction intensity")
ax.set_ylabel(f"FPC2 ({explained[1]*100:.1f}% var) – direction divergence")
ax.set_title("Commodities clustered by FPCA scores (hierarchical)")
plt.tight_layout()
plt.savefig("plot_10_cluster_scatter.png", dpi=120)
plt.show()

cluster1_idx = np.where(hc_labels == 2)[0]
cluster2_idx = np.where(hc_labels == 1)[0]

print("Cluster 1:", [commodities[i] for i in cluster1_idx])
print("Cluster 2:", [commodities[i] for i in cluster2_idx])

ylim_common = (lgp.min(), lgp.max())

fig, axes = plt.subplots(1, 2, figsize=(16, 6), sharey=True)
for j in cluster1_idx:
    axes[0].plot(plot_days, lgp[:, j], lw=1.2, alpha=0.8)
axes[0].axhline(0, color="grey", lw=0.8, ls=":")
axes[0].axvline(0, color="red",  lw=1.5, ls="--")
axes[0].set_title("Cluster 1")
axes[0].set_xlabel("Days relative to tariff announcement")
axes[0].set_ylabel("Scaled log return")
axes[0].set_ylim(ylim_common)

for j in cluster2_idx:
    axes[1].plot(plot_days, lgp[:, j], lw=1.2, alpha=0.8)
axes[1].axhline(0, color="grey", lw=0.8, ls=":")
axes[1].axvline(0, color="red",  lw=1.5, ls="--")
axes[1].set_title("Cluster 2")
axes[1].set_xlabel("Days relative to tariff announcement")
axes[1].set_ylim(ylim_common)

plt.suptitle("Smoothed curves by cluster", fontsize=13)
plt.tight_layout()
plt.savefig("plot_11_cluster_curves.png", dpi=120)
plt.show()

# ============================================================
# 14. STATISTICAL TESTS
# ============================================================
# Bootstrap infrastructure shared by all tests

def _pointwise_variance(curves):
    """Pointwise variance across curves. curves: (n, T)"""
    return curves.var(axis=0, ddof=1)   # (T,)


def _mean_curve_eval(curves):
    return curves.mean(axis=0)          # (T,)


# -------------------------------------------------------
# 14a. ONE-SAMPLE POINTWISE BOOTSTRAP Z-TEST  (Zboottest)
#      H0: mu(t) = 0 for all t
# -------------------------------------------------------

def Z_boot_Rstyle(curves, t_seq, mu0=0.0, replication=500, alpha=0.05):
    n, T = curves.shape

    # Original mean and standard deviation at each time point
    mu_x = curves.mean(axis=0)
    sd_x = curves.std(axis=0, ddof=1)

    # Bootstrap matrix: rows = time points, columns = replications
    Z_star = np.zeros((T, replication))

    for b in range(replication):
        idx_b = np.random.choice(n, size=n, replace=True)
        boot_samp = curves[idx_b]

        mu_star = boot_samp.mean(axis=0)
        sd_star = boot_samp.std(axis=0, ddof=1)

        delta = mu_star - mu_x

        Z_star[:, b] = np.sqrt(n) * delta / (sd_star + 1e-12)

    # R-style pointwise critical value
    crit_val = np.quantile(Z_star, 1 - alpha / 2, axis=1)

    # Observed statistic
    Z = np.sqrt(n) * (mu_x - mu0) / (sd_x + 1e-12)

    return {
        "statistics": Z,
        "critical_value": crit_val,
        "t_seq": t_seq,
        "z_star": Z_star
    }

stat_all = Z_boot_Rstyle(curves_eval, t_seq, mu0=0.0, replication=500, alpha=0.05)

z_stats = stat_all["statistics"]
crit = stat_all["critical_value"]

sig_mask = np.abs(z_stats) > crit

sig_times = t_seq[sig_mask]
sig_dates = pd.Timestamp("2025-04-02") + pd.to_timedelta(sig_times.astype(int), unit="D")

sig_df = pd.DataFrame({
    "time": sig_times,
    "date": sig_dates,
    "z_statistic": z_stats[sig_mask],
    "critical_value": crit[sig_mask],
    "direction": [
        "Positive abnormal return" if z > 0 else "Negative abnormal return"
        for z in z_stats[sig_mask]
    ]
})

print("\nSignificant time points:\n", sig_df.to_string())

fig, ax = plt.subplots(figsize=(7, 5))

ax.plot(t_seq, z_stats, color="black", lw=1)

ax.plot(t_seq, crit, color="blue", lw=1.5, ls="--")
ax.plot(t_seq, -crit, color="blue", lw=1.5, ls="--")

ax.set_xlabel("Time")
ax.set_ylabel("Z statistics")
ax.set_title("One sample t-test", fontweight="bold")

ymin = min(np.min(z_stats), np.min(-crit)) - 0.5
ymax = max(np.max(z_stats), np.max(crit)) + 0.5
ax.set_ylim(ymin, ymax)

plt.tight_layout()
plt.show()

# -------------------------------------------------------
# 14b. ONE-SAMPLE L2-NORM BASED TEST
#      H0: mu = 0  (global)
# -------------------------------------------------------
def L2_stat_Rstyle(curves, t_seq, mu0=0.0, replication=500):
    n, T = curves.shape
    # Mean function evaluated on t_seq
    xbar = curves.mean(axis=0)
    # Observed L2 statistic, R-style: no dt multiplier
    F_obs = n * np.sum((xbar - mu0) ** 2)
    boot_stats = np.zeros(replication)
    for b in range(replication):
        idx_b = np.random.choice(n, size=n, replace=True)
        boot_samp = curves[idx_b]

        xbar_b = boot_samp.mean(axis=0)

        # R: btmu <- apply(btx, 1, mean) - mu.x.t
        btmu = xbar_b - xbar

        # R: btFstat[i] <- n * t(btmu) %*% btmu
        boot_stats[b] = n * np.sum(btmu ** 2)

    pval = np.mean(boot_stats >= F_obs)

    return {
        "statistic": F_obs,
        "pvalue": pval,
        "boot_stats": boot_stats
    }

l2_res = L2_stat_Rstyle(curves_eval, t_seq, mu0=0.0, replication=500)

print(f"L2 test: T = {l2_res['statistic']:.4f}, p-value = {l2_res['pvalue']:.4f}")

# -------------------------------------------------------
# 14c. ONE-SAMPLE F-TYPE TEST
#      H0: mu = 0  (global)
# -------------------------------------------------------
def trace_matrix(M):
    return np.trace(M)

def F_stat_Rstyle(curves, t_seq, mu0=0.0, replication=500):
    n, T = curves.shape

    # Mean curve
    mu_x = curves.mean(axis=0)

    # Centered curves
    z = curves - mu_x

    # R equivalent:
    # if(n > k) Sigma <- (t(z.t) %*% z.t)/(n-1)
    # else      Sigma <- (z.t %*% t(z.t))/(n-1)
    #
    # Since usually n < k in your case:
    Sigma = (z.T @ z) / (n - 1)

    A = np.trace(Sigma)

    # Observed F statistic
    F_obs = n * np.sum((mu_x - mu0) ** 2) / A

    # Bootstrap
    boot_stats = np.zeros(replication)

    for b in range(replication):
        idx_b = np.random.choice(n, size=n, replace=True)
        boot_samp = curves[idx_b]

        # Bootstrap mean
        mu_b = boot_samp.mean(axis=0)

        # Center bootstrap sample
        z_b = boot_samp - mu_b

        # Bootstrap covariance
        Sigma_b = (z_b.T @ z_b) / (n - 1)
        A_b = np.trace(Sigma_b)

        # R: btmu <- apply(btx,1,mean) - mu.x.t
        btmu = mu_b - mu_x

        # R: btFstat[i] <- (n * t(btmu) %*% btmu)/trace(btSigma)
        boot_stats[b] = n * np.sum(btmu ** 2) / A_b

    pval = np.mean(boot_stats >= F_obs)

    return {
        "statistic": F_obs,
        "pvalue": pval,
        "boot_stats": boot_stats
    }

f_res = F_stat_Rstyle(curves_eval, t_seq, mu0=0.0, replication=500)

print(f"F-type test: F = {f_res['statistic']:.4f}, p-value = {f_res['pvalue']:.4f}")

# -------------------------------------------------------
# 14d. TWO-SAMPLE POINTWISE Z-TEST  (Ztwosample)
#      H0: mu_1(t) = mu_2(t)
# -------------------------------------------------------
def Z_twosample_Rstyle(curves_x, curves_y, t_seq, alpha=0.05):
    n, k = curves_x.shape
    m = curves_y.shape[0]

    # Mean functions
    mu_x = curves_x.mean(axis=0)
    mu_y = curves_y.mean(axis=0)

    # Difference between mean functions
    delta_t = mu_x - mu_y

    # Centered curves
    z_x_t = curves_x - mu_x
    z_y_t = curves_y - mu_y

    # Equivalent to R: z.t <- cbind(z.x.t, z.y.t)
    # In Python, rows are curves and columns are time points
    z_t = np.vstack([z_x_t, z_y_t])

    # This matches the structure of your R code:
    # Sigma <- (z.t %*% t(z.t))/(n-2)
    # gamma.t <- diag(Sigma)
    #
    # Since we only need the diagonal, this is equivalent:
    gamma_t = np.sum(z_t**2, axis=0) / (n - 2)

    # Pointwise statistic
    Zpointwise = np.sqrt((n * m) / (n + m)) * delta_t / np.sqrt(gamma_t)

    # R-style critical value
    crit = t.ppf(1 - alpha / 2, df=n - 2)

    return {
        "statistics_pointwise": Zpointwise,
        "critical_value": crit
    }

c1 = curves_eval[cluster1_idx]   # Cluster 1 curves, shape: (n1, T)
c2 = curves_eval[cluster2_idx]   # Cluster 2 curves, shape: (n2, T)

stat_2s = Z_twosample_Rstyle(c1, c2, t_seq, alpha=0.05)

z2 = stat_2s["statistics_pointwise"]
crit2 = stat_2s["critical_value"]

sig_mask2 = np.abs(z2) > crit2

sig_2s_df = pd.DataFrame({
    "time": t_seq[sig_mask2],
    "date": pd.Timestamp("2025-04-02") + pd.to_timedelta(t_seq[sig_mask2].astype(int), unit="D"),
    "z_statistic": z2[sig_mask2],
    "critical_value": crit2,
    "direction": [
        "Cluster 1 > Cluster 2" if z > 0 else "Cluster 1 < Cluster 2"
        for z in z2[sig_mask2]
    ]
})

print("\nTwo-sample significant time points:\n", sig_2s_df.to_string())

fig, ax = plt.subplots(figsize=(7, 5))

ax.plot(t_seq, z2, color="black", lw=1)

ax.axhline(crit2, color="blue", lw=1.5, ls="--")
ax.axhline(-crit2, color="blue", lw=1.5, ls="--")

ax.set_xlabel("Time")
ax.set_ylabel("Z statistics")
ax.set_title("Two samples t-test", fontweight="bold")

ymin = min(np.min(z2), -crit2) - 0.5
ymax = max(np.max(z2), crit2) + 0.5
ax.set_ylim(ymin, ymax)

plt.tight_layout()
plt.savefig("plot_13_ztest_twosample_Rstyle.png", dpi=120)
plt.show()

# -------------------------------------------------------
# 14e. TWO-SAMPLE L2-NORM GLOBAL TEST
# -------------------------------------------------------
def L2_stat_twosample_Rstyle_method1(curves_x, curves_y, t_seq):
    nx, T = curves_x.shape
    ny, T2 = curves_y.shape

    if T != T2:
        raise ValueError("curves_x and curves_y must have the same number of time points.")

    k = len(t_seq)

    xbar = curves_x.mean(axis=0)
    ybar = curves_y.mean(axis=0)

    delta = xbar - ybar

    cn = (nx * ny) / (nx + ny)

    # R: L2stat <- cn * t(delta.t) %*% delta.t
    L2stat = cn * np.sum(delta ** 2)

    # Centered curves
    zx = curves_x - xbar
    zy = curves_y - ybar

    # R: z.t <- cbind(z.x.t, z.y.t)
    # R has time x curves, so transpose first
    z_t = np.column_stack([zx.T, zy.T])

    # Match R structure
    if nx > k or ny > k:
        Sigma = (z_t.T @ z_t) / (nx - 2)
    else:
        Sigma = (z_t @ z_t.T) / (nx - 2)

    A = np.trace(Sigma)
    B = np.sum(np.diag(Sigma) ** 2)

    alp = B / A
    df = (A ** 2) / B

    pvalue = 1 - chi2.cdf(L2stat / alp, df)

    return {
        "statistic": L2stat,
        "pvalue": pvalue,
        "alpha_param": alp,
        "df": df
    }

l2_2s = L2_stat_twosample_Rstyle_method1(c1, c2, t_seq)

print(f"Two-sample L2 test: T = {l2_2s['statistic']:.4f}, p-value = {l2_2s['pvalue']:.4f}")
print(f"alpha = {l2_2s['alpha_param']:.4f}, df = {l2_2s['df']:.4f}")

# -------------------------------------------------------
# 14f. TWO-SAMPLE F-TYPE GLOBAL TEST
# -------------------------------------------------------

def F_stat_twosample_Rstyle_method1(curves_x, curves_y, t_seq):
    n, k = curves_x.shape
    m, k2 = curves_y.shape

    if k != k2:
        raise ValueError("curves_x and curves_y must have the same number of time points.")

    # Mean curves
    mu_x = curves_x.mean(axis=0)
    mu_y = curves_y.mean(axis=0)

    # Difference between mean curves
    delta = mu_x - mu_y

    # cn = (n*m)/(n+m)
    cn = (n * m) / (n + m)

    # Centered curves
    z_x = curves_x - mu_x
    z_y = curves_y - mu_y

    # R has matrices as time x curves
    z_x_t = z_x.T
    z_y_t = z_y.T

    # R: z.t <- cbind(z.x.t, z.y.t)
    z_t = np.column_stack([z_x_t, z_y_t])

    # R covariance construction
    if n > k or m > k:
        Sigma = (z_t.T @ z_t) / (n - 2)
    else:
        Sigma = (z_t @ z_t.T) / (n - 2)

    A = np.trace(Sigma)
    B = np.trace(Sigma @ Sigma)

    # R: Fstat <- (cn * t(delta.t) %*% delta.t) / A
    Fstat = cn * np.sum(delta ** 2) / A

    # R method = 1
    kappa = A**2 / B
    df1 = kappa
    df2 = (n - 2) * kappa

    pvalue = 1 - f.cdf(Fstat, df1, df2)

    return {
        "statistic": Fstat,
        "pvalue": pvalue,
        "df1": df1,
        "df2": df2,
        "A": A,
        "B": B
    }

f_2s = F_stat_twosample_Rstyle_method1(c1, c2, t_seq)

print(f"Two-sample F-type test: F = {f_2s['statistic']:.6f}, p-value = {f_2s['pvalue']:.6g}")
print(f"df1 = {f_2s['df1']:.5f}")
print(f"df2 = {f_2s['df2']:.5f}")

# ============================================================
# 15. AMPLITUDE & PHASE DECOMPOSITION
# ============================================================

def amplitude_phase_decomp(original, registered):
    mean_orig = original.mean(axis=1, keepdims=True)   # (T,1)
    MS_total  = np.mean((original - mean_orig) ** 2)

    mean_reg  = registered.mean(axis=1, keepdims=True)
    MS_amp    = np.mean((registered - mean_reg) ** 2)
    MS_pha    = MS_total - MS_amp
    R_sq = MS_pha / (MS_total + 1e-12)
    return {"MS_amp": MS_amp, "MS_pha": MS_pha, "RSQR": R_sq}


# Simple registration: align each curve to the cross-sectional mean by
# time-shifting within ±5 days (discrete approximation of register.fd)
def simple_register(lgp_matrix, plot_days):
    mean_c    = lgp_matrix.mean(axis=1)
    registered = np.zeros_like(lgp_matrix)
    max_shift = 5
    for j in range(lgp_matrix.shape[1]):
        best_lag, best_mse = 0, np.inf
        for lag in range(-max_shift, max_shift + 1):
            shifted = np.roll(lgp_matrix[:, j], lag)
            mse = np.mean((shifted - mean_c) ** 2)
            if mse < best_mse:
                best_mse, best_lag = mse, lag
        registered[:, j] = np.roll(lgp_matrix[:, j], best_lag)
    return registered

lgp_reg   = simple_register(lgp, plot_days)
decomp    = amplitude_phase_decomp(lgp, lgp_reg)
total_ms  = decomp["MS_amp"] + decomp["MS_pha"]
print(f"Amplitude MS : {decomp['MS_amp']:.4f} ({100*decomp['MS_amp']/total_ms:.1f}%)")
print(f"Phase MS     : {decomp['MS_pha']:.4f} ({100*decomp['MS_pha']/total_ms:.1f}%)")
print(f"R-squared    : {decomp['RSQR']:.4f}")

# ============================================================
# 16. SUMMARY TABLE OF ALL TESTS
# ============================================================
summary = pd.DataFrame({
    "Test": [
        "One-sample L2 global (H₀: μ=0)",
        "One-sample F-type global (H₀: μ=0)",
        "Two-sample L2 global (H₀: μ₁=μ₂)",
        "Two-sample F-type global (H₀: μ₁=μ₂)",
    ],
    "Statistic": [
        l2_res["statistic"], f_res["statistic"],
        l2_2s["statistic"],  f_2s["statistic"],
    ],
    "p-value": [
        l2_res["pvalue"], f_res["pvalue"],
        l2_2s["pvalue"],  f_2s["pvalue"],
    ],
    "Decision (α=0.05)": [
        "Reject H₀" if p < 0.05 else "Fail to reject"
        for p in [l2_res["pvalue"], f_res["pvalue"],
                  l2_2s["pvalue"],  f_2s["pvalue"]]
    ]
})
print("\n========== TEST SUMMARY ==========")
print(summary.to_string(index=False))
