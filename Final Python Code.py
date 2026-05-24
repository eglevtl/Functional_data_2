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
import matplotlib.gridspec as gridspec
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

from skfda.exploratory.depth import ModifiedBandDepth
from skfda.exploratory.visualization import Boxplot
from skfda.exploratory.outliers import BoxplotOutlierDetector

from skfda.exploratory.depth import IntegratedDepth

from skfda.preprocessing.registration.validation import AmplitudePhaseDecomposition
from skfda.preprocessing.registration import FisherRaoElasticRegistration

# ============================================================
# 1. LOAD DATA
# ============================================================
path = r"Yfinance_close_prices_V2.xlsx"
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

def make_penalty_matrix(knots, n_basis, order, domain, penalty_order=0, n_quad=500):
    t_min, t_max = domain
    t_quad, w_quad = np.polynomial.legendre.leggauss(n_quad)
    t_mapped = 0.5*(t_max - t_min)*t_quad + 0.5*(t_max + t_min)
    w_mapped = 0.5*(t_max - t_min)*w_quad
    Bpen = np.zeros((n_quad, n_basis))
    for i in range(n_basis):
        c = np.zeros(n_basis)
        c[i] = 1.0
        spl = BSpline(knots, c, order - 1)
        if penalty_order == 0:
            Bpen[:, i] = spl(t_mapped)
        elif penalty_order == 1:
            Bpen[:, i] = spl.derivative(1)(t_mapped)
        elif penalty_order == 2:
            Bpen[:, i] = spl.derivative(2)(t_mapped)
        else:
            raise ValueError("penalty_order must be 0, 1, or 2")
    return (Bpen * w_mapped[:, None]).T @ Bpen

def smooth_bspline(t_grid, Y, n_basis, order=4, lam=0.0, penalty_order=0):
    n, p     = Y.shape
    domain   = (t_grid.min(), t_grid.max())
    B, knots = make_bspline_basis_matrix(t_grid, n_basis, order, domain)
    P = make_penalty_matrix(
        knots,
        n_basis,
        order,
        domain,
        penalty_order=penalty_order
    )
    A = B.T @ B + lam * P
    coef = np.linalg.solve(A, B.T @ Y)
    fitted = B @ coef
    resid = Y - fitted
    H = B @ np.linalg.solve(A, B.T)
    df = np.trace(H)
    denom = (1 - df / n) ** 2
    gcv = np.sum(resid ** 2) / (n * denom) if denom > 1e-12 else np.inf
    return coef, knots, gcv, B, np.linalg.inv(A)

nbasis_grid = np.arange(7, 21)
lambda_grid = np.logspace(-2, 2, 100)

gcv_mat = np.full((len(nbasis_grid), len(lambda_grid)), np.nan)

for i, nb in enumerate(nbasis_grid):
    for j, lam in enumerate(lambda_grid):
        try:
            _, _, gcv_val, _, _ = smooth_bspline(t_rel, scaled_returns, n_basis=nb, order=4, lam=lam, penalty_order=2)
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

# ============================================================
# 6. FIT SMOOTHED FUNCTIONAL OBJECTS
# ============================================================
coef, knots, _, B_mat, A_inv = smooth_bspline(
    t_rel, scaled_returns, n_basis=best_nbasis, order=4, lam=best_lambda, penalty_order=2
)
# coef shape: (n_basis, p)

def eval_fd(t_eval, coef, knots, order=4):
    t_eval  = np.atleast_1d(t_eval)
    n_basis = coef.shape[0]
    domain  = (knots[order-1], knots[-order])
    B, _    = make_bspline_basis_matrix(t_eval, n_basis, order, domain)
    return B @ coef

# Helper: evaluate all curves on a fine grid
plot_days = np.arange(t_rel.min(), t_rel.max() + 1, dtype=float)
lgp       = eval_fd(plot_days, coef, knots)   # (n_days, p)

t_seq = plot_days.copy()
curves_eval = lgp.T

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
# 10. FUNCTIONAL DEPTH ANALYSIS, OUTLIERS, BOXPLOTS
# ============================================================

tt = plot_days
X = lgp.T   # rows = commodities, columns = time points

fdataobj = FDataGrid(
    data_matrix=X,
    grid_points=tt
)


def fraiman_muniz_depth(X):
    n, T = X.shape
    depth = np.zeros(n)

    for t in range(T):
        ranks = np.argsort(np.argsort(X[:, t])) + 1
        F = ranks / n
        depth += 1 - np.abs(F - 0.5)

    return depth / T

def modal_depth(X, tt=None, h=None):
    n, T = X.shape
    w = np.gradient(tt) if tt is not None else np.ones(T) / T

    # pairwise L2 distances between curves
    D = np.sqrt(((X[:, None, :] - X[None, :, :])**2 * w).sum(axis=2))

    # automatic bandwidth
    if h is None:
        h = np.median(D[D > 0])
        if h == 0 or np.isnan(h):
            h = 1.0

    # Gaussian kernel modal depth
    return np.exp(-0.5 * (D / h)**2).mean(axis=1)

def random_projection_depth(X, n_proj=500, seed=42):
    rng = np.random.default_rng(seed)
    n, T = X.shape
    depths = np.zeros(n)

    for _ in range(n_proj):
        v = rng.normal(size=T)
        v /= np.linalg.norm(v)

        z = X @ v                      # project curves to 1D
        ranks = np.argsort(np.argsort(z)) + 1
        F = ranks / n

        depths += 1 - np.abs(F - 0.5)  # univariate depth

    return depths / n_proj

# Compute depth scores
depths = {
    "Depth": fraiman_muniz_depth(X),
    "mode Depth": modal_depth(X, tt),
    "RP Depth": random_projection_depth(X),
}


fig = plt.figure(figsize=(10, 7))

axes = [
    fig.add_axes([0.07, 0.58, 0.40, 0.32]),
    fig.add_axes([0.55, 0.58, 0.40, 0.32]),
    fig.add_axes([0.31, 0.16, 0.40, 0.32]),
]

labels = [
    ("FM.med", "FM.tr10%"),
    ("mode.med", "mode.tr10%"),
    ("RP.med", "RP.tr10%"),
]


for ax, (title, scores), (med_label, trim_label) in zip(axes, depths.items(), labels):

    scores = np.asarray(scores)

    # deepest curve = functional median
    median_idx = np.argmax(scores)

    # trim = 0.1, keep deepest 90%
    order = np.argsort(scores)[::-1]
    keep_idx = order[:int(0.9 * len(order))]

    # plot all curves
    for i in range(X.shape[0]):
        ax.plot(tt, X[i], color="black", lw=0.6, alpha=0.35)

    # red = median/deepest curve
    ax.plot(
        tt,
        X[median_idx],
        color="red",
        lw=2.3,
        label=med_label
    )

    # blue = 10% trimmed mean curve
    ax.plot(
        tt,
        X[keep_idx].mean(axis=0),
        color="blue",
        lw=2.3,
        label=trim_label
    )

    ax.set_title(title, fontweight="bold")
    ax.set_xlabel("t")
    ax.set_ylabel("X(t)")
    ax.legend(frameon=False, fontsize=8)

plt.savefig("plot_functional_depth_comparison.png", dpi=300, bbox_inches="tight")
plt.show()

################functional depth and outlier detection####################

# ------------------------------------------------------------
# Functional boxplot depth
# ------------------------------------------------------------
depth = ModifiedBandDepth()(fdataobj)

# Functional median = deepest curve
med_idx = np.argmax(depth)

# Central 50% curves
order = np.argsort(depth)[::-1]
n_central = int(np.ceil(0.5 * len(order)))
central_idx = order[:n_central]

central_curves = X[central_idx]
median_curve = X[med_idx]

# Functional boxplot envelope and fences
env_low = central_curves.min(axis=0)
env_high = central_curves.max(axis=0)

fence_low = median_curve - 1.5 * (env_high - env_low)
fence_high = median_curve + 1.5 * (env_high - env_low)

# Outliers: curves outside fences
out_idx = np.where(
    np.any((X < fence_low) | (X > fence_high), axis=1)
)[0]

depth_series = pd.Series(depth, index=commodities)

print("depth")
print(depth_series.to_string())

print("\n$outpoint")
print(out_idx + 1)        # +1 because R indexing starts at 1

print("\n$medcurve")
print(pd.Series([med_idx + 1], index=[commodities[med_idx]]))



#MUOD detection:
def boxplot_outliers(x):
    q1, q3 = np.percentile(x, [25, 75])
    iqr = q3 - q1
    return np.where((x < q1 - 1.5 * iqr) | (x > q3 + 1.5 * iqr))[0]


def muod(X):
    mean_curve = X.mean(axis=0)

    # MUOD-style indices
    magnitude = X.mean(axis=1) - mean_curve.mean()
    amplitude = X.std(axis=1)
    shape = np.array([
        1 - np.corrcoef(x, mean_curve)[0, 1]
        for x in X
    ])

    return shape, amplitude, magnitude


# equivalent to t(lgp)
X = lgp.T

shape, amplitude, magnitude = muod(X)

out_shape = boxplot_outliers(shape)
out_amplitude = boxplot_outliers(amplitude)
out_magnitude = boxplot_outliers(magnitude)

print("m$outliers")

print("$shape")
if len(out_shape) == 0:
    print("integer(0)")
else:
    print(out_shape + 1)   # R-style indexing

print("\n$amplitude")
if len(out_amplitude) == 0:
    print("integer(0)")
else:
    print(out_amplitude + 1)

print("\n$magnitude")
if len(out_magnitude) == 0:
    print("integer(0)")
else:
    print(out_magnitude + 1)


#plot everything:
fbp_outliers = [9, 11, 14, 15, 16, 18]   # functional boxplot outliers
muod_magnitude = [14, 15, 18]            # MUOD magnitude outliers
median_curve = 3                         # functional median

# Convert to Python indices
fbp_idx = [i - 1 for i in fbp_outliers]
muod_idx = [i - 1 for i in muod_magnitude]
median_idx = median_curve - 1

# Curves to highlight
highlight_idx = sorted(set(fbp_idx + muod_idx + [median_idx]))

# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------
fig, ax = plt.subplots(figsize=(12, 6))

# Plot all curves in grey
for i in range(X.shape[0]):
    ax.plot(tt, X[i], color="grey", lw=0.6, alpha=0.35)

# Colors for highlighted curves
colors = {
    8: "brown",      # 10-Year Treasury Note
    10: "green",    # Aluminum
    13: "orange",   # Natural Gas
    14: "red",      # Wheat
    15: "purple",   # Corn
    17: "darkorange", # Coffee
}

# Plot functional boxplot outliers
for i in fbp_idx:
    label = f"{commodities[i]}: functional boxplot outlier"

    if i in muod_idx:
        label = f"{commodities[i]}: functional boxplot + MUOD magnitude outlier"

    ax.plot(
        tt,
        X[i],
        lw=2.5,
        color=colors.get(i, None),
        label=label
    )

# Plot functional median
ax.plot(
    tt,
    X[median_idx],
    color="blue",
    lw=3,
    label=f"{commodities[median_idx]}: functional median"
)

# If any MUOD outlier was not already functional boxplot outlier, plot it separately
for i in muod_idx:
    if i not in fbp_idx:
        ax.plot(
            tt,
            X[i],
            lw=2.5,
            color=colors.get(i, None),
            label=f"{commodities[i]}: MUOD magnitude outlier"
        )

# ------------------------------------------------------------
# Labels and style
# ------------------------------------------------------------
ax.set_xlabel("Days relative to tariff announcement")
ax.set_ylabel("Standardized log return (smoothed)")

ax.legend(
    loc="upper left",
    bbox_to_anchor=(1.01, 1),
    frameon=False,
    fontsize=9
)

plt.tight_layout()
plt.savefig("plot_outlier_detection_python.png", dpi=300, bbox_inches="tight")
plt.show()



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

fig, axes = plt.subplots(2, 2, figsize=(10, 7))

for k, ax in enumerate(axes.flat):
    harmonic = fpca.components_.data_matrix[k, :, 0]

    ax.plot(plot_days, harmonic, color="black", lw=2)
    ax.axhline(0, color="grey", lw=0.8)

    ax.set_title(
        f"PCA function {k+1} "
        f"(Percentage of variability {explained[k]*100:.1f})",
        fontsize=10,
        fontweight="bold"
    )
    ax.set_xlabel("x")
    ax.set_ylabel(f"Harmonic {k+1}")

plt.tight_layout()
plt.savefig("plot_fpca_harmonics_R_style.png", dpi=300)
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
    return curves.var(axis=0, ddof=1)


def _mean_curve_eval(curves):
    return curves.mean(axis=0)


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
    B = np.sum(np.diag(Sigma) ** 2)

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

# ============================================================
# 17. VOLATILITY DATA LOADING
# ============================================================

path2 = r"Yfinance_close_prices_volatility.xlsx"
px2   = pd.read_excel(path2, sheet_name="Sheet1")
px2["Date"] = pd.to_datetime(px2["Date"])
px2   = px2.sort_values("Date").reset_index(drop=True)

existing_cols = [c for c in commodities if c in px2.columns]
px2 = px2[["Date"] + existing_cols]

returns_vol = px2.copy()
for col in existing_cols:
    returns_vol[col] = np.log(px2[col].astype(float)).diff()

returns_vol      = returns_vol.dropna().reset_index(drop=True)
dates_return_vol = returns_vol["Date"]
returns_mat      = returns_vol[existing_cols].values.astype(float)

# ============================================================
# 18. HISTORICAL VOLATILITY
# ============================================================

vol_window_start = event_date - pd.Timedelta(days=365)
vol_window_end   = event_date - pd.Timedelta(days=21)

idx_vol = np.where(
    (dates_return_vol >= vol_window_start) &
    (dates_return_vol <= vol_window_end)
)[0]

print(f"Volatility window: {vol_window_start.date()} to {vol_window_end.date()} "
      f"({len(idx_vol)} trading days)")

# Historical annualised volatility
hist_vol = pd.Series(
    returns_mat[idx_vol, :].std(axis=0, ddof=1) * np.sqrt(252),
    index=existing_cols
)

print("\nHistorical annualised volatility per commodity:")
print(hist_vol[commodities].sort_values(ascending=False).round(4).to_string())

# Standardised volatility (z-scores)
hist_vol_z = (hist_vol - hist_vol.mean()) / hist_vol.std(ddof=1)

_R_metals_agri = {
    "Gold", "Silver", "Palladium", "Platinum",
    "Copper", "Aluminum", "Gasoline", "Crude_Oil",
    "Wheat", "Corn", "Soybeans", "Coffee", "Sugar", "Cotton"
}

_anchor_mask   = np.array([c in _R_metals_agri for c in commodities])
_anchor_labels = hc_labels[_anchor_mask]
_dominant      = int(np.bincount(_anchor_labels - 1).argmax()) + 1

if _dominant == 1:
    _aligned_labels = hc_labels.copy()
    print("[Cluster align] Python cluster 1 = R Metals_Agri — no changes.")
else:
    _aligned_labels = np.where(hc_labels == 1, 2, 1)
    print("[Cluster align] Clusters aligned.")

print("\nClusters:")
for lbl, name in {1: "Metals_Agri", 2: "Currencies_Soft"}.items():
    members = [commodities[i] for i, c in enumerate(_aligned_labels) if c == lbl]
    print(f"  {name} ({len(members)}): {members}")

# ============================================================
# 19. SCALAR PREDICTOR DATA FRAME
# ============================================================

scalar_df = pd.DataFrame({
    "commodity"     : commodities,
    "cluster"       : pd.Categorical(
                        ["Metals_Agri" if _aligned_labels[i] == 1
                         else "Currencies_Soft"
                         for i in range(len(commodities))],
                        categories=["Metals_Agri", "Currencies_Soft"]
                      ),
    "vol_z"         : hist_vol_z[commodities].values,
    "cluster_dummy" : (_aligned_labels == 2).astype(int), 
})

print("\nScalar predictor data frame:")
print(scalar_df.to_string(index=False))

# ============================================================
# 20. EVALUATE FUNCTIONAL RESPONSES ON COMMON GRID
# ============================================================

yindex = plot_days.copy()  
n_time = len(yindex)

Y_mat = lgp.T.copy()          

scalar_df["Y"] = list(Y_mat)    

def _make_bspline_basis(t_grid, n_basis, order=4):
    t_min, t_max = t_grid.min(), t_grid.max()
    n_interior   = n_basis - order
    interior     = np.linspace(t_min, t_max, n_interior + 2)[1:-1]
    knots        = np.concatenate([
        np.repeat(t_min, order),
        interior,
        np.repeat(t_max, order)
    ])
    B = np.zeros((len(t_grid), n_basis))
    for i in range(n_basis):
        c = np.zeros(n_basis)
        c[i] = 1.0
        B[:, i] = BSpline(knots, c, order - 1)(t_grid)
    return B, knots


def _diff_penalty(n_basis, diff_order=2):
    D = np.diff(np.eye(n_basis), n=diff_order, axis=0)
    return D.T @ D


def _r2(y, y_hat, n, p):
    ss_res = np.sum((y - y_hat) ** 2)
    ss_tot = np.sum((y - y.mean()) ** 2)
    r2     = 1 - ss_res / ss_tot if ss_tot > 1e-12 else 0.0
    r2_adj = 1 - (1 - r2) * (n - 1) / max(n - p - 1, 1)
    return r2, r2_adj


# ============================================================
# 21. PENALISED FUNCTION-ON-SCALAR REGRESSION  (pffr)
# ============================================================

def pffr_pointwise(Y_mat, scalar_df, yindex, n_basis_smooth=15, lam=1e-3):

    n_comm, n_time = Y_mat.shape
    n_pred         = 3   # intercept + cluster + vol_z

    X = np.column_stack([
        np.ones(n_comm),
        scalar_df["cluster_dummy"].values,
        scalar_df["vol_z"].values
    ])

    beta_raw  = np.zeros((n_pred, n_time))
    se_hat    = np.zeros((n_pred, n_time))
    r2_vec    = np.zeros(n_time)

    XtX_inv = np.linalg.pinv(X.T @ X)

    for t_idx in range(n_time):
        y_t                = Y_mat[:, t_idx]
        b                  = XtX_inv @ X.T @ y_t
        beta_raw[:, t_idx] = b

        y_hat  = X @ b
        resid  = y_t - y_hat
        s2     = np.sum(resid ** 2) / max(n_comm - n_pred, 1)
        se_hat[:, t_idx] = np.sqrt(np.diag(XtX_inv) * s2)

        ss_tot = np.sum((y_t - y_t.mean()) ** 2)
        ss_res = np.sum(resid ** 2)
        r2_vec[t_idx] = 1 - ss_res / ss_tot if ss_tot > 0 else 0.0

    Phi, _ = _make_bspline_basis(yindex, n_basis_smooth, order=4)
    P      = _diff_penalty(n_basis_smooth, diff_order=2)
    A      = Phi.T @ Phi + lam * P

    beta_hat = np.zeros_like(beta_raw)
    for k in range(n_pred):
        c           = np.linalg.solve(A, Phi.T @ beta_raw[k])
        beta_hat[k] = Phi @ c

    mean_r2_adj = float(np.mean(
        1 - (1 - r2_vec) * (n_comm - 1) / max(n_comm - n_pred - 1, 1)
    ))

    # ── Parametric coefficients at t=0 ──
    t0_idx   = int(np.argmin(np.abs(yindex)))
    df_resid = max(n_comm - n_pred, 1)

    param_names = ["(Intercept)", "clusterCurrencies_Soft", "vol_z"]
    estimates   = beta_hat[:, t0_idx]
    std_errors  = se_hat[:, t0_idx]
    t_vals      = estimates / (std_errors + 1e-12)
    p_vals      = 2 * (1 - t.cdf(np.abs(t_vals), df=df_resid))

    param_table = pd.DataFrame({
        "Estimate"  : estimates.round(6),
        "Std. Error": std_errors.round(6),
        "t value"   : t_vals.round(4),
        "Pr(>|t|)"  : p_vals.round(4),
    }, index=param_names)

    # ── Smooth term summary ──
    H_smooth  = Phi @ np.linalg.solve(A, Phi.T)
    edf_total = float(np.trace(H_smooth))
    edf_per   = edf_total / n_pred

    smooth_names = [
        "Intercept(yindex)",
        "clusterCurrencies_Soft(yindex)",
        "vol_z(yindex)"
    ]

    f_stats = np.array([
        float(np.mean(beta_hat[k]**2) / (np.mean(se_hat[k]**2) + 1e-12))
        for k in range(n_pred)
    ])
    edf_each = np.array([edf_per] * n_pred)

    from scipy.stats import f as f_dist
    p_smooth = np.array([
        float(1 - f_dist.cdf(f_stats[k], dfn=edf_each[k], dfd=df_resid))
        for k in range(n_pred)
    ])

    smooth_table = pd.DataFrame({
        "edf"    : edf_each.round(3),
        "F"      : f_stats.round(3),
        "p-value": p_smooth.round(6),
    }, index=smooth_names)

    # ── Model fit ──
    ss_res_total = float(np.sum((Y_mat - X @ beta_raw)**2))
    ss_tot_total = float(np.sum((Y_mat - Y_mat.mean())**2))
    r2_global    = 1 - ss_res_total / ss_tot_total
    n_obs        = n_comm * n_time
    dev_expl     = r2_global * 100
    scale_est    = ss_res_total / max(n_obs - n_pred, 1)

    return (beta_hat, se_hat, r2_vec, mean_r2_adj,
            param_table, smooth_table,
            r2_global, mean_r2_adj, dev_expl, scale_est, n_obs)


beta_hat, se_hat, r2_vec, fosr_r2_adj, \
param_tbl, smooth_tbl, \
r2_global, r2_adj_global, dev_expl, scale_est, n_obs = pffr_pointwise(
    Y_mat, scalar_df, yindex, n_basis_smooth=15, lam=1e-3
)

print(f"\npffr equivalent — mean R²(adj): {fosr_r2_adj:.4f}")

print("\nParametric Coefficients (evaluated at t=0):")
print(param_tbl.to_string())

print("\nSmooth and Functional Coefficients:")
print(smooth_tbl.to_string())

print(f"\nModel Fit Statistics:")
print(f"  Adjusted R²        : {r2_adj_global:.4f}")
print(f"  Deviance explained : {dev_expl:.1f}%")
print(f"  Scale estimate     : {scale_est:.6f}")
print(f"  Observations (n)   : {n_obs} ({n_obs // len(yindex)} x {len(yindex)})")

labels_beta = [
    r"$\hat{\beta}_0(t)$ – Functional Intercept",
    r"$\hat{\beta}_1(t)$ – Cluster effect (Currencies_Soft)",
    r"$\hat{\beta}_2(t)$ – Historical volatility effect",
]
colors_beta = ["black", "steelblue", "darkorange"]

fig, axes = plt.subplots(3, 1, figsize=(12, 10), sharex=True)

for k, ax in enumerate(axes):
    b  = beta_hat[k]
    se = se_hat[k]
    ax.plot(yindex, b, color=colors_beta[k], lw=2, label=labels_beta[k])
    ax.fill_between(yindex, b - 1.96 * se, b + 1.96 * se,
                    alpha=0.2, color=colors_beta[k], label="95% pointwise CI")
    ax.axhline(0, color="grey", lw=0.8, ls="--")
    ax.axvline(0, color="red",  lw=1.2, ls="--", label="t=0 (tariff)")
    ax.set_ylabel(f"beta_{k}(t)")
    ax.set_title(labels_beta[k])
    ax.legend(fontsize=8, frameon=False)

axes[-1].set_xlabel("Days relative to tariff announcement")
plt.suptitle("FoSR: Pointwise coefficient functions", fontsize=13, fontweight="bold")
plt.tight_layout()
plt.savefig("plot_fosr_coefficients.png", dpi=150)
plt.show()
print("Saved plot_fosr_coefficients.png")


# ============================================================
# 22. BAYESIAN FUNCTION-ON-SCALAR REGRESSION
# ============================================================

def bayes_fosr_approx(Y_mat, scalar_df, yindex,
                      alpha_ridge=0.0, n_basis_smooth=10, lam=1e-3):
   
    n_comm, n_time = Y_mat.shape
    n_pred         = 3

    X = np.column_stack([
        np.ones(n_comm),
        scalar_df["cluster_dummy"].values,
        scalar_df["vol_z"].values
    ])

    XtX     = X.T @ X + alpha_ridge * np.eye(n_pred)
    XtX_inv = np.linalg.inv(XtX)

    beta_raw = np.zeros((n_pred, n_time))
    for t_idx in range(n_time):
        beta_raw[:, t_idx] = XtX_inv @ X.T @ Y_mat[:, t_idx]

    Phi, _ = _make_bspline_basis(yindex, n_basis_smooth, order=4)
    P      = _diff_penalty(n_basis_smooth, diff_order=2)
    A      = Phi.T @ Phi + lam * P

    beta_hat = np.zeros_like(beta_raw)
    for k in range(n_pred):
        c           = np.linalg.solve(A, Phi.T @ beta_raw[k])
        beta_hat[k] = Phi @ c

    return beta_hat


bayes_default = bayes_fosr_approx(Y_mat, scalar_df, yindex,
                                   alpha_ridge=0.01, n_basis_smooth=15)
bayes_VB      = bayes_fosr_approx(Y_mat, scalar_df, yindex,
                                   alpha_ridge=0.1,  n_basis_smooth=10)
bayes_OLS     = bayes_fosr_approx(Y_mat, scalar_df, yindex,
                                   alpha_ridge=0.0,  n_basis_smooth=10)

models_bayes = {
    "default" : bayes_default,
    "VB"      : bayes_VB,
    "OLS"     : bayes_OLS,
}

coef_names  = [
    r"$\beta_0(t)$ – Functional Intercept",
    r"$\beta_1(t)$ – Cluster effect (Currencies_Soft dummy)",
    r"$\beta_2(t)$ – Historical volatility effect",
]
method_cols = {"default": "black", "VB": "steelblue", "OLS": "darkorange"}

fig, axes = plt.subplots(3, 1, figsize=(12, 10), sharex=True)

for k, ax in enumerate(axes):
    for method, beta in models_bayes.items():
        ax.plot(yindex, beta[k], lw=1.8,
                color=method_cols[method], label=method)
    ax.axhline(0, color="grey", lw=0.8, ls="--")
    ax.axvline(0, color="red",  lw=1.2, ls="--")
    ax.set_title(coef_names[k])
    ax.set_ylabel("beta(t)")
    ax.legend(fontsize=8, frameon=False)

axes[-1].set_xlabel("Days relative to tariff announcement")
plt.suptitle("Bayesian FoSR: coefficient comparison across methods",
             fontsize=13, fontweight="bold")
plt.tight_layout()
plt.savefig("plot_bayes_fosr_comparison.png", dpi=150)
plt.show()
print("Saved plot_bayes_fosr_comparison.png")


# ============================================================
# 23. SCALAR-ON-FUNCTION REGRESSION  (pfr)
# ============================================================

idx_post_sofr    = np.isin(t_rel, np.arange(1, 21))
post_mean_vec    = scaled_returns[idx_post_sofr, :].mean(axis=0)
scalar_df        = scalar_df.copy()
scalar_df["post_mean"]     = post_mean_vec
scalar_df["hist_vol_z"]    = hist_vol_z[commodities].values

# (atitinka R: pre_idx <- which(yindex < 0); cca_mat_pre <- Y_mat[, pre_idx])
pre_idx     = np.where(yindex < 0)[0]
cca_mat_pre = Y_mat[:, pre_idx]       # (n_comm, n_pre)
yindex_pre  = yindex[pre_idx]


def pfr_sofr(cca_mat_pre, scalar_df, response_col,
             extra_scalar_cols=None,
             yindex_pre=None,
             k=15,
             lam=None):
 
    n_comm, n_pre = cca_mat_pre.shape

    if yindex_pre is None:
        yindex_pre = np.linspace(0, 1, n_pre)

    y = scalar_df[response_col].values.astype(float)

    Phi, _  = _make_bspline_basis(yindex_pre, k, order=4)
    P       = _diff_penalty(k, diff_order=2)

    
    dt     = np.gradient(yindex_pre)
    W_Phi  = Phi * dt[:, None]        # (n_pre, k)
    V      = cca_mat_pre @ W_Phi      # (n_comm, k)


    ones = np.ones((n_comm, 1))
    if extra_scalar_cols:
        Z_sc     = scalar_df[extra_scalar_cols].values.astype(float)
        X_design = np.hstack([ones, V, Z_sc])
        n_scalar = Z_sc.shape[1]
    else:
        X_design = np.hstack([ones, V])
        n_scalar = 0

    n_total = X_design.shape[1]


    P_full = np.zeros((n_total, n_total))
    P_full[1:1 + k, 1:1 + k] = P   # λ pridedama žemiau

    if lam is None:
        lam_grid   = np.logspace(-4, 4, 100)
        gcv_scores = np.zeros(len(lam_grid))

        for j, lam_try in enumerate(lam_grid):
            A_try   = X_design.T @ X_design + lam_try * P_full
            try:
                coef_try = np.linalg.solve(A_try, X_design.T @ y)
            except np.linalg.LinAlgError:
                gcv_scores[j] = np.inf
                continue
            y_hat_try = X_design @ coef_try
            resid_try = y - y_hat_try
            # Hat matrica pėdsakas (efektyvūs laisvės laipsniai)
            try:
                H_try = X_design @ np.linalg.solve(A_try, X_design.T)
                edf_try = np.trace(H_try)
            except np.linalg.LinAlgError:
                gcv_scores[j] = np.inf
                continue
            denom = (1 - edf_try / n_comm) ** 2
            gcv_scores[j] = (np.sum(resid_try ** 2) / n_comm) / (
                denom if denom > 1e-10 else np.inf
            )

        best_lam = lam_grid[np.argmin(gcv_scores)]
    else:
        best_lam = lam


    A     = X_design.T @ X_design + best_lam * P_full
    coef  = np.linalg.solve(A, X_design.T @ y)
    y_hat = X_design @ coef

    # --- Efektyvūs laisvės laipsniai: edf = trace(H) ---
    # (atitinka R mgcv edf; skaičiuojama kaip trace(X (X'X+λP)^{-1} X'))
    H        = X_design @ np.linalg.solve(A, X_design.T)
    edf      = float(np.trace(H))                 
    df_resid = max(n_comm - edf, 1e-6)            


    ss_res = np.sum((y - y_hat) ** 2)
    ss_tot = np.sum((y - y.mean()) ** 2)
    r2     = 1 - ss_res / ss_tot if ss_tot > 1e-12 else 0.0
    r2_adj = 1 - (1 - r2) * (n_comm - 1) / df_resid

    s2    = ss_res / df_resid
    A_inv = np.linalg.pinv(A)
  
    se_all = np.sqrt(np.diag(A_inv) * s2)

    intercept = coef[0]
    c_beta    = coef[1:1 + k]
    beta_func = Phi @ c_beta           # β(t) = Φ(t) c

    if n_scalar > 0:
        c_scalar     = coef[1 + k:]
        scalar_coefs = pd.Series(c_scalar, index=extra_scalar_cols)
    else:
        scalar_coefs = pd.Series(dtype=float)


    param_names = ["(Intercept)"] + \
                  (extra_scalar_cols if extra_scalar_cols else [])
    param_idx   = [0] + list(range(1 + k, n_total))
    param_coefs = coef[param_idx]
    param_se    = se_all[param_idx]
    t_vals      = param_coefs / (param_se + 1e-12)
    p_vals      = 2 * (1 - t.cdf(np.abs(t_vals), df=df_resid))

    p_table = pd.DataFrame({
        "Estimate"  : param_coefs.round(6),
        "Std. Error": param_se.round(6),
        "t value"   : t_vals.round(4),
        "Pr(>|t|)"  : p_vals.round(4),
    }, index=param_names)

    return {
        "model_coef"  : coef,
        "r2"          : r2,
        "r2_adj"      : r2_adj,
        "edf"         : edf,
        "df_resid"    : df_resid,
        "scale"       : s2,
        "lam"         : best_lam,
        "beta_func"   : beta_func,
        "scalar_coefs": scalar_coefs,
        "p_table"     : p_table,
        "y_hat"       : y_hat,
        "intercept"   : intercept,
        "Phi"         : Phi,
        "c_beta"      : c_beta,
    }


# ============================================================
# 24.  POST-EVENT RETURN
# ============================================================

print("\nResponse: post-event mean return")

sofr_post = pfr_sofr(
    cca_mat_pre,
    scalar_df,
    response_col       = "post_mean",
    extra_scalar_cols  = ["cluster_dummy", "vol_z"],
    yindex_pre         = yindex_pre,
    k                  = min(15, len(yindex_pre) - 1),
    lam                = None,   
)

print(f"\nSummary:")
print(f"  R²(adj)    = {sofr_post['r2_adj']:.4f}   "
      f"Deviance explained = {sofr_post['r2']*100:.1f}%")
print(f"  edf        = {sofr_post['edf']:.3f}   "
      f"Scale est. = {sofr_post['scale']:.6f}   n = {len(scalar_df)}")
print(f"  lambda(GCV)= {sofr_post['lam']:.2e}")
print("\nParametric coefficients:")
print(sofr_post["p_table"].to_string())

fig, ax = plt.subplots(figsize=(9, 5))
ax.plot(yindex_pre, sofr_post["beta_func"], color="black", lw=2,
        label=r"$\hat{\beta}(t)$")
ax.axhline(0, color="grey", lw=0.8, ls="--")
ax.axvline(0, color="red",  lw=1.2, ls="--", label="t=0 (tariff)")
ax.set_xlabel("Days relative to tariff announcement")
ax.set_ylabel(r"$\hat{\beta}(t)$")
ax.set_title("SoFR: pre-event functional predictor")
ax.legend(fontsize=9, frameon=False)
plt.tight_layout()
plt.savefig("plot_sofr_post.png", dpi=150)
plt.show()
print("Saved plot_sofr_post.png")


# ============================================================
# 25. PREDICT HISTORICAL VOLATILITY
# ============================================================

print("\nResponse: historical volatility (standardised)")

sofr_vol = pfr_sofr(
    cca_mat_pre,
    scalar_df,
    response_col       = "hist_vol_z",
    extra_scalar_cols  = ["cluster_dummy"],
    yindex_pre         = yindex_pre,
    k                  = min(15, len(yindex_pre) - 1),
    lam                = None,   
)

print(f"\nSummary:")
print(f"  R²(adj)    = {sofr_vol['r2_adj']:.4f}   "
      f"Deviance explained = {sofr_vol['r2']*100:.1f}%")
print(f"  edf        = {sofr_vol['edf']:.3f}   "
      f"Scale est. = {sofr_vol['scale']:.6f}   n = {len(scalar_df)}")
print(f"  lambda(GCV)= {sofr_vol['lam']:.2e}")
print("\nParametric coefficients:")
print(sofr_vol["p_table"].to_string())

# Grafikas  (atitinka R: plot(sofr_vol, ...))
fig, ax = plt.subplots(figsize=(9, 5))
ax.plot(yindex_pre, sofr_vol["beta_func"], color="black", lw=2,
        label=r"$\hat{\beta}(t)$")
ax.axhline(0, color="grey", lw=0.8, ls="--")
ax.axvline(0, color="red",  lw=1.2, ls="--", label="t=0 (tariff)")
ax.set_xlabel("Days relative to tariff announcement")
ax.set_ylabel(r"$\hat{\beta}(t)$")
ax.set_title("SoFR: predicting historical volatility")
ax.legend(fontsize=9, frameon=False)
plt.tight_layout()
plt.savefig("plot_sofr_vol.png", dpi=150)
plt.show()
print("Saved plot_sofr_vol.png")

vol_table = scalar_df[["commodity", "cluster", "vol_z"]].copy()
vol_table["hist_vol_annualised"] = (hist_vol[commodities].values * 100).round(2)
vol_table = vol_table.sort_values(
    ["cluster", "hist_vol_annualised"], ascending=[True, False]
).reset_index(drop=True)

print("\n\n========== VOLATILITY SUMMARY TABLE ==========")
print(vol_table.to_string(index=False))

# BOXPLOT  (atitinka R: boxplot(hist_vol ~ factor(hc_clusters, ...)))
cluster_map   = {1: "Metals & Agri", 2: "Currencies & Soft"}
data_by_clust = [
    hist_vol[commodities][np.array(_aligned_labels) == k].values
    for k in [1, 2]
]

fig, ax = plt.subplots(figsize=(8, 5))
bp = ax.boxplot(data_by_clust, patch_artist=True,
                labels=list(cluster_map.values()))
bp["boxes"][0].set_facecolor("steelblue")
bp["boxes"][1].set_facecolor("tomato")

for k_idx, k in enumerate([1, 2]):
    mask  = np.array(_aligned_labels) == k
    xvals = np.random.normal(k_idx + 1, 0.04, size=mask.sum())
    ax.scatter(xvals, hist_vol[commodities][mask].values,
               color="black", s=30, zorder=5, alpha=0.7)

ax.set_ylabel("Annualised historical volatility")
ax.set_title("Historical volatility by cluster")
plt.tight_layout()
plt.savefig("plot_vol_boxplot.png", dpi=150)
plt.show()
print("Saved plot_vol_boxplot.png")


# ============================================================
# 26. SENSITIVITY ANALYSIS – EXCLUDING NATURAL GAS
# ============================================================

print("\n" + "=" * 60)
print("  SENSITIVITY ANALYSIS: EXCLUDING NATURAL GAS")
print("=" * 60)

ng_name  = "Natural_Gas"
keep_idx = [i for i, c in enumerate(commodities) if c != ng_name]
keep_nms = [commodities[i] for i in keep_idx]

print(f"Commodities retained: {len(keep_nms)}  (dropped: {ng_name})")
print("Note: original cluster labels preserved — no re-clustering.\n")

orig_clusters_sub = _aligned_labels[keep_idx]

print("Cluster membership (original labels, Natural Gas excluded):")
for lbl, name in {1: "Metals_Agri", 2: "Currencies_Soft"}.items():
    members = [keep_nms[i] for i, c in enumerate(orig_clusters_sub) if c == lbl]
    print(f"  {name}: {members}")
print()


hist_vol_sub   = hist_vol[keep_nms]
hist_vol_z_sub = (hist_vol_sub - hist_vol_sub.mean()) / hist_vol_sub.std(ddof=1)

scaled_ret_sub  = scaled_returns[:, keep_idx]
idx_post_sub    = np.isin(t_rel, np.arange(1, 21))
post_mean_sub   = scaled_ret_sub[idx_post_sub, :].mean(axis=0)

scalar_df_sub = pd.DataFrame({
    "commodity"     : keep_nms,
    "cluster"       : pd.Categorical(
                        ["Metals_Agri" if c == 1 else "Currencies_Soft"
                         for c in orig_clusters_sub],
                        categories=["Metals_Agri", "Currencies_Soft"]
                      ),
    "vol_z"         : hist_vol_z_sub[keep_nms].values,
    "cluster_dummy" : (orig_clusters_sub == 2).astype(int),
    "post_mean"     : post_mean_sub,
    "hist_vol_z"    : hist_vol_z_sub[keep_nms].values,
})

print("Scalar predictor data frame (sub-sample):")
print(scalar_df_sub[["commodity", "cluster", "vol_z"]].to_string(index=False))
print()


Y_mat_sub   = Y_mat[keep_idx, :]
scalar_df_sub["Y"] = list(Y_mat_sub)


print("--- pffr (Natural Gas excluded, original clusters) ---\n")

beta_sub, se_sub, r2_sub_vec, fosr_sub_r2_adj = pffr_pointwise(
    Y_mat_sub, scalar_df_sub, yindex, n_basis_smooth=15, lam=1e-3
)

print(f"pffr R²(adj): full = {fosr_r2_adj:.4f}  |  sub (no NatGas) = {fosr_sub_r2_adj:.4f}\n")

print("--- SoFR: predicting post-event return (Natural Gas excluded) ---\n")

pre_idx_sub     = np.where(yindex < 0)[0]
cca_mat_pre_sub = Y_mat_sub[:, pre_idx_sub]
yindex_pre_sub  = yindex[pre_idx_sub]

sofr_post_sub = pfr_sofr(
    cca_mat_pre_sub,
    scalar_df_sub,
    response_col       = "post_mean",
    extra_scalar_cols  = ["cluster_dummy", "vol_z"],
    yindex_pre         = yindex_pre_sub,
    k                  = min(15, len(yindex_pre_sub) - 1),
    lam                = None,   # GCV paieška
)

print("SoFR summary (post-event return, Natural Gas excluded):")
print(f"  R²(adj)    = {sofr_post_sub['r2_adj']:.4f}   "
      f"Deviance explained = {sofr_post_sub['r2']*100:.1f}%")
print(f"  edf        = {sofr_post_sub['edf']:.3f}   "
      f"Scale est. = {sofr_post_sub['scale']:.6f}   n = {len(scalar_df_sub)}")
print(f"  lambda(GCV)= {sofr_post_sub['lam']:.2e}")
print("\nParametric coefficients:")
print(sofr_post_sub["p_table"].to_string())

fig, ax = plt.subplots(figsize=(9, 5))
ax.plot(yindex_pre_sub, sofr_post_sub["beta_func"],
        color="black", lw=2, label=r"$\hat{\beta}(t)$")
ax.axhline(0, color="grey", lw=0.8, ls="--")
ax.axvline(0, color="red",  lw=1.2, ls="--", label="t=0 (tariff)")
ax.set_xlabel("Days relative to tariff announcement")
ax.set_ylabel(r"$\hat{\beta}(t)$")
ax.set_title("SoFR – pre-event predictor (Natural Gas excluded)")
ax.legend(fontsize=9, frameon=False)
plt.tight_layout()
plt.savefig("plot_sofr_post_sub.png", dpi=150)
plt.show()

# -------------------------------------------------------
# 27. COEFFICIENT COMPARISON: full vs sub-sample
# -------------------------------------------------------
print("\n--- Parametric coefficient comparison: full vs sub-sample ---\n")

param_full = sofr_post["p_table"]
param_sub  = sofr_post_sub["p_table"]

common_terms = param_full.index.intersection(param_sub.index)

comparison = pd.DataFrame({
    "Term"     : common_terms,
    "Est_full" : param_full.loc[common_terms, "Estimate"].values.round(4),
    "pval_full": param_full.loc[common_terms, "Pr(>|t|)"].values.round(4),
    "Est_sub"  : param_sub.loc[common_terms,  "Estimate"].values.round(4),
    "pval_sub" : param_sub.loc[common_terms,  "Pr(>|t|)"].values.round(4),
})

print(comparison.to_string(index=False))

print("\nRobustness check (same sign AND p < 0.05 in both?):")
for _, row in comparison.iterrows():
    sig_full  = row["pval_full"] < 0.05
    sig_sub   = row["pval_sub"]  < 0.05
    same_sign = np.sign(row["Est_full"]) == np.sign(row["Est_sub"])
    robust    = sig_full and sig_sub and same_sign
    if robust:
        status = "ROBUST"
    elif same_sign:
        status = "same sign, significance changed"
    else:
        status = "CHANGED"
    print(f"  {row['Term']:<30}: {status}")

print("\n" + "=" * 60)
print("  SENSITIVITY SUMMARY")
print("=" * 60)

print(f"pffr  R²(adj): full = {fosr_r2_adj:.3f}  |  sub (no NatGas) = {fosr_sub_r2_adj:.3f}")
print(f"SoFR  R²(adj): full = {sofr_post['r2_adj']:.3f}  |  sub (no NatGas) = {sofr_post_sub['r2_adj']:.3f}")
print(f"SoFR  R²:      full = {sofr_post['r2']:.3f}  |  sub (no NatGas) = {sofr_post_sub['r2']:.3f}")
print("Clustering: original labels retained — no reassignment.")
print(f"Cluster sizes: Metals_Agri = {(orig_clusters_sub == 1).sum()}, "
      f"Currencies_Soft = {(orig_clusters_sub == 2).sum()}")