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

from skfda.exploratory.depth import ModifiedBandDepth
from skfda.exploratory.visualization import Boxplot
from skfda.exploratory.outliers import BoxplotOutlierDetector

from skfda.exploratory.depth import IntegratedDepth

from skfda.preprocessing.registration.validation import AmplitudePhaseDecomposition
from skfda.preprocessing.registration import FisherRaoElasticRegistration

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
