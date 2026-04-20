# =============================================================================
# SETUP
# =============================================================================
import os
import sys

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler

# scikit-fda (pip install scikit-fda)
import skfda
from skfda.representation.basis import BSplineBasis
from skfda.preprocessing.smoothing import BasisSmoother


# =============================================================================
# HELPERS
# =============================================================================

def load_price_data(path, sheet_name="Close_prices"):
    if not os.path.exists(path):
        raise FileNotFoundError(f"Data file not found: {path}")

    df = pd.read_excel(path, sheet_name=sheet_name)
    if "Date" not in df.columns:
        raise ValueError("The dataset must contain a 'Date' column.")

    df["Date"] = pd.to_datetime(df["Date"])
    df = df.sort_values("Date").reset_index(drop=True)

    prices = df.drop(columns=["Date"]).astype(float)
    if prices.isna().any().any():
        raise ValueError("Price data contains missing values. Clean or impute before analysis.")
    if (prices <= 0).any().any():
        raise ValueError("Price data must be strictly positive for log transformation.")

    return df["Date"], prices.values, prices.columns.tolist()


def compute_log_returns(prices):
    log_prices = np.log(prices)
    return np.diff(log_prices, axis=0)


def build_relative_day_axis(dates, event_date):
    if not (dates.min() <= event_date <= dates.max()):
        raise ValueError("Event date is outside the available price history.")
    return (dates - event_date).dt.days.values


def ensure_window_has_data(mask, name):
    if not mask.any():
        raise ValueError(f"No observations found in the {name} window.")


def select_smoothing_parameters(data_matrix, grid_points, nbasis_grid, lambda_grid):
    gcv_mat = np.zeros((len(nbasis_grid), len(lambda_grid)))

    for i, nbasis in enumerate(nbasis_grid):
        basis = BSplineBasis(
            domain_range=(grid_points.min(), grid_points.max()),
            n_basis=nbasis,
            order=4,
        )

        for j, lam in enumerate(lambda_grid):
            smoother = BasisSmoother(
                basis=basis,
                smoothing_parameter=lam,
            )
            fd = skfda.FDataGrid(data_matrix=data_matrix, grid_points=grid_points)
            fd_smooth = smoother.fit_transform(fd)
            residuals = data_matrix - fd_smooth.data_matrix[..., 0]
            gcv_mat[i, j] = np.mean(residuals**2)

    best_idx = np.unravel_index(np.argmin(gcv_mat), gcv_mat.shape)
    return nbasis_grid[best_idx[0]], lambda_grid[best_idx[1]], gcv_mat


def plot_fd(fd, xlabel, ylabel, title=""):
    plt.figure(figsize=(10, 6))
    fd.plot()
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.title(title)
    plt.tight_layout()
    plt.show()


# =============================================================================
# MAIN WORKFLOW
# =============================================================================

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    data_path = os.path.join(script_dir, "Yfinance_close_prices.xlsx")
    event_date = pd.to_datetime("2025-04-02")

    dates, prices, asset_names = load_price_data(data_path)
    returns = compute_log_returns(prices)
    dates_return = dates.iloc[1:]

    scaler = StandardScaler()
    scaled_returns = scaler.fit_transform(returns)

    t_rel = build_relative_day_axis(dates_return, event_date)
    if len(t_rel) != scaled_returns.shape[0]:
        raise RuntimeError("Relative time axis length does not match number of return observations.")

    fd_data = skfda.FDataGrid(data_matrix=scaled_returns.T, grid_points=t_rel)
    plot_fd(
        fd_data,
        xlabel="Days relative to event",
        ylabel="Scaled log return",
        title="Scaled returns as functional curves",
    )

    nbasis_grid = np.arange(7, 11)
    lambda_grid = np.logspace(-2, 2, 40)
    best_nbasis, best_lambda, gcv_mat = select_smoothing_parameters(
        data_matrix=scaled_returns.T,
        grid_points=t_rel,
        nbasis_grid=nbasis_grid,
        lambda_grid=lambda_grid,
    )

    print(f"Best nbasis: {best_nbasis}")
    print(f"Best lambda: {best_lambda:.4g}")

    plt.figure(figsize=(10, 6))
    for i, nbasis in enumerate(nbasis_grid):
        plt.plot(np.log10(lambda_grid), gcv_mat[i, :], label=f"nbasis={nbasis}")
    plt.xlabel("log10(lambda)")
    plt.ylabel("Mean squared residual")
    plt.legend()
    plt.tight_layout()
    plt.show()

    basis = BSplineBasis(
        domain_range=(t_rel.min(), t_rel.max()),
        n_basis=best_nbasis,
        order=4,
    )
    smoother = BasisSmoother(basis=basis, smoothing_parameter=best_lambda)
    fd_smooth = smoother.fit_transform(
        skfda.FDataGrid(data_matrix=scaled_returns.T, grid_points=t_rel)
    )

    plot_fd(
        fd_smooth,
        xlabel="Days relative to event",
        ylabel="Scaled log return (smoothed)",
        title="Smoothed functional curves",
    )
    plt.axvline(x=0, color="red", linestyle="--", linewidth=2)
    plt.show()

    pre_mask = (t_rel >= -20) & (t_rel <= -1)
    post_mask = (t_rel >= 1) & (t_rel <= 20)
    ensure_window_has_data(pre_mask, "pre-event")
    ensure_window_has_data(post_mask, "post-event")

    pre_mean = scaled_returns[pre_mask].mean(axis=0)
    post_mean = scaled_returns[post_mask].mean(axis=0)
    result = pd.DataFrame(
        {"pre_mean": pre_mean, "post_mean": post_mean},
        index=asset_names,
    )

    print("\nPre/post mean scaled returns:")
    print(result)


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        sys.exit(1)

# =============================================================================
# SMOOTHING + GCV SELECTION
# =============================================================================

nbasis_grid = np.arange(7, 11)
lambda_grid = np.logspace(-2, 2, 40)

gcv_mat = np.zeros((len(nbasis_grid), len(lambda_grid)))

for i, nbasis in enumerate(nbasis_grid):

    basis = BSplineBasis(
        domain_range=range_t,
        n_basis=nbasis,
        order=4  # cubic splines
    )

    for j, lam in enumerate(lambda_grid):

        smoother = BasisSmoother(
            basis=basis,
            smoothing_parameter=lam
        )

        fd_smooth = smoother.fit_transform(
            skfda.FDataGrid(
                data_matrix=scaled_returns,
                grid_points=t_rel
            )
        )

        # Approximate GCV (scikit-fda does not directly expose same GCV as R)
        residuals = scaled_returns - fd_smooth.evaluate(t_rel)[:, :, 0]
        gcv_mat[i, j] = np.sum(residuals**2)

# Best parameters
idx = np.unravel_index(np.argmin(gcv_mat), gcv_mat.shape)
best_nbasis = nbasis_grid[idx[0]]
best_lambda = lambda_grid[idx[1]]

print("Best nbasis:", best_nbasis)
print("Best lambda:", best_lambda)

# Plot GCV curves
for i, nbasis in enumerate(nbasis_grid):
    plt.plot(np.log10(lambda_grid), gcv_mat[i, :], label=f"nbasis={nbasis}")

plt.xlabel("log10(lambda)")
plt.ylabel("GCV (approx)")
plt.legend()
plt.show()

# =============================================================================
# FINAL SMOOTHING
# =============================================================================

basis = BSplineBasis(
    domain_range=range_t,
    n_basis=best_nbasis,
    order=4
)

smoother = BasisSmoother(
    basis=basis,
    smoothing_parameter=best_lambda
)

fd_smooth = smoother.fit_transform(
    skfda.FDataGrid(
        data_matrix=scaled_returns,
        grid_points=t_rel
    )
)

# Plot smoothed curves
fd_smooth.plot()
plt.xlabel("Days relative to tariff announcement")
plt.ylabel("Scaled log return (smoothed)")
plt.axvline(x=0, color='red', linestyle='--', linewidth=2)
plt.show()

# =============================================================================
# PRE / POST ANALYSIS
# =============================================================================

pre_days = np.arange(-20, 0)
post_days = np.arange(1, 21)

idx_pre = np.isin(t_rel, pre_days)
idx_post = np.isin(t_rel, post_days)

pre_mean = scaled_returns[idx_pre].mean(axis=0)
post_mean = scaled_returns[idx_post].mean(axis=0)

result = pd.DataFrame({
    "pre_mean": pre_mean,
    "post_mean": post_mean
}, index=colnames)

print(result)