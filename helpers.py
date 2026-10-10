"""Helper functions shared by the notebooks.

The first group are Python versions of the helper scripts that came with the
R code (li.r, meanlike.r, msmooth.r, binormal.r and arsim.dat).  The second
group are small stand-ins for R built-ins (approx, qqnorm, qqline, cut, ...)
that have no exact one-line equivalent in numpy/scipy.
"""

from pathlib import Path

import numpy as np
from scipy import stats

DATA = Path(__file__).resolve().parent / "data"


# ---------------------------------------------------------------- R built-ins

def scan(name, skip=0, nlines=None):
    """All the numbers in data/<name>, like R's scan(name, skip=, nlines=)."""
    lines = (DATA / name).read_text().splitlines()[skip:]
    if nlines is not None:
        lines = lines[:nlines]
    return np.array(" ".join(lines).split(), dtype=float)


def approx(x, y, xout):
    """Linear interpolation like R's approx(x, y, xout)$y.

    Points are sorted by x (tied x values are averaged) and xout values
    outside the range of x give NaN, as in R (rule=1).
    """
    x = np.asarray(x, dtype=float)
    y = np.asarray(y, dtype=float)
    ux, inv = np.unique(x, return_inverse=True)
    uy = np.bincount(inv, weights=y) / np.bincount(inv)
    return np.interp(xout, ux, uy, left=np.nan, right=np.nan)


def ppoints(n):
    """R's ppoints(n): probability points for normal plots."""
    a = 3 / 8 if n <= 10 else 1 / 2
    return (np.arange(1, n + 1) - a) / (n + 1 - 2 * a)


def qqnorm(x):
    """R's qqnorm(x, plot=F): returns (theoretical, sample) in the original order."""
    x = np.asarray(x, dtype=float)
    ranks = stats.rankdata(x, method="ordinal").astype(int) - 1
    return stats.norm.ppf(ppoints(len(x)))[ranks], x


def qqline(ax, x, **kwargs):
    """R's qqline(x): line through the first and third quartiles."""
    y = np.quantile(x, [0.25, 0.75])
    z = stats.norm.ppf([0.25, 0.75])
    slope = (y[1] - y[0]) / (z[1] - z[0])
    ax.axline((z[0], y[0]), slope=slope, **kwargs)


def cut(x, breaks):
    """Bin index like R's cut(x, breaks): intervals (b[i], b[i+1]].

    Values outside the breaks get index -1 (NA in R).
    """
    breaks = np.asarray(breaks, dtype=float)
    idx = np.searchsorted(breaks, x, side="left") - 1
    idx[(idx < 0) | (idx >= len(breaks) - 1)] = -1
    return idx


def table(idx, nbins):
    """Counts per bin of cut(): R's as.numeric(table(cut(...)))."""
    return np.bincount(idx[idx >= 0], minlength=nbins).astype(float)


def bin_means(y, idx, nbins):
    """R's tapply(y, cutx, mean), with empty bins set to 0."""
    w = table(idx, nbins)
    s = np.bincount(idx[idx >= 0], weights=np.asarray(y)[idx >= 0], minlength=nbins)
    return np.divide(s, w, out=np.zeros(nbins), where=w > 0)


def density(x, bw, n=512, cut=3):
    """Gaussian kernel density estimate like R's density(x, bw=bw).

    bw is the standard deviation of the kernel. Returns (grid, density).
    """
    x = np.asarray(x, dtype=float)
    grid = np.linspace(x.min() - cut * bw, x.max() + cut * bw, n)
    return grid, stats.norm.pdf(grid[:, None], x[None, :], bw).mean(axis=1)


def coef_table(res, names=None):
    """Coefficient table of a statsmodels fit, like R's summary(fit)$coef."""
    import pandas as pd
    out = pd.DataFrame({"Estimate": res.params, "Std. Error": res.bse,
                        "value": res.tvalues, "Pr(>|value|)": res.pvalues})
    if names is not None:
        out.index = names
    return out


def num_hessian(f, x, h=1e-4):
    """Numerical Hessian of f at x (central differences), like nlm(..., hessian=T)."""
    x = np.asarray(x, dtype=float)
    k = len(x)
    H = np.zeros((k, k))
    for i in range(k):
        for j in range(k):
            ei = np.zeros(k)
            ej = np.zeros(k)
            ei[i] = h
            ej[j] = h
            H[i, j] = (f(x + ei + ej) - f(x + ei - ej)
                       - f(x - ei + ej) + f(x - ei - ej)) / (4 * h * h)
    return H


# ------------------------------------------------------------- li.r

def li(th, like, alpha=0.15):
    """Likelihood interval: the values of th where like crosses alpha.

    Uses linear interpolation on each side of the maximum.
    """
    th = np.asarray(th, dtype=float)
    like = np.asarray(like, dtype=float)
    that = np.mean(th[like == np.max(like)])

    low = th < that
    if low.sum() < 2:
        lowval = th.min()
    else:
        lowval = approx(like[low], th[low], alpha)

    up = th > that
    if up.sum() < 2:
        return np.array([lowval, th.max()])
    upval = approx(like[up], th[up], alpha)
    return np.array([lowval, upval])


# ------------------------------------------------------------- meanlike.r

def meanlike(x):
    """Empirical likelihood for the mean.

    Returns (theta, loglik), with loglik normalised to have maximum 0.
    """
    x = np.asarray(x, dtype=float)
    n = len(x)
    sx = np.sort(x)
    nt = 40
    nlam = 40
    th = np.linspace(1.01 * sx[0], sx[n - 2] + 0.99 * (sx[n - 1] - sx[n - 2]), nt)
    theta = []
    loglik = []
    for t in th:
        a = x - t
        lam = np.linspace(n / a.min(), n / a.max(), nlam)[1:-1]
        glam = (a / (n - np.outer(lam, a))).sum(axis=1)
        lam0 = approx(glam, lam, 0.0)
        if not np.isnan(lam0):  # lam0 exists
            w = 1 / (n - lam0 * a)
            if w.min() > 0 and w.max() < 1:  # proper distribution
                theta.append(t)
                loglik.append(np.sum(np.log(w)))
    loglik = np.array(loglik)
    return np.array(theta), loglik - loglik.max()


# ------------------------------------------------------------- msmooth.r

def msmooth(x, y, npar=21, lam=10, d=2):
    """Smooth estimate of f(x) in y = f(x) + error.

    lam   is the smoothing parameter
    npar  is the number of equispaced points of x
    d     is the degree of differencing
    Returns (x, fhat) at the gridded locations of x.
    """
    xx = np.linspace(np.min(x) - 0.0001, np.max(x), npar)
    cutx = cut(x, xx)
    xx1 = xx[1:] - (xx[1] - xx[0]) / 2

    # bin statistics
    w = table(cutx, npar - 1)
    ybar = bin_means(y, cutx, npar - 1)

    D = np.diff(np.eye(npar - 1), n=d, axis=0)  # d-th order difference matrix
    R1 = D.T @ D

    fhat = np.linalg.solve(np.diag(w) + lam * R1, w * ybar)
    return xx1, fhat


# ------------------------------------------------------------- binormal.r
# Binomial-normal marginal probability, using Gaussian quadrature.

_xi16 = np.array([.0950125098, .2816035507, .4580167776, .6178762444,
                  .7554044083, .8656312023, .9445750230, .9894009439])
_xi16 = np.concatenate([-_xi16[::-1], _xi16])
_wi16 = np.array([.1894506104, .1826034150, .1691565193, .1495959888,
                  .1246289712, .0951585116, .0622535239, .0271524594])
_wi16 = np.concatenate([_wi16[::-1], _wi16])

_xi32 = np.array([.0483076656, .1444719615, .2392873622, .3318686022,
                  .4213512761, .5068999089, .5877157572, .6630442669,
                  .7321821187, .7944837959, .8493676137, .8963211557,
                  .9349060759, .9647622555, .9856115115, .9972638618])
_xi32 = np.concatenate([-_xi32[::-1], _xi32])
_wi32 = np.array([.0965400885, .0956387200, .0938443990, .0911738786,
                  .0876520930, .0833119242, .0781938957, .0723457941,
                  .0658222227, .0586840934, .0509980592, .0428358980,
                  .0342738629, .0253920653, .0162743947, .0070186100])
_wi32 = np.concatenate([_wi32[::-1], _wi32])


def _dbinorm(x, n, p, sig, xi, wi):
    th = np.log(p / (1 - p))
    u = xi * 4 * sig
    pu = np.exp(th + u) / (1 + np.exp(th + u))
    pu = np.clip(pu, np.exp(-25), 1 - np.exp(-25))
    probu = stats.binom.pmf(x, n, pu) * stats.norm.pdf(u, 0, sig)
    return 4 * sig * np.sum(wi * probu)


def dbinorm(x, n, p, sig):
    """Binomial-normal marginal probability, 16-point Gaussian quadrature."""
    return _dbinorm(x, n, p, sig, _xi16, _wi16)


def dbinorm32(x, n, p, sig):
    """Binomial-normal marginal probability, 32-point Gaussian quadrature."""
    return _dbinorm(x, n, p, sig, _xi32, _wi32)


# ------------------------------------------------------------- arsim.dat

def ar_sim(n, phi, sigma=1, rng=None):
    """Simulate n values of an AR(p) process, discarding a burn-in of 50."""
    rng = np.random.default_rng() if rng is None else rng
    phi = np.atleast_1d(phi)
    p = len(phi)
    x = np.zeros(n + 50)
    for i in range(p, n + 50):
        x[i] = np.sum(phi * x[i - 1::-1][:p]) + rng.normal() * sigma
    return x[50:]
