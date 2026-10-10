# In All Likelihood: R examples in Python

Python versions, as Jupyter notebooks, of the R code that accompanies
Yudi Pawitan, *In All Likelihood: Statistical Modelling and Inference Using
Likelihood* (Oxford University Press, 2001). The R scripts and data files are
the author's; the copyright in them remains with him.

## Layout

| Path | Contents |
|---|---|
| `chapterNN.ipynb` | One notebook per chapter. Each section is one R script, headed with the script's name. |
| `helpers.py` | Python versions of the shared R helpers (`li.r`, `meanlike.r`, `msmooth.r`, `binormal.r`, and `ar.sim` from `arsim.dat`), plus small stand-ins for R functions such as `approx`, `scan`, `cut`, `qqnorm` and `density`. |
| `data/` | The data files, unchanged. Some (`exercise*.dat`) are not used by any script. |
| `R/` | The original R scripts, unchanged. |

There is no code for chapters 7 and 14.

## Running

```sh
python -m venv .venv
source .venv/bin/activate
pip install -r requirements.txt
jupyter lab
```

Run the notebooks from the repository root so that `helpers.py` and `data/`
are found. The notebooks are committed with their outputs.

## Notes on the conversion

- `lm()` and `glm()` are replaced by `statsmodels`; `coef_table()` in
  `helpers.py` prints the equivalent of `summary(fit)$coef`.
- `nlm()` is replaced by `scipy.optimize.minimize`.
- Random numbers come from numpy, so simulated results differ from R's
  (the R seeds are reused as numpy seeds where the scripts set one).
- Some R functions have no exact Python equivalent:
  - `supsmu()` (Chapter 15) is replaced by `lowess`.
  - `loess()` (Example 18.1) is replaced by `lowess`, which is locally linear
    rather than quadratic.
  - `survreg()` (Example 11.6) is replaced by maximising the same censored
    exponential and Weibull log-likelihoods directly.
  - `coxph()` and `survfit()` are replaced by statsmodels' `PHReg` and
    `SurvfuncRight`.
- A few scripts needed small fixes to run, each marked with a comment:
  - variables defined in other scripts (`x0` in `EX6-7C.R`, `sumx` in
    `FIG11-6.R`, `npar` in `FIG18-9.R`);
  - a hard-coded path in `EX18-10.R`;
  - a typo (`t  itle`) in `EX4-16.R`;
  - functions with no return value in `EX17-6A.R`.
- Everything else follows the R code as written, including a few lines that
  look like slips in the original:
  - `1 - pnorm(chi2, 11)` in `EX4-3.R`;
  - `sigma` taken as a variance in `EX6-19A.R`;
  - `se <- sqrt(se)` and `x <- 6` in `FIG10-3.R`;
  - `lm(y ~ lx + lx^2)` in `EX6-18B.R`, which in R is a straight-line fit.
- The editor backup files in `R/` (`*.$$$`, `EX11-11.$R`) are older copies
  of the scripts and are not tracked.
