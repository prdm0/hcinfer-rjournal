# Common setup for the hcinfer R Journal reproducibility scripts.
# Attach the packages used by the worked example, tables, and figures.
# Load the public-schools dataset from the installed package so the
# scripts do not depend on the local source tree.

suppressPackageStartupMessages({
  library(hcinfer)
  library(ggplot2)
  library(dplyr)
})

# Keep console output compact and deterministic across rendering runs.
options(width = 80, digits = 4, scipen = 2)

# The data live in the package. Load them here so later scripts share one
# copy and one column-construction path.
data("PublicSchools2", package = "hcinfer")

# Scaled income in units of ten thousand dollars, matching the manuscript
# model definition (income is per capita nominal income for 2024).
public_schools <- PublicSchools2
public_schools$scaled_income <- public_schools$income / 10000

# The full-sample intercept-free model used throughout the worked example.
# Both interaction columns are retained so the design matrix order matches
# the coefficient order reported by the package (scaled_income first).
fit <- lm(expenditure ~ scaled_income + scaled_income:south - 1,
          data = public_schools)

# Number of coefficients and observations for leverage thresholds.
p <- length(coef(fit))
n <- nobs(fit)

invisible(NULL)
