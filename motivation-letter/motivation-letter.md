---
output: pdf_document
fontsize: 12pt
---

\thispagestyle{empty}
\today

Editor  
The R Journal  
\bigskip

Dear Editor,
\bigskip

We are pleased to submit the manuscript entitled "hcinfer: An R Package for Heteroskedasticity-Consistent Inference in Linear Regression" for consideration as a software article in The R Journal.

Linear regression fitted by ordinary least squares remains a standard tool in applied statistics, but its usual standard errors assume constant error variance. When error variances differ across observations, hypothesis tests and confidence intervals based on the conventional covariance matrix can be misleading, even though the coefficient estimates remain unbiased. Heteroskedasticity-consistent covariance estimators correct this problem without a parametric model for the error variances, yet different estimators can yield substantially different inferences, especially when high-leverage observations are present. This is the setting the manuscript addresses.

The manuscript introduces the \texttt{hcinfer} package, available on CRAN, which provides a focused workflow for heteroskedasticity-consistent inference in linear models fitted with \texttt{lm}. The package integrates covariance estimation, coefficient-level normal Wald tests, confidence intervals, leverage diagnostics, adjustment-factor inspection, and graphical reporting behind a small and consistent interface, and it reuses standard R generics so that its objects behave like familiar model outputs. The default covariance estimator is HC$\beta$, a leverage-sensitive heteroskedasticity-consistent estimator that adjusts residual weights through a beta distribution fitted to the leverage values. We emphasize that HC$\beta$ was proposed by the authors of this manuscript; the underlying methodology is developed in the companion paper "A Beta-Based Heteroskedasticity-Consistent Covariance Matrix Estimator" (arXiv:2607.10905). The package implements the HC0 through HC5m estimators together with HC$\beta$, and it is fully reproducible: all examples, tables, and figures in the manuscript are generated from evaluated R code.

The manuscript fits The R Journal through its combination of a CRAN-published package, a concise and reproducible software workflow for heteroskedasticity-robust inference, an integrated leverage-aware default estimator contributed by the authors, and a worked example on public school expenditure data in which the choice of covariance estimator changes the substantive conclusion. The package is released under the MIT license, and the source, documentation website, and test suite are available at \url{https://github.com/prdm0/hcinfer}.

Thank you for considering our manuscript. We believe it will be of interest to readers working on regression inference, robust covariance estimation, and statistical computing in R.

\bigskip
\bigskip

Sincerely,

Francisco Cribari-Neto  
Departamento de Estatística  
Universidade Federal de Pernambuco  
Recife/PE, Brazil  
francisco.cribari@ufpe.br
