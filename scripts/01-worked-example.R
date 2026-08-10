# Reproduce the worked example from the manuscript.
# Fits the intercept-free education-expenditure model and prints the
# HCbeta inference, confidence intervals, and summary.
#
# Expected anchors (manuscript):
#   beta2 = 4100.65, beta3 = -322.66
#   HCbeta SE(beta3) = 254.58, z = -1.27, p = 0.205
#   95% CI for beta3 = [-821.62, 176.31]
#   HCbeta summary shape parameters: a_tilde = 27.57, b_tilde = 1.60
#
# Run from the article directory:
#   Rscript scripts/01-worked-example.R

source("scripts/00-setup.R")

result <- hcinfer(fit)

# OLS coefficient estimates.
coef_table <- tests(result)
beta2 <- coef_table$estimate[coef_table$term == "scaled_income"]
beta3 <- coef_table$estimate[coef_table$term == "scaled_income:south"]
cat(sprintf("beta2 = %.2f  (expected 4100.65)\n", beta2))
cat(sprintf("beta3 = %.2f  (expected -322.66)\n", beta3))

# HCbeta standard error and Wald test for the interaction coefficient.
hcbeta_row <- coef_table[coef_table$term == "scaled_income:south", ]
cat(sprintf("HCbeta SE(beta3) = %.2f  (expected 254.58)\n",
            hcbeta_row$std_error))
cat(sprintf("z = %.2f  (expected -1.27)\n", hcbeta_row$z_value))
cat(sprintf("p-value = %.3f  (expected 0.205)\n", hcbeta_row$p_value))

# 95% HCbeta confidence interval for the interaction coefficient.
ci <- confint(result, parm = "scaled_income:south")
cat(sprintf("95%% CI for beta3 = [%.2f, %.2f]  (expected [-821.62, 176.31])\n",
            ci$conf_low, ci$conf_high))

# HCbeta shape parameters reported by the summary method.
params <- vcov_hc(fit, type = "hcbeta")$method_params
cat(sprintf("a_tilde = %.2f  (expected 27.57)\n", params$a_tilde))
cat(sprintf("b_tilde = %.2f  (expected 1.60)\n", params$b_tilde))

# Full console output for inspection.
cat("\n--- confint(result) ---\n")
print(confint(result))
cat("\n--- tests(result) ---\n")
print(tests(result))
cat("\n--- summary(result) ---\n")
print(summary(result))

invisible(NULL)
