# Regenerate the four data figures for the hcinfer R Journal article.
# Each figure is written to figures/ as both vector PDF and raster PNG.
#
# Figures:
#   fig:scatter        Expenditure versus scaled income with the two
#                     fitted intercept-free lines (south 0 and 1).
#   fig:intervals     HCbeta confidence-interval forest plot from
#                     plot.hcinfer.
#   fig:weights       Adjustment factor g_t versus leverage h_t for
#                     HC3, HC4, HC4m, and HCbeta, full sample.
#                     High-leverage points (h_t > 3p/n) are highlighted.
#   fig:weights-nodc  Same diagnostic on the subsample without the
#                     District of Columbia.
#
# Run from the article directory:
#   Rscript scripts/03-figures.R

source("scripts/00-setup.R")

if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

# Save a ggplot object to both PDF and PNG at fixed dimensions so the
# figures remain reproducible across renderings.
save_both <- function(plot, base, width = 7, height = 4.5,
                      dpi = 300, scale = 1) {
  pdf_path <- file.path("figures", paste0(base, ".pdf"))
  png_path <- file.path("figures", paste0(base, ".png"))
  ggplot2::ggsave(pdf_path, plot, width = width, height = height,
                  scale = scale, device = grDevices::pdf)
  ggplot2::ggsave(png_path, plot, width = width, height = height,
                  scale = scale, dpi = dpi, device = grDevices::png)
  invisible(c(pdf_path, png_path))
}

# ---------------------------------------------------------------------------
# fig:scatter
# Expenditure versus scaled income with the two fitted lines. The
# intercept-free model implies one line per region: slope beta2 for
# south = 0 and slope beta2 + beta3 for south = 1, both through the origin.
# ---------------------------------------------------------------------------

coefs <- coef(fit)
slope_north <- coefs["scaled_income"]
slope_south <- slope_north + coefs["scaled_income:south"]

scatter_plot <- ggplot2::ggplot(
  public_schools,
  ggplot2::aes(x = scaled_income, y = expenditure, color = factor(south))
) +
  ggplot2::geom_point(size = 2, alpha = 0.85) +
  ggplot2::geom_abline(
    ggplot2::aes(slope = slope_north, intercept = 0),
    color = "#2c5f8a", linewidth = 0.8, linetype = "solid"
  ) +
  ggplot2::geom_abline(
    ggplot2::aes(slope = slope_south, intercept = 0),
    color = "#c0392b", linewidth = 0.8, linetype = "solid"
  ) +
  ggplot2::scale_color_manual(
    name = "Region",
    values = c(`0` = "#2c5f8a", `1` = "#c0392b"),
    labels = c("Non-South", "South")
  ) +
  ggplot2::labs(
    x = "Per capita income (units of $10,000)",
    y = "Annual per-pupil expenditure (USD)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

save_both(scatter_plot, "scatter", width = 7, height = 4.5)

# ---------------------------------------------------------------------------
# fig:intervals
# The package forest plot for the HCbeta inference object.
# ---------------------------------------------------------------------------

result <- hcinfer(fit)
intervals_plot <- plot(result)
save_both(intervals_plot, "hcbeta-intervals", width = 7, height = 3.2)

# ---------------------------------------------------------------------------
# fig:weights and fig:weights-nodc
# Adjustment factor g_t versus leverage h_t for HC3, HC4, HC4m, and HCbeta,
# faceted by method with free vertical scales. Points with leverage above
# the 3p/n threshold are highlighted in red, matching plot.hcinfer_vcov.
# ---------------------------------------------------------------------------

weight_methods <- c("hc3", "hc4", "hc4m", "hcbeta")
method_labels <- c(HC3 = "HC3", HC4 = "HC4", HC4m = "HC4m", HCbeta = "HCbeta")

build_weights <- function(model, data) {
  p_local <- length(coef(model))
  n_local <- nobs(model)
  threshold <- 3 * p_local / n_local

  rows <- purrr::map(weight_methods, function(method) {
    cov <- vcov_hc(model, type = method)
    tibble::tibble(
      method = method_labels[[cov$label]],
      h_t = unname(cov$leverage),
      g_t = unname(cov$weights),
      high_leverage = unname(cov$leverage) > threshold
    )
  })
  long <- dplyr::bind_rows(rows)
  ggplot2::ggplot(long, ggplot2::aes(x = h_t, y = g_t)) +
    ggplot2::geom_vline(
      xintercept = threshold,
      linewidth = 0.35, linetype = "dashed", color = "#c0392b"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(color = high_leverage),
      size = 1.8, alpha = 0.85
    ) +
    ggplot2::facet_wrap(~method, scales = "free_y", ncol = 2) +
    ggplot2::scale_color_manual(
      values = c(`FALSE` = "#2c5f8a", `TRUE` = "#c0392b"),
      guide = "none"
    ) +
    ggplot2::labs(
      x = expression(h[t]),
      y = expression(g[t])
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

weights_plot <- build_weights(fit, public_schools)
save_both(weights_plot, "weights-leverage", width = 8, height = 6)

# No-District-of-Columbia variant. The reduced sample uses 50 jurisdictions.
public_schools_no_dc <- public_schools[
  public_schools$state != "District of Columbia",
]
fit_no_dc <- lm(expenditure ~ scaled_income + scaled_income:south - 1,
                data = public_schools_no_dc)
weights_plot_nodc <- build_weights(fit_no_dc, public_schools_no_dc)
save_both(weights_plot_nodc, "weights-leverage-noDC", width = 8, height = 6)

cat("Figures written to figures/:\n")
cat("  scatter.pdf, scatter.png\n")
cat("  hcbeta-intervals.pdf, hcbeta-intervals.png\n")
cat("  weights-leverage.pdf, weights-leverage.png\n")
cat("  weights-leverage-noDC.pdf, weights-leverage-noDC.png\n")

invisible(NULL)
