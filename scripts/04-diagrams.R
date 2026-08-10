# Compile the two TikZ schematic diagrams to PDF and PNG.
#
# Inputs:
#   scripts/tikz/workflow.tex     User-facing workflow diagram.
#   scripts/tikz/architecture.tex Repository-level architecture diagram.
# Outputs (in figures/):
#   workflow.pdf, workflow.png
#   architecture.pdf, architecture.png
#
# The PDF is the vector original for the article. The PNG is the raster
# companion used by the self-contained HTML output, rendered at 300 DPI.
#
# Run from the article directory:
#   Rscript scripts/04-diagrams.R

diagrams <- c("workflow", "architecture", "sandwich-efficiency")
tikz_dir <- "scripts/tikz"
out_dir <- "figures"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

compile_one <- function(name) {
  tex <- file.path(tikz_dir, paste0(name, ".tex"))
  pdf_out <- file.path(out_dir, paste0(name, ".pdf"))
  png_out <- file.path(out_dir, paste0(name, ".png"))

  # Compile the standalone LaTeX source. pdf_file directs the output into
  # figures/ and clean removes auxiliary files left by the engine.
  tinytex::latexmk(tex, engine = "pdflatex",
                   pdf_file = pdf_out, clean = TRUE)

  if (!file.exists(pdf_out)) {
    stop(sprintf("Compilation of %s did not produce %s.", name, pdf_out))
  }

  # Rasterize every page at 300 DPI for the HTML output.
  pdftools::pdf_convert(pdf_out, dpi = 300, format = "png",
                        filenames = png_out, verbose = FALSE)
  invisible(c(pdf_out, png_out))
}

for (name in diagrams) {
  compile_one(name)
}

cat("Diagrams written to figures/:\n")
cat("  workflow.pdf, workflow.png\n")
cat("  architecture.pdf, architecture.png\n")
cat("  sandwich-efficiency.pdf, sandwich-efficiency.png\n")

invisible(NULL)
