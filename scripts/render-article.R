# Render the hcinfer R Journal article to PDF and HTML.
#
# The article is an R Markdown document that knits to both
# rjtools::rjournal_pdf_article and rjtools::rjournal_web_article. Rendering
# output_format = "all" produces hcinfer.pdf and hcinfer.html next to the
# source Rmd.
#
# Known failure mode and automatic correction:
#   rjtools issue #117. The bundled RJwrapper.tex defines CSLReferences for
#   pandoc versions older than 3.1.7. Pandoc >= 3.1.7 removed that macro, so
#   the PDF compile fails with "Undefined control sequence \cslentryspacing"
#   or "Lonely \item". The documented fix is to pin pandoc to a version whose
#   CSLReferences definition the wrapper still supports, then retry. This
#   script applies that fix automatically when the primary render fails.
#
# Fallback sequence:
#   1. Primary render with the active (system) pandoc.
#   2. On failure, install and activate pandoc 3.1.6, retry.
#   3. If 3.1.6 still fails, install and activate pandoc 3.1.11.1, retry.
#   4. Restore the system pandoc for rmarkdown before exiting.
#
# Failures are reported, not swallowed. Run from anywhere:
#   Rscript scripts/render-article.R
# or from the article directory:
#   Rscript hcinfer-rjournal/scripts/render-article.R

# Locate the article directory from this script's path so the render runs
# with the Rmd as the working directory and figures/, hcinfer.bib, and
# RJournal.sty resolve correctly.
script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)][1])
  if (is.na(file_arg) || file_arg == "") {
    # Sourced or run interactively: fall back to the current directory.
    getwd()
  } else {
    dirname(normalizePath(file_arg))
  }
}, error = function(e) getwd())

article_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/")
rmd <- "hcinfer.Rmd"

if (!file.exists(file.path(article_dir, rmd))) {
  stop(sprintf(
    "Article not found at %s. Run this script from scripts/ inside the article directory.",
    file.path(article_dir, rmd)
  ))
}

setwd(article_dir)

render_all <- function() {
  rmarkdown::render(rmd, output_format = "all", quiet = FALSE)
}

# Try the primary render with whatever pandoc is active (system or IDE).
ok <- tryCatch({
  render_all()
  TRUE
}, error = function(e) {
  message("Primary render failed: ", conditionMessage(e))
  FALSE
})

# Helper that installs (if needed) and activates a pandoc version, rerenders,
# and reports whether both outputs exist. Returns the previous active version
# so the caller can restore it later.
try_pandoc_version <- function(version) {
  if (!requireNamespace("pandoc", quietly = TRUE)) {
    stop("The pandoc R package is required for the automatic fallback but is not installed.")
  }
  previous <- pandoc::pandoc_version()
  installed <- pandoc::pandoc_installed_versions()
  if (!version %in% installed) {
    message("Installing pandoc ", version, " ...")
    pandoc::pandoc_install(version)
  }
  message("Activating pandoc ", version, " and retrying the render ...")
  pandoc::pandoc_activate(version, rmarkdown = TRUE)
  render_all()
  previous
}

if (!ok) {
  fallback_ok <- tryCatch({
    try_pandoc_version("3.1.6")
    TRUE
  }, error = function(e) {
    message("Pandoc 3.1.6 retry failed: ", conditionMessage(e))
    FALSE
  })

  if (!fallback_ok) {
    fallback_ok <- tryCatch({
      try_pandoc_version("3.1.11.1")
      TRUE
    }, error = function(e) {
      message("Pandoc 3.1.11.1 retry failed: ", conditionMessage(e))
      FALSE
    })
  }

  if (!fallback_ok) {
    stop("Rendering failed with the system pandoc, pandoc 3.1.6, and pandoc 3.1.11.1.")
  }

  # Restore the system pandoc for rmarkdown so later renders are unaffected.
  tryCatch(
    pandoc::pandoc_activate("system", rmarkdown = TRUE),
    error = function(e) {
      message("Could not restore the system pandoc: ", conditionMessage(e))
    }
  )
}

pdf_path <- file.path(article_dir, "hcinfer.pdf")
html_path <- file.path(article_dir, "hcinfer.html")
if (!file.exists(pdf_path) || file.info(pdf_path)$size == 0) {
  stop("Render completed but hcinfer.pdf is missing or empty.")
}
if (!file.exists(html_path) || file.info(html_path)$size == 0) {
  stop("Render completed but hcinfer.html is missing or empty.")
}

message("Render complete: ", pdf_path, " and ", html_path)

invisible(NULL)
