#!/usr/bin/env Rscript

# Build a portable SHA-256 inventory of the current shareable project. The
# installed R package tree is represented separately by package/version because
# hashing every compiled package file adds noise without improving analysis
# provenance.
.libPaths(c(file.path(getwd(), "R-library"), .libPaths()))
if (!requireNamespace("digest", quietly = TRUE)) stop("Package 'digest' is required.")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(root, "project_provenance")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, "current_project_files_sha256.csv")

content_roots <- file.path(root, c("analysis", "data", "figures"))
files <- unlist(lapply(content_roots, list.files, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE),
                use.names = FALSE)
root_files <- list.files(root, recursive = FALSE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
root_files <- root_files[file.info(root_files)$isdir %in% FALSE]
files <- unique(c(files[file.info(files)$isdir %in% FALSE], root_files))
files <- files[basename(files) != "current_project_files_sha256.csv"]
relative <- substring(normalizePath(files, winslash = "/", mustWork = TRUE), nchar(root) + 2L)
info <- file.info(files)

manifest <- data.frame(
  relative_path = relative,
  bytes = as.numeric(info$size),
  modified_utc = format(info$mtime, tz = "UTC", usetz = TRUE),
  sha256 = unname(vapply(files, digest::digest, character(1), file = TRUE, algo = "sha256")),
  stringsAsFactors = FALSE
)
manifest <- manifest[order(manifest$relative_path), ]
utils::write.csv(manifest, out_file, row.names = FALSE)
cat("Wrote", nrow(manifest), "current-file hashes to", out_file, "\n")
