script_dir <- Sys.getenv("CH5_SCRIPT_DIR", unset = "work/chapter5_0831")
steps <- c(
  "01_prepare_data.R",
  "02_static_models.R",
  "02b_static_explain.R",
  "04_jm_unified.R",
  "03_rsflc_survival.R",
  "05_combine_results.R"
)
for (step in steps) {
  message("\n===== RUNNING ", step, " =====")
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    file.path(script_dir, step))
  if (!identical(status, 0L)) stop("Step failed: ", step)
}
message("ALL_STEPS_OK")
