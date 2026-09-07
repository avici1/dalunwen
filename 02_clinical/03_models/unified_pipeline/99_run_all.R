## Chapter 5 fully reproducible pipeline
## Run from any directory with:
## Rscript 99_run_all.R

this_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/"), error = function(e) NA_character_)
script_dir <- if (!is.na(this_file)) dirname(this_file) else normalizePath(getwd(), winslash = "/")
project_root <- Sys.getenv("CH5_PROJECT_ROOT", unset = normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE))
artifact_dir <- Sys.getenv("CH5_ARTIFACT_DIR", unset = file.path(project_root, "\u56fe\u50cf\u8868\u683c"))
Sys.setenv(CH5_SCRIPT_DIR = script_dir, CH5_PROJECT_ROOT = project_root, CH5_ARTIFACT_DIR = artifact_dir)

steps <- c(
  "01_stage_inputs.R",
  "02_prepare_data.R",
  "03_descriptive_tables_and_flow.R",
  "04_static_models.R",
  "05_static_explain.R",
  "06_rsflc_model.R",
  "08_joint_model.R",
  "09_combine_results.R",
  "10_build_docx.R"
)

for (step in steps) {
  message(sprintf("\n===== RUNNING %s =====", step))
  source(file.path(script_dir, step), encoding = "UTF-8", local = new.env(parent = globalenv()))
}

message("Chapter 5 pipeline completed.")
