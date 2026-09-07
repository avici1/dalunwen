# Script to check subjectid overlap across three Excel files
# Date: 2026-03-13

# Install required packages if not already installed
if (!require("readxl")) install.packages("readxl")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")

# Load packages
library(readxl)
library(openxlsx)
library(dplyr)

# Define file paths
file_paths <- c(
  "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理/stroke_baseline_simple.xlsx",
  "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理/stroke_lab2.xlsx",
  "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理/stroke_patient.xlsx"
)

# Extract filenames for better readability
file_names <- basename(file_paths)

# Create a list to store subjectid from each file
subjectid_list <- list()

# Read each file and extract subjectid
cat("Reading Excel files...\n")
for (i in 1:length(file_paths)) {
  file_path <- file_paths[i]
  file_name <- file_names[i]
  
  cat(paste("Reading", file_name, "..."))
  
  tryCatch({
    # Read the first sheet
    df <- read_excel(file_path)
    
    # Check if subjectid column exists
    if ("subjectid" %in% colnames(df)) {
      # Extract unique subjectid values
      subjectids <- df$subjectid %>% unique() %>% sort()
      subjectid_list[[file_name]] <- subjectids
      cat(paste(" Found", length(subjectids), "unique subjectid\n"))
    } else if ("subject_id" %in% colnames(df)) {
      # Sometimes it might be subject_id instead
      subjectids <- df$subject_id %>% unique() %>% sort()
      subjectid_list[[file_name]] <- subjectids
      cat(paste(" Found", length(subjectids), "unique subject_id\n"))
    } else {
      # If neither column exists, report it
      subjectid_list[[file_name]] <- character(0)
      cat(" No subjectid or subject_id column found\n")
    }
  }, error = function(e) {
    # If there's an error reading the file
    subjectid_list[[file_name]] <- character(0)
    cat(paste(" Error:", e$message, "\n"))
  })
}

# Check if any file has no subjectid
if (any(sapply(subjectid_list, length) == 0)) {
  cat("\n❌ Error: Some files do not contain subjectid information!\n")
  print(names(subjectid_list)[sapply(subjectid_list, length) == 0])
  stop("Unable to complete overlap analysis due to missing subjectid columns")
}

# Get all unique subjectid across all files
all_subjectid <- unlist(subjectid_list) %>% unique() %>% sort()
cat(paste("\nTotal unique subjectid across all files:", length(all_subjectid), "\n"))

# Check for complete overlap
is_complete_overlap <- length(intersect(subjectid_list[[1]], intersect(subjectid_list[[2]], subjectid_list[[3]]))) == length(all_subjectid)

# Check pairwise overlaps
pairwise_overlap <- list()
pairwise_overlap[[paste(file_names[1], "&", file_names[2])]] <- intersect(subjectid_list[[1]], subjectid_list[[2]])
pairwise_overlap[[paste(file_names[1], "&", file_names[3])]] <- intersect(subjectid_list[[1]], subjectid_list[[3]])
pairwise_overlap[[paste(file_names[2], "&", file_names[3])]] <- intersect(subjectid_list[[2]], subjectid_list[[3]])

# Create a presence/absence matrix
cat("Creating presence/absence matrix...\n")
presence_matrix <- data.frame(subjectid = all_subjectid)

# Add columns for each file
for (file_name in names(subjectid_list)) {
  presence_matrix[[file_name]] <- ifelse(all_subjectid %in% subjectid_list[[file_name]], "✓", "✗")
}

# Add overlap category column
presence_matrix$overlap_category <- apply(presence_matrix[, -1], 1, function(x) {
  count <- sum(x == "✓")
  if (count == 3) return("All three files")
  if (count == 2) return("Two files")
  if (count == 1) return("One file only")
  return("None")
})

# Create summary statistics
summary_stats <- data.frame(
  Metric = c(
    "Total unique subjectid",
    "Subjectid in all three files",
    "Subjectid in exactly two files",
    "Subjectid in exactly one file",
    "Complete overlap across all files"
  ),
  Value = c(
    length(all_subjectid),
    sum(presence_matrix$overlap_category == "All three files"),
    sum(presence_matrix$overlap_category == "Two files"),
    sum(presence_matrix$overlap_category == "One file only"),
    ifelse(is_complete_overlap, "Yes", "No")
  )
)

# Add pairwise overlap statistics
for (pair in names(pairwise_overlap)) {
  summary_stats <- rbind(summary_stats, data.frame(
    Metric = paste("Subjectid in", pair),
    Value = length(pairwise_overlap[[pair]])
  ))
}

# Create Excel summary file
output_file <- "f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理/subjectid_overlap_analysis.xlsx"
cat(paste("Creating Excel summary file:", output_file, "\n"))

# Create workbook
wb <- createWorkbook()

# Add presence matrix sheet
addWorksheet(wb, "Subjectid_Presence")
writeData(wb, "Subjectid_Presence", presence_matrix)

# Add summary statistics sheet
addWorksheet(wb, "Summary_Statistics")
writeData(wb, "Summary_Statistics", summary_stats)

# Auto-adjust column widths for better readability
setColWidths(wb, "Subjectid_Presence", 1:ncol(presence_matrix), 18)
setColWidths(wb, "Summary_Statistics", 1:ncol(summary_stats), 30)

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

# Print results
cat("\n--- Subjectid Overlap Analysis Results ---")
cat("\n\nSummary Statistics:")
print(summary_stats)

cat("\n\n--- Detailed Overlap Categories ---")
overlap_counts <- table(presence_matrix$overlap_category)
print(overlap_counts)

if (is_complete_overlap) {
  cat("\n\n✅ All three files have completely overlapping subjectid!")
} else {
  cat("\n\n❌ The files do NOT have completely overlapping subjectid.")
  cat("\nCheck the detailed report for specific differences.")
}

cat("\n\n✅ Subjectid overlap analysis completed successfully!")
cat(paste("\n\n📋 Detailed results saved to:", output_file))

# Filter and save patients that exist in all three tables
cat("\n\n--- Saving patients present in all three tables ---")



all_three_subjectid <- presence_matrix %>% 
  filter(overlap_category == "All three files") %>% 
  pull(subjectid)

cat(paste("Number of patients present in all three tables:", length(all_three_subjectid), "\n"))

# Read, filter, and save each file
if (length(all_three_subjectid) > 0) {
  final_output <- "f:/文章_大论文/MIMIC数据库_代码/trea代码/0313数据处理/0314_finnal_data.xlsx"
  cat(paste("Saving filtered data to:", final_output, "\n"))
  
  # Create new workbook
  final_wb <- createWorkbook()
  
  for (i in 1:length(file_paths)) {
    file_path <- file_paths[i]
    file_name <- file_names[i]
    sheet_name <- gsub("\\.xlsx$", "", file_name)
    
    cat(paste("Filtering", file_name, "..."))
    
    # Read the file again
    df <- read_excel(file_path)
    
    # Check which column name contains subjectid
    id_col <- ifelse("subjectid" %in% colnames(df), "subjectid", "subject_id")
    
    # Filter the data
    filtered_df <- df %>% filter(!!sym(id_col) %in% all_three_subjectid)
    
    # Add to workbook
    addWorksheet(final_wb, sheet_name)
    writeData(final_wb, sheet_name, filtered_df)
    
    cat(paste(" Saved", nrow(filtered_df), "rows\n"))
  }
  
  # Save the final workbook
  saveWorkbook(final_wb, final_output, overwrite = TRUE)
  cat(paste("\n✅ Successfully saved filtered data to:", final_output))
} else {
  cat("\n⚠️ No patients found that exist in all three tables!")
}
























######################################################

# New code for merging and transforming data
# Added after line 300 as requested

# Step 1: Load all necessary data files
cat("\n\n--- Starting data transformation and merging ---")

# Load template data (for reference)
template_data <- read_excel("f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313_白天/data_all_sup1.xlsx")

# Load lab data
cat("\nReading lab data...")
lab_data <- read_excel("f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理/0314_finnal_data.xlsx", sheet = "stroke_lab2")

# Load baseline data
cat("\nReading baseline data...")
baseline_data <- read_excel("f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理/0313_stroke_baseline.xlsx")

# Load ICU data
cat("\nReading ICU data...")
icu_data <- read_excel("f:/文章_大论文/MIMIC数据库_代码/TREA代码/0313数据处理/0313_stroke_icuMERGE.xlsx")

# Step 2: Pivot lab data to wide format (itemid as columns, value as values)
cat("\nPivoting lab data to wide format...")
lab_data_wide <- lab_data %>% 
  select(subject_id, charttime, itemid, value) %>% 
  # Convert value to numeric
  mutate(value = as.numeric(value)) %>% 
  # Pivot wider
  tidyr::pivot_wider(
    names_from = itemid,
    values_from = value,
    values_fn = mean  # Use mean if multiple values per time point
  )

# Step 3: Merge baseline and ICU data by subjectid
cat("\nMerging baseline and ICU data...")
baseline_icu_data <- baseline_data %>% 
  left_join(icu_data, by = "subject_id")

# Step 4: Merge lab data with baseline+ICU data
cat("\nMerging all data together...")
final_data <- lab_data_wide %>% 
  left_join(baseline_icu_data, by = "subject_id")

# Step 5: Save the final wide format data
final_output <- "f:/文章_大论文/MIMIC数据库_代码/trea代码/0313数据处理/0314_final_widedata.xlsx"
cat(paste("\nSaving final data to:", final_output, "..."))

# Create workbook and save data
wb_final <- createWorkbook()
addWorksheet(wb_final, "wide_data")
writeData(wb_final, "wide_data", final_data)
setColWidths(wb_final, "wide_data", 1:ncol(final_data), 15)
saveWorkbook(wb_final, final_output, overwrite = TRUE)

cat("\n\n✅ Data transformation and merging completed successfully!")
cat(paste("\n\n📋 Final wide format data saved to:", final_output))

