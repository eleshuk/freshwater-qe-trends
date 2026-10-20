# Pipeline 00: scrape, strip, clean and filter the WISE WFD surface water body data
# Run from the project root (open freshwater-qe-trends.Rproj)
# Outputs: data/WFD/metadata, parquet_tables, parquet_tables_stripped, parquet_tables_cleaned, parquet_tables_joined

# Load the functions for this pipeline
source("R/0001_scrape_descriptions_md.R")
source("R/0002_scrape_wfd_data.R")
source("R/0003_strip_shared_cols.R")
source("R/0004_wfd_cleaning.R")
source("R/0005_join_bqe.R")
source("R/0006_filter_reference.R")
source("R/0007_column_presence.R")

# Paths
wfd_root <- "data/WFD"
parquet_root <- file.path(wfd_root, "parquet_tables")
parquet_root_stripped <- file.path(wfd_root, "parquet_tables_stripped")
parquet_root_cleaned <- file.path(wfd_root, "parquet_tables_cleaned")

# Set to TRUE to download the zips from Discodata (slow, only needed once)
scrape <- FALSE

# 0001 Scrape WFD table and column metadata (theme_filter = "SWB" by default)
scrape_wise_wfd_md(wfd_root)

# 0002 Download zips, remove bad files and convert raw WFD zips to parquet
if (scrape) {
  download_results <- download_wfd_zips(
    links_file = file.path(wfd_root, "WFD_URLs.xlsx"),
    dest_dir = wfd_root
  )
  remove_bad_zips(wfd_root)
}
convert_wfd_to_parquet(zip_dir = wfd_root, parquet_root = parquet_root)

# 0003 Strip shared reference and assessment columns into swb_reference and swb_assessment_reference
strip_shared_cols(parquet_root = parquet_root, parquet_root_stripped = parquet_root_stripped)

# 0004 Clean the stripped tables
clean_wfd_tables(parquet_root_stripped = parquet_root_stripped, parquet_root_cleaned = parquet_root_cleaned)

# 0005 Join BQEs to the reference table, writing parquet_tables_joined/joined_bqe_ref_w2010_final.parquet
bqe_data <- create_qe_ref_join_filtered(
  wfd_root = wfd_root,
  qe_interest = "QE1 - Biological quality elements",
  export_csv = FALSE,
  export_parquet = TRUE
)
# For all quality elements instead, set qe_interest to c(
#   "QE1 - Biological quality elements",
#   "QE3 - Chemical and physico-chemical quality elements",
#   "QE2 - Hydromorphological quality elements"
# )

# 0006 Filter the reference table to water bodies with BQE data
filter_ref(wfd_root)

# 0007 Column presence across the raw SWB tables (Fig. S1) and the stripped and cleaned tables (Fig. S2)
column_presence(parquet_root, list_swb_tables(parquet_root), "data/summary_tables/figS1_column_presence_raw.csv")
column_presence(
  parquet_root_cleaned,
  setdiff(list_swb_tables(parquet_root_cleaned), c("swb_reference_filtered", "swb_surface_water_body_failing_rbsp_sw_failing_rbsp_other")),
  "data/summary_tables/figS2_column_presence_cleaned.csv"
)
