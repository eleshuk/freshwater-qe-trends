# freshwater-qe-trends

This repository contains all code pertaining to the 2026 thesis titled "Assessing Spatio-temporal Trends for the Quality Element Status of Freshwater-related Environments", by Elizabeth Leshuk.

This repository contains code to 1) scrape WISE WFD data and metadata from [Discodata](https://discodata.eea.europa.eu/) (European Environment Agency), 2) filter, clean, and process the Surface Water Body related tables (Figure 1), 3) model individual biological quality element (BQE) data, 4) plot

This study builds a harmonized, water body-level dataset of Water Framework Directive (WFD) surface water biological quality element (BQE) status for the 2010, 2016, and 2022 reporting cycles. The dataset integrates spatial, climate, land use, nutrient, and reported pressures data, and develops a semi-automated pipeline that can rerun as new reporting cycles are released. Then, looking only at rivers and using mixed-effects ordinal models, it asks:

1.  What are the spatial and temporal patterns in river BQE status across Europe

2.  Which drivers and pressures are associated with BQE status, and how do they differ between BQEs

3.  How much variation in status is attributable to location and national assessment, compared with drivers and pressures?

![](images/20260826_data_acquisition.png)

Fig. 1. Workflow used to acquire surface water body (SWB) data from the Water Information System for Europe - Water Framework Directive (WISE_WFD) database on the European Environment Agency (EEA) Discodata platform.

## How to run the pipeline

Open `freshwater-qe-trends.Rproj` so the working directory is the project root, then run the eight runner scripts in order. Every path is relative to the project root and all data inputs and outputs live in `data/`.

| Runner                      | Steps                                                                                                                                                                                                                                        | Main outputs                                                                                                                      |
|-----------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| `run_00_scrape_clean.R`     | 0001 scrape metadata, 0002 scrape WFD data to parquet, 0003 strip shared columns, 0004 clean, 0005 join BQEs to reference, 0006 filter reference                                                                                             | `data/WFD/parquet_tables_cleaned/`, `data/WFD/parquet_tables_joined/joined_bqe_ref_w2010_final.parquet`, `swb_reference_filtered` |
| `run_01_spatial.R`          | 0101 remove coastal waters from `Input_data.gpkg`, then 0102 `python python/0102_assign_catchments.py` (run manually)                                                                                                                        | `data/spatial/ccm2_wfd_join/Input_data_filtered.gpkg`, `data/spatial/Workflow_outputs.gpkg`                                       |
| `run_02_extract_drivers.R`  | 0201 climate preprocessing (`climate_method` = "cdo" or "windows"), 0202-0207 climate, nitrogen, phosphorus, land cover, imperviousness and AMBER per catchment                                                                              | `data/drivers/<driver>/outputs/`                                                                                                  |
| `run_03_joins.R`            | 0301 BQE spatial join per cycle, 0302 drop geometry, 0303 driver matrix, 0304 biogeographical regions, 0305 river basins                                                                                                                     | `data/model/model_ready.parquet`, `model_ready_biogeo.parquet`, `model_ready_biogeo_rb.parquet`                                   |
| `run_04_model_prep.R`       | 0401 split lakes and rivers, 0402 transform river covariates, 0403 covariate checks (Fig. S3-S5)                                                                                                                                             | `data/model/rivers_model_df_transformed.parquet`, `plots/covariate_checks/`                                                       |
| `run_05_model.R`            | 0501 fit the five river BQE CLMMs (about 45 min on a MacBook Air; existing fits are skipped)                                                                                                                                                 | `data/model/M4/m4_stage1_results_rivers/RW/`                                                                                      |
| `run_06_model_assessment.R` | 0601 AIC, kappa and observed vs predicted (Tables S5-S6, Fig. S7), 0602 parallel slopes (Table S7), 0603 random-effect QQ (Fig. S6)                                                                                                          | `plots/model_assessment/`, tables in the results folder                                                                           |
| `run_07_figures.R`          | 0701 coefficients (Fig. 10, Table S8), 0702 status by cycle and region (Fig. 7, 8, 9, 12), 0703 variance partitioning (Fig. 11, Table S9), 0704 data for the exploration maps, then `python python/0704_plot_bqe_maps.py` (Fig. 4, 5, 6, S8) | `plots/model_results/`, `plots/maps/`, `data/model/M4/m4_stage1_results_rivers/RW/tables/`                                        |

After the pipelines, `tests/check_pipeline_outputs.R` checks the saved outputs. Table 7 (water bodies after each filtering step) and the Fig. S1/S2 column-presence tables are written to `data/summary_tables/` by pipelines 03 and 00.

### Inputs that are not created by the pipelines

-   `data/WFD/WFD_URLs.xlsx`: Discodata download links (only needed when `scrape <- TRUE`)
-   `data/WFD/parquet_tables_spatial/surface_water_bodies/{year}_spatial_data_swb.parquet` and `rivers/{year}_spatial_data_rivers.parquet`: per-cycle WFD geometries, made from the gpkgs in `data/WFD/spatial_data/`
-   `data/spatial/ccm2_wfd_join/Input_data.gpkg`: WFD water bodies with the CCM2 catchments and network
-   `data/spatial/CCM2_stuff.gdb`: CCM2 catchments and basins
-   `data/biogeographical_regions/BiogeoRegions2016.shp` and `WB_drainage_areas.gpkg`
-   Raw driver data in `data/drivers/`: `climate/1995-2010_climate` and `climate/2011-2022_climate` (E-OBS), `nitrogen/`, `phosphorus/`, `land_cover/`, `imperviousness_density/`, `dams/atlas_AMBER.csv`

## Project structure

### R/

Function files, numbered by pipeline and run order (00xx scrape and clean, 01xx spatial, 03xx joins, 04xx model prep, 05xx model fit, 06xx model assessment, 07xx figures). Each file only defines functions; the `run_*.R` scripts in the project root call them. `0600_model_helpers.R` and `theme_thesis.R` are shared by pipelines 06 and 07.

-   `0003_strip_shared_cols.R` also contains optional QA functions (`check_swb_integrity()`, `check_swb_consistency()`, `check_join_keys()`, `check_assessment()`, `check_grain()`, `check_grain_all()`) that are not run by the pipeline

### python/

-   `0704_plot_bqe_maps.py` draws the exploration maps (Fig. 4, 5, 6, S8). By default it maps the less filtered dataset built in step 0704 (UK, coastal waters, angiosperms and macroalgae kept); `--joined-file` and `--spatial-suffix` choose another version
-   `0102_assign_catchments.py` joins CCM2 (De Jager & Vogt, 2007) to WFD water bodies
    -   Polygons are kept if more than 10% of a water body's total area falls within a catchment, or if the overlap is more than 10,000 m²
    -   Lines are kept if more than 10% of the segment length falls within a catchment (`intersection length / Segment_length > 0.1`)
    -   Input: `data/spatial/ccm2_wfd_join/Input_data_filtered.gpkg` (or `WFD_INPUT_PATH`)
    -   Output: `data/spatial/Workflow_outputs.gpkg`
    -   Interactive by default; for an unattended run set `WFD_EXECUTION_MODE=unattended` and `WFD_RUN_PROFILE=full`

### extract_drivers/

-   Functions to join external driver data with CCM2 (De Jager & Vogt, 2007) catchments, numbered 02xx
-   These functions are best run on a computer with \> 50GB RAM
    -   For this thesis, the code was run on a desktop computer with an Intel Core i9-10900X processor (10 cores, 20 threads, 3.70 GHz base frequency) and 64 GB of RAM, running Windows 10 Enterprise LTSC 2019 (version 1809, 64-bit) running R 4.3.2.
    -   The large memory requirements were due to the 1.4 million catchments in Europe
-   Drivers: climate, nitrogen surplus, phosphorus surplus, land cover, imperviousness, AMBER barriers
-   `0202b_extract_climate_batched.R` gives the same climate outputs as `0202_extract_climate.R` but processes catchments in batches, so it runs on a 16 GB machine; an interrupted run resumes from the last finished batch

### plots/

-   Figures written by pipelines 04, 06 and 07

### notebooks/

-   Contains the original notebook used to join WFD water bodies with CCM2 catchments

## Original data sources

-   Climate - E-OBS (Cornes et al., 2018)
-   Land cover - CORINE Land Cover (CLMS, 2020)
-   Imperviousness - HRL Imperviousness Density (CLMS, 2020)
-   Nitrogen surplus - Batool et al. (2022)
-   Phosphorus surplus - Batool et al. (2025)
-   AMBER (Belletti et al., 2020)
-   WISE-WFD (European Environmental Agency - Discodata database)
-   [WFD spatial data](https://www.eea.europa.eu/en/datahub/datahubitem-view/a50d10f6-e0f6-496b-a6e2-27eb5ad204e8)
