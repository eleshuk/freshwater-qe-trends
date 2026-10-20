# Merge external drivers with WFD data and build the model-ready dataframe
# Uses cached data when prompted to avoid recalculating for each metric
# Recomputes a cache when an input file is newer than it


# Tracks elapsed time per step and prints a summary at the end
make_timer <- function() {
  log <- list()
  current_step <- NULL
  current_start <- NULL
  
  start <- function(step_name) {
    current_step <<- step_name
    current_start <<- proc.time()
    message(sprintf("[%s] Starting: %s",
                    format(Sys.time(), "%H:%M:%S"), step_name))
  }
  
  done <- function(detail = NULL) {
    elapsed <- (proc.time() - current_start)[["elapsed"]]
    msg <- sprintf("[%s] Done: %s (%.1fs)",
                   format(Sys.time(), "%H:%M:%S"), current_step, elapsed)
    if (!is.null(detail)) msg <- paste0(msg, " — ", detail)
    message(msg)
    log[[length(log) + 1L]] <<- list(step = current_step, seconds = elapsed)
  }
  
  summary <- function() {
    if (length(log) == 0L) return(invisible(NULL))
    df <- do.call(rbind, lapply(log, as.data.frame, stringsAsFactors = FALSE))
    df$pct <- round(100 * df$seconds / sum(df$seconds), 1)
    total <- sum(df$seconds)
    message("\n=== Pipeline timing summary ===")
    for (i in seq_len(nrow(df))) {
      bar <- paste(rep("#", max(1, round(df$pct[i] / 2))), collapse = "")
      message(sprintf("  %-40s %6.1fs  %5.1f%%  %s",
                      df$step[i], df$seconds[i], df$pct[i], bar))
    }
    message(sprintf("  %-40s %6.1fs", "TOTAL", total))
  }
  
  list(start = start, done = done, summary = summary)
}
# Checks for cached parquet; if found, reads it. Otherwise runs compute_fn,
# saves the result, and returns it.
cached_prepare <- function(cache_path, compute_fn, timer, step_name,
                           use_cache = TRUE, input_path = NULL) {
  # Treat the cache as stale when any input file was modified after it was written
  is_stale <- !is.null(input_path) && file.exists(cache_path) &&
    any(file.mtime(input_path) > file.mtime(cache_path), na.rm = TRUE)
  if (is_stale) message(step_name, ": input is newer than cache, recomputing")
  
  if (use_cache && file.exists(cache_path) && !is_stale) {
    timer$start(paste0(step_name, " [cached]"))
    result <- arrow::read_parquet(cache_path)
    timer$done(sprintf("%d rows from cache", nrow(result)))
    return(result)
  }
  
  # Compute fresh
  result <- compute_fn()
  
  # Save cache
  timer$start(paste0("Write cache: ", basename(cache_path)))
  arrow::write_parquet(result, cache_path)
  timer$done()
  
  result
}
# Build pressure matrix
build_pressure_matrix <- function(wfd_root, timer) {
  
  kept_substances <- c(
    "CAS_7439-97-6 - Mercury and its compounds",
    "CAS_7440-43-9 - Cadmium and its compounds",
    "CAS_117-81-7 - Di(2-ethylhexyl)phthalate (DEHP)",
    "CAS_7439-92-1 - Lead and its compounds",
    "CAS_36643-28-4 - Tributyltin-cation",
    "CAS_7440-02-0 - Nickel and its compounds",
    "CAS_330-54-1 - Diuron",
    "CAS_50-32-8 - Benzo(a)pyrene",
    "CAS_34123-59-6 - Isoproturon"
  )
  
  substance_names <- c(
    "CAS_7439-97-6 - Mercury and its compounds" = "mercury",
    "CAS_7440-43-9 - Cadmium and its compounds" = "cadmium",
    "CAS_117-81-7 - Di(2-ethylhexyl)phthalate (DEHP)" = "dehp",
    "CAS_7439-92-1 - Lead and its compounds" = "lead",
    "CAS_36643-28-4 - Tributyltin-cation" = "tributyltin",
    "CAS_7440-02-0 - Nickel and its compounds" = "nickel",
    "CAS_330-54-1 - Diuron" = "diuron",
    "CAS_50-32-8 - Benzo(a)pyrene" = "benzo_a_pyrene",
    "CAS_34123-59-6 - Isoproturon" = "isoproturon"
  )
  
  substance_cols <- paste0("failing_", substance_names)
  
  kept_pressure_groups <- c(
    "P1 - Point sources",
    "P2 - Diffuse sources",
    "P3 - Abstraction",
    "P4 - Hydromorphology",
    "P5 - Introduced species and litter",
    "P7 - Anthropogenic pressure - Other",
    "P8 - Anthropogenic pressure - Unknown"
  )
  
  pressure_group_names <- c(
    "P1 - Point sources" = "point_sources",
    "P2 - Diffuse sources" = "diffuse_sources",
    "P3 - Abstraction" = "abstraction",
    "P4 - Hydromorphology" = "hydromorphology",
    "P5 - Introduced species and litter" = "introduced_species_litter",
    "P7 - Anthropogenic pressure - Other" = "anthropogenic_other",
    "P8 - Anthropogenic pressure - Unknown" = "anthropogenic_unknown"
  )
  
  pressure_cols <- paste0("pressure_", pressure_group_names)
  
  timer$start("Read WFD parquets (reference, substance, pressure)")
  ref <- arrow::read_parquet(
    file.path(wfd_root, "parquet_tables_cleaned", "swb_reference_filtered", "part-0.parquet")
  ) |>
    dplyr::select(cYear, euSurfaceWaterBodyCode, naturalAWBHMWB)
  
  substance <- arrow::read_parquet(
    file.path(wfd_root, "parquet_tables_cleaned", "swb_surface_water_body_sw_priority_substance", "part-0.parquet")
  )
  
  pressure <- arrow::read_parquet(
    file.path(wfd_root, "parquet_tables_cleaned", "swb_surface_water_body_sw_significant_pressure_type", "part-0.parquet")
  )
  timer$done(sprintf("ref=%d, substance=%d, pressure=%d rows",
                     nrow(ref), nrow(substance), nrow(pressure)))
  
  timer$start("Build pressure matrix flags")
  
  # HMWB/AWB from reference table
  ref_flags <- ref |>
    dplyr::rename(n_awb_hmwb = naturalAWBHMWB) |>
    dplyr::mutate(n_awb_hmwb = snakecase::to_snake_case(n_awb_hmwb))
  
  # Substance failing flags
  substance_prepped <- substance |>
    dplyr::filter(swPrioritySubstanceCode %in% kept_substances) |>
    dplyr::mutate(
      failing = dplyr::case_when(
        swPrioritySubstanceCausingFailure == "Yes" ~ 1L,
        swPrioritySubstanceCausingFailure == "No" ~ 0L,
        TRUE ~ -99L
      ),
      substance_short = substance_names[swPrioritySubstanceCode]
    ) |>
    dplyr::select(cYear, euSurfaceWaterBodyCode, substance_short, failing)
  
  substance_wide <- substance_prepped |>
    tidyr::pivot_wider(
      names_from = substance_short,
      values_from = failing,
      names_prefix = "failing_",
      values_fill = 0L
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(substance_cols),
                    ~ dplyr::if_else(. == -99L, NA_integer_, .))
    )
  
  # uPBT flag
  upbt_flag <- substance |>
    dplyr::filter(swPrioritySubstanceCode %in% kept_substances) |>
    dplyr::mutate(
      is_upbt = dplyr::if_else(
        uPBT == "Yes" & swPrioritySubstanceCausingFailure == "Yes", 1L, 0L
      )
    ) |>
    dplyr::summarise(
      failing_upbt = max(is_upbt, na.rm = TRUE),
      .by = c(cYear, euSurfaceWaterBodyCode)
    )
  
  # Pressure type flags
  pressure_wide <- pressure |>
    dplyr::filter(swSignificantPressureTypeGroup %in% kept_pressure_groups) |>
    dplyr::mutate(
      pressure_short = pressure_group_names[swSignificantPressureTypeGroup]
    ) |>
    dplyr::distinct(cYear, euSurfaceWaterBodyCode, pressure_short) |>
    dplyr::mutate(present = 1L) |>
    tidyr::pivot_wider(
      names_from = pressure_short,
      values_from = present,
      names_prefix = "pressure_",
      values_fill = 0L
    )
  
  # Track which water bodies have substance data
  bodies_with_substance <- substance_wide |>
    dplyr::distinct(cYear, euSurfaceWaterBodyCode) |>
    dplyr::mutate(.has_substance = TRUE)
  
  # Assemble
  pm <- ref_flags |>
    dplyr::left_join(substance_wide, by = c("cYear", "euSurfaceWaterBodyCode")) |>
    dplyr::left_join(bodies_with_substance, by = c("cYear", "euSurfaceWaterBodyCode")) |>
    dplyr::left_join(upbt_flag, by = c("cYear", "euSurfaceWaterBodyCode")) |>
    dplyr::left_join(pressure_wide, by = c("cYear", "euSurfaceWaterBodyCode")) |>
    dplyr::mutate(
      .has_substance = tidyr::replace_na(.has_substance, FALSE),
      dplyr::across(
        dplyr::all_of(substance_cols),
        ~ dplyr::if_else(!.has_substance & is.na(.), 0L, .)
      ),
      failing_upbt = dplyr::if_else(
        !.has_substance & is.na(failing_upbt), 0L, failing_upbt
      ),
      dplyr::across(dplyr::all_of(pressure_cols), ~ tidyr::replace_na(., 0L))
    ) |>
    # Keep the flag so "no substance data" (coded 0 above) can be told apart from "monitored, not failing"
    dplyr::rename(has_substance_data = .has_substance)
  
  timer$done(sprintf("%d x %d", nrow(pm), ncol(pm)))
  pm
}
# Prepare CLC
prepare_clc <- function(clc_path, year_map, timer) {
  timer$start("Prepare CLC land cover")
  
  clc_raw <- arrow::read_parquet(clc_path)
  
  clc_level1 <- clc_raw |>
    dplyr::mutate(
      clc_year = as.integer(clc_year),
      clc_l1 = dplyr::case_when(
        clc_code >= 1  & clc_code <= 11 ~ "clc_artificial_m2",
        clc_code >= 12 & clc_code <= 22 ~ "clc_agricultural_m2",
        clc_code >= 23 & clc_code <= 34 ~ "clc_forest_seminatural_m2",
        clc_code >= 35 & clc_code <= 39 ~ "clc_wetlands_m2",
        clc_code >= 40 & clc_code <= 44 ~ "clc_water_m2",
        TRUE ~ "clc_other_m2"
      )
    ) |>
    dplyr::group_by(WSO1_ID, clc_year, clc_l1) |>
    dplyr::summarise(area_m2 = sum(area_m2, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = clc_l1, values_from = area_m2, values_fill = 0)
  
  result <- year_map |>
    dplyr::select(cYear, clc_year) |>
    dplyr::inner_join(clc_level1, by = "clc_year") |>
    dplyr::select(-clc_year)
  
  timer$done(sprintf("%d rows", nrow(result)))
  result
}
# Reads each parquet once (as separate dataframes, not bound — to stay within
# memory), uses matrixStats for fast row operations
prepare_climate_bioclim <- function(climate_dir, timer) {
  timer$start("Prepare climate bioclim variables")
  
  if (!requireNamespace("matrixStats", quietly = TRUE)) {
    message("  Installing matrixStats for fast row operations...")
    install.packages("matrixStats", repos = "https://cloud.r-project.org",
                     quiet = TRUE)
  }
  
  needed_files <- c(
    tg = "climate_tg_mean.parquet",
    tn = "climate_tn_mean.parquet",
    tx = "climate_tx_mean.parquet",
    rr = "climate_rr_sum.parquet",
    hu = "climate_hu_mean.parquet"
  )
  
  period_map <- tibble::tribble(
    ~cYear, ~year_start, ~year_end,
    "2010",       2004L,     2010L,
    "2016",       2011L,     2016L,
    "2022",       2017L,     2022L
  )
  
  # Read each parquet once into a named list (not bound together, to avoid
  # a single 6GB+ dataframe that blows memory)
  message("  Reading 5 climate parquets...")
  var_data <- purrr::map(names(needed_files), \(var) {
    arrow::read_parquet(
      file.path(climate_dir, needed_files[[var]]),
      col_select = c("WSO1_ID", "month", "year", "value")
    ) |>
      dplyr::filter(year >= 2004L, year <= 2022L)
  })
  names(var_data) <- names(needed_files)
  
  # Helper: for one variable and one period, compute monthly climatology
  monthly_clim <- function(df, yr_start, yr_end) {
    df |>
      dplyr::filter(year >= yr_start, year <= yr_end) |>
      dplyr::group_by(WSO1_ID, month) |>
      dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  }
  
  results <- purrr::map(1:nrow(period_map), \(i) {
    p <- period_map[i, ]
    message(sprintf("  Climate period %s (%d-%d)...",
                    p$cYear, p$year_start, p$year_end))
    
    # Build monthly climatologies per variable, then pivot to wide
    period_long <- purrr::map(names(var_data), \(var) {
      monthly_clim(var_data[[var]], p$year_start, p$year_end) |>
        dplyr::mutate(variable = var)
    }) |>
      dplyr::bind_rows()
    
    monthly_wide <- period_long |>
      tidyr::pivot_wider(names_from = c(variable, month),
                         values_from = value, names_sep = "_")
    rm(period_long)
    
    ids <- monthly_wide$WSO1_ID
    
    tmin_m <- as.matrix(monthly_wide[paste0("tn_", 1:12)])
    tmax_m <- as.matrix(monthly_wide[paste0("tx_", 1:12)])
    tavg_m <- as.matrix(monthly_wide[paste0("tg_", 1:12)])
    prec_m <- as.matrix(monthly_wide[paste0("rr_", 1:12)])
    hu_m <- as.matrix(monthly_wide[paste0("hu_", 1:12)])
    rm(monthly_wide)
    
    diurnal_m <- tmax_m - tmin_m
    
    # Sliding 3-month window index matrix
    win_idx <- matrix(0L, nrow = 12, ncol = 12)
    for (k in 0:2) {
      for (m in 1:12) win_idx[m, ((m - 1 + k) %% 12) + 1] <- 1L
    }
    
    wet_m <- prec_m %*% win_idx
    tmpq_m <- (tavg_m %*% win_idx) / 3
    huq_m <- (hu_m %*% win_idx) / 3
    
    wettest_q <- max.col(wet_m, ties.method = "first")
    driest_q <- max.col(-wet_m, ties.method = "first")
    warmest_q <- max.col(tmpq_m, ties.method = "first")
    coldest_q <- max.col(-tmpq_m, ties.method = "first")
    
    # Vectorised row operations via matrixStats
    row_maxs <- matrixStats::rowMaxs
    row_mins <- matrixStats::rowMins
    row_sds <- matrixStats::rowSds
    row_means <- function(m) rowMeans(m, na.rm = TRUE)
    row_sums <- function(m) rowSums(m, na.rm = TRUE)
    pick <- function(m, idx) m[cbind(seq_along(idx), idx)]
    
    annual_range <- row_maxs(tmax_m, na.rm = TRUE) -
      row_mins(tmin_m, na.rm = TRUE)
    
    out <- tibble::tibble(
      WSO1_ID = ids, cYear = p$cYear,
      bio01 = row_means(tavg_m), bio02 = row_means(diurnal_m),
      bio03 = ifelse(annual_range == 0, NA_real_,
                     100 * row_means(diurnal_m) / annual_range),
      bio04 = 100 * row_sds(tavg_m, na.rm = TRUE),
      bio05 = row_maxs(tmax_m, na.rm = TRUE),
      bio06 = row_mins(tmin_m, na.rm = TRUE),
      bio07 = annual_range,
      bio08 = pick(tmpq_m, wettest_q), bio09 = pick(tmpq_m, driest_q),
      bio10 = row_maxs(tmpq_m, na.rm = TRUE),
      bio11 = row_mins(tmpq_m, na.rm = TRUE),
      bio12 = row_sums(prec_m), bio13 = row_maxs(prec_m, na.rm = TRUE),
      bio14 = row_mins(prec_m, na.rm = TRUE),
      bio15 = 100 * row_sds(prec_m + 1, na.rm = TRUE) /
        row_means(prec_m + 1),
      bio16 = row_maxs(wet_m, na.rm = TRUE),
      bio17 = row_mins(wet_m, na.rm = TRUE),
      bio18 = pick(wet_m, warmest_q), bio19 = pick(wet_m, coldest_q),
      hu_annual_mean = row_means(hu_m),
      hu_seasonality = 100 * row_sds(hu_m, na.rm = TRUE),
      hu_max_month = row_maxs(hu_m, na.rm = TRUE),
      hu_min_month = row_mins(hu_m, na.rm = TRUE),
      hu_range = row_maxs(hu_m, na.rm = TRUE) -
        row_mins(hu_m, na.rm = TRUE),
      hu_wettest_qtr = pick(huq_m, wettest_q),
      hu_driest_qtr = pick(huq_m, driest_q),
      hu_warmest_qtr = pick(huq_m, warmest_q),
      hu_coldest_qtr = pick(huq_m, coldest_q)
    )
    
    rm(tmin_m, tmax_m, tavg_m, prec_m, hu_m,
       diurnal_m, wet_m, tmpq_m, huq_m)
    out
  }) |>
    dplyr::bind_rows()
  
  rm(var_data); gc()
  
  result <- results |>
    dplyr::mutate(dplyr::across(bio01:hu_coldest_qtr,
                                ~dplyr::if_else(is.infinite(.), NA_real_, .))) |>
    dplyr::select(cYear, WSO1_ID, bio01:bio19,
                  hu_annual_mean:hu_coldest_qtr)
  
  timer$done(sprintf("%d rows, %d columns", nrow(result), ncol(result)))
  result
}
# Prepare nitrogen
prepare_nitrogen <- function(nitrogen_path, year_map, timer) {
  timer$start("Prepare nitrogen")
  nitrogen <- arrow::read_parquet(nitrogen_path)
  
  # Average across each RBMP period rather than using a single mapped year
  period_ranges <- dplyr::tibble(
    cYear = c("2010", "2016", "2022"),
    start = c(2004L, 2011L, 2017L),
    end = c(2010L, 2016L, 2019L)
  )
  
  result <- period_ranges |>
    purrr::pmap(\(cYear, start, end) {
      nitrogen |>
        dplyr::filter(year >= start, year <= end) |>
        dplyr::group_by(WSO1_ID) |>
        dplyr::summarise(n_surplus_kg_ha = mean(n_surplus_kg_ha, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::mutate(cYear = cYear)
    }) |>
    purrr::list_rbind()
  
  timer$done(sprintf("%d rows", nrow(result)))
  result
}
# Prepare phosphorus
prepare_phosphorus <- function(phosphorus_path, year_map, timer) {
  timer$start("Prepare phosphorus")
  phosphorus <- arrow::read_parquet(phosphorus_path)
  
  # Average across each RBMP period rather than using a single mapped year
  period_ranges <- dplyr::tibble(
    cYear = c("2010", "2016", "2022"),
    start = c(2004L, 2011L, 2017L),
    end = c(2010L, 2016L, 2019L)
  )
  
  result <- period_ranges |>
    purrr::pmap(\(cYear, start, end) {
      phosphorus |>
        dplyr::filter(year >= start, year <= end) |>
        dplyr::group_by(WSO1_ID) |>
        dplyr::summarise(p_surplus_kg_ha = mean(p_surplus_kg_ha, na.rm = TRUE),
                         .groups = "drop") |>
        dplyr::mutate(cYear = cYear)
    }) |>
    purrr::list_rbind()
  
  timer$done(sprintf("%d rows", nrow(result)))
  result
}
# Prepare imperviousness
prepare_imperviousness <- function(imd_path, year_map, timer) {
  timer$start("Prepare imperviousness")
  imd <- arrow::read_parquet(imd_path)
  
  result <- year_map |>
    dplyr::select(cYear, imd_year) |>
    dplyr::inner_join(imd, by = c("imd_year" = "year")) |>
    dplyr::select(-imd_year)
  
  timer$done(sprintf("%d rows", nrow(result)))
  result
}
# Prepare AMBER
prepare_amber <- function(amber_path, timer) {
  timer$start("Prepare AMBER barriers")
  amber_raw <- arrow::read_parquet(amber_path)
  
  amber_total <- amber_raw |>
    dplyr::distinct(WSO1_ID, n_barriers_total)
  
  amber_by_type <- amber_raw |>
    dplyr::mutate(
      col_name = paste0("amber_n_", tolower(gsub("[/ ]", "_", Type)))
    ) |>
    dplyr::select(WSO1_ID, col_name, n_barriers) |>
    tidyr::pivot_wider(names_from = col_name,
                       values_from = n_barriers, values_fill = 0L)
  
  result <- amber_total |>
    dplyr::left_join(amber_by_type, by = "WSO1_ID") |>
    dplyr::rename(amber_n_total = n_barriers_total)
  
  timer$done(sprintf("%d rows", nrow(result)))
  result
}

# Merge drivers onto pressure matrix
merge_drivers <- function(pressure_matrix, drivers_root, workflow_gpkg, timer,
                          cache_dir, use_cache = TRUE) {
  
  # Land cover and imperviousness years per cycle; climate, N and P use period averages instead
  year_map <- tibble::tribble(
    ~cYear, ~clc_year, ~imd_year,
    "2010",     2006L,     2009L,
    "2016",     2012L,     2015L,
    "2022",     2018L,     2018L
  )
  
  # Read bridge table via DBI (no geometry, skip sf overhead)
  # Keep the primary catchment area, used as the weight when averaging rates over a water body's catchments
  timer$start("Read sWB_CCM bridge table")
  con <- DBI::dbConnect(RSQLite::SQLite(), workflow_gpkg)
  swb_ccm <- DBI::dbReadTable(con, "sWB_CCM_tbl") |>
    tibble::as_tibble() |>
    dplyr::select(WB_code, WSO1_ID, PC_area) |>
    dplyr::mutate(
      .weight = dplyr::coalesce(PC_area, 1)
    ) |>
    dplyr::select(WB_code, WSO1_ID, .weight)
  DBI::dbDisconnect(con)
  timer$done(sprintf("%d rows", nrow(swb_ccm)))
  
  # Input paths for each driver (outputs of the extract_drivers pipeline), also used to spot stale caches
  clc_path <- file.path(drivers_root, "land_cover", "outputs", "clc_catchment_area_by_class.parquet")
  climate_dir <- file.path(drivers_root, "climate", "outputs")
  climate_files <- file.path(climate_dir, c(
    "climate_tg_mean.parquet", "climate_tn_mean.parquet", "climate_tx_mean.parquet",
    "climate_rr_sum.parquet", "climate_hu_mean.parquet"
  ))
  nitrogen_path <- file.path(drivers_root, "nitrogen", "outputs", "nitrogen_catchment.parquet")
  phosphorus_path <- file.path(drivers_root, "phosphorus", "outputs", "phosphorus_catchment.parquet")
  imd_path <- file.path(drivers_root, "imperviousness_density", "outputs", "imperviousness_catchment.parquet")
  amber_path <- file.path(drivers_root, "dams", "outputs", "amber_catchment.parquet")
  
  # Prepare each driver, reading the cache unless it is missing or older than its input
  clc <- cached_prepare(
    file.path(cache_dir, "cache_clc_prepared.parquet"),
    \() prepare_clc(clc_path, year_map, timer),
    timer, "CLC land cover", use_cache, clc_path
  )
  
  climate <- cached_prepare(
    file.path(cache_dir, "cache_climate_bioclim.parquet"),
    \() prepare_climate_bioclim(climate_dir, timer),
    timer, "Climate bioclim", use_cache, climate_files
  )
  
  nitrogen <- cached_prepare(
    file.path(cache_dir, "cache_nitrogen_prepared.parquet"),
    \() prepare_nitrogen(nitrogen_path, year_map, timer),
    timer, "Nitrogen", use_cache, nitrogen_path
  )
  
  phosphorus <- cached_prepare(
    file.path(cache_dir, "cache_phosphorus_prepared.parquet"),
    \() prepare_phosphorus(phosphorus_path, year_map, timer),
    timer, "Phosphorus", use_cache, phosphorus_path
  )
  
  imd <- cached_prepare(
    file.path(cache_dir, "cache_imd_prepared.parquet"),
    \() prepare_imperviousness(imd_path, year_map, timer),
    timer, "Imperviousness", use_cache, imd_path
  )
  
  amber <- cached_prepare(
    file.path(cache_dir, "cache_amber_prepared.parquet"),
    \() prepare_amber(amber_path, timer),
    timer, "AMBER barriers", use_cache, amber_path
  )
  
  # Assemble time-varying drivers
  timer$start("Assemble driver table (joins)")
  driver_tbl <- clc |>
    dplyr::full_join(climate, by = c("cYear", "WSO1_ID")) |>
    dplyr::full_join(nitrogen, by = c("cYear", "WSO1_ID")) |>
    dplyr::full_join(phosphorus, by = c("cYear", "WSO1_ID")) |>
    dplyr::full_join(imd, by = c("cYear", "WSO1_ID"))
  rm(clc, climate, nitrogen, phosphorus, imd); gc()
  
  # Time-invariant drivers
  driver_tbl <- driver_tbl |>
    dplyr::left_join(amber, by = "WSO1_ID")
  # dplyr::left_join(ld, by = "WSO1_ID")
  rm(amber); gc()
  
  # Catchments with no barriers get 0
  amber_cols <- names(driver_tbl)[grepl("^amber_", names(driver_tbl))]
  driver_tbl <- driver_tbl |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(amber_cols), \(x) dplyr::coalesce(x, 0L)
    ))
  timer$done(sprintf("%d rows, %d cols", nrow(driver_tbl), ncol(driver_tbl)))
  
  # Join drivers to bridge (many catchments per water body)
  timer$start("Spatial join via bridge table")
  driver_by_wb <- swb_ccm |>
    dplyr::inner_join(driver_tbl, by = "WSO1_ID")
  rm(driver_tbl, swb_ccm); gc()
  timer$done(sprintf("%d rows after bridge join", nrow(driver_by_wb)))
  
  # Aggregate across all proximal catchments per water body,
  # weighted by primary catchment area (PC_area, or 1 where it is missing)
  timer$start("Aggregate drivers per water body (catchment-area-weighted)")
  
  clc_area_cols <- c(
    "clc_artificial_m2", "clc_agricultural_m2", "clc_forest_seminatural_m2",
    "clc_wetlands_m2", "clc_water_m2", "clc_other_m2"
  )
  # Extensive variables (areas, counts): sum
  sum_cols <- intersect(
    c(clc_area_cols, "impervious_area_m2", amber_cols),
    names(driver_by_wb)
  )
  # Intensive variables (rates, percentages, indices): weighted mean
  mean_cols <- setdiff(
    names(driver_by_wb),
    c("WB_code", "WSO1_ID", "cYear", ".weight", sum_cols)
  )
  
  # Weighted mean that handles NA weights and all-NA values
  wmean <- function(x, w) {
    keep <- !is.na(x) & !is.na(w)
    if (!any(keep)) return(NA_real_)
    stats::weighted.mean(x[keep], w[keep])
  }
  
  # Use data.table for faster grouped aggregation when available
  if (requireNamespace("data.table", quietly = TRUE)) {
    dt <- data.table::as.data.table(driver_by_wb)
    rm(driver_by_wb); gc()
    
    sum_agg <- dt[, lapply(.SD, \(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)),
                  by = .(WB_code, cYear), .SDcols = sum_cols]
    
    # Area-weighted mean for intensive variables
    mean_agg <- dt[, lapply(.SD, \(x) wmean(x, .weight)),
                   by = .(WB_code, cYear), .SDcols = mean_cols]
    
    driver_agg <- sum_agg[mean_agg, on = .(WB_code, cYear)]
    driver_agg <- tibble::as_tibble(driver_agg)
    rm(dt, sum_agg, mean_agg)
  } else {
    # Fallback to dplyr
    driver_agg <- driver_by_wb |>
      dplyr::group_by(WB_code, cYear) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(sum_cols),
          \(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
        ),
        dplyr::across(
          dplyr::all_of(mean_cols),
          \(x) wmean(x, .weight)
        ),
        .groups = "drop"
      )
    rm(driver_by_wb)
  }
  gc()
  timer$done(sprintf("%d water body-year rows", nrow(driver_agg)))
  
  timer$start("Final join: drivers onto pressure matrix")
  result <- pressure_matrix |>
    dplyr::left_join(driver_agg,
                     by = c("euSurfaceWaterBodyCode" = "WB_code", "cYear"),
                     relationship = "many-to-one")
  timer$done(sprintf("%d x %d", nrow(result), ncol(result)))
  result
}
# Build model-ready dataframe
build_model_ready <- function(pressure_with_drivers, wfd_root,
                              filter_to_drivers = FALSE, timer) {
  
  timer$start("Build model-ready dataframe (BQE join)")
  bqe <- arrow::read_parquet(
    file.path(wfd_root, "parquet_tables_joined", "joined_bqe_ref_w2010_final.parquet")
  )
  status_levels <- c("1", "2", "3", "4", "5")
  status_labels <- c("High", "Good", "Moderate", "Poor", "Bad")
  
  model_df <- bqe |>
    dplyr::select(
      cYear, euSurfaceWaterBodyCode, qeCode,
      qeStatusOrPotentialValue, countryCode, rbdName,
      surfaceWaterBodyCategory
    ) |>
    dplyr::left_join(pressure_with_drivers,
                     by = c("cYear", "euSurfaceWaterBodyCode"),
                     relationship = "many-to-one") |>
    dplyr::mutate(
      qeStatus = factor(qeStatusOrPotentialValue,
                        levels = status_levels,
                        labels = status_labels,
                        ordered = TRUE),
      qeCode = factor(qeCode),
      countryCode = factor(countryCode),
      cYear = factor(cYear)
    )
  
  # The left join must keep exactly one row per BQE record
  stopifnot(nrow(model_df) == nrow(bqe))
  
  if (filter_to_drivers) {
    model_df <- model_df |>
      dplyr::filter(!is.na(bio01))
  }
  
  timer$done(sprintf("%d rows, %d columns", nrow(model_df), ncol(model_df)))
  model_df
}
# Diagnostics
run_diagnostics <- function(model_df) {
  
  n_total <- nrow(model_df)
  n_matched <- model_df |>
    dplyr::filter(!is.na(n_awb_hmwb)) |>
    nrow()
  cat("Total BQE rows:", n_total, "\n")
  cat("Matched to pressure matrix:", n_matched,
      sprintf("(%.1f%%)\n", 100 * n_matched / n_total))
  cat("Unmatched:", n_total - n_matched, "\n\n")
  
  all_joined_cols <- setdiff(
    names(model_df),
    c("cYear", "euSurfaceWaterBodyCode", "qeCode",
      "qeStatusOrPotentialValue", "countryCode", "rbdName",
      "surfaceWaterBodyCategory", "qeStatus")
  )
  
  year_counts <- model_df |> dplyr::count(cYear, name = "n_total")
  
  na_summary <- model_df |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(all_joined_cols), \(x) sum(is.na(x))),
      .by = cYear
    ) |>
    tidyr::pivot_longer(-cYear, names_to = "column", values_to = "n_na") |>
    dplyr::left_join(year_counts, by = "cYear") |>
    dplyr::mutate(pct_na = round(100 * n_na / n_total, 1))
  
  cat("NA summary:\n")
  print(na_summary, n = Inf)
  
  cat("\nRows per cycle x BQE:\n")
  model_df |>
    dplyr::count(cYear, qeCode) |>
    tidyr::pivot_wider(names_from = cYear, values_from = n) |>
    print(n = Inf)
}
# Run the whole driver matrix build and write pressure_matrix, pressure_matrix_with_drivers and model_ready to model_dir
# use_cache: if TRUE, reads cached driver parquets when they exist instead of recomputing.
#   Set FALSE (or delete cache files) to force a fresh compute
run_driver_pipeline <- function(wfd_root, drivers_root, workflow_gpkg, cache_dir, model_dir, use_cache = TRUE) {
  timer <- make_timer()
  pipeline_start <- proc.time()

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)

  if (use_cache) {
    message("Cache enabled, cached drivers will be read from: ", cache_dir)
    message("To force recompute, set use_cache = FALSE")
    message("Or delete individual cache files to recompute just that driver.\n")
  }

  # Step 1: pressure matrix
  pm <- build_pressure_matrix(wfd_root, timer)
  cat("Pressure matrix dimensions:", nrow(pm), "x", ncol(pm), "\n")

  timer$start("Write pressure_matrix.parquet")
  arrow::write_parquet(pm, file.path(model_dir, "pressure_matrix.parquet"))
  timer$done()

  # Step 2: merge drivers
  pm_drivers <- merge_drivers(pm, drivers_root, workflow_gpkg, timer, cache_dir, use_cache)
  rm(pm)
  gc()
  cat("Driver merge dimensions:", nrow(pm_drivers), "x", ncol(pm_drivers), "\n")

  timer$start("Write pressure_matrix_with_drivers.parquet")
  arrow::write_parquet(pm_drivers, file.path(model_dir, "pressure_matrix_with_drivers.parquet"))
  timer$done()

  # Step 3: build model-ready df
  model_df <- build_model_ready(pm_drivers, wfd_root, filter_to_drivers = FALSE, timer)
  rm(pm_drivers)
  gc()

  # Step 4: diagnostics
  timer$start("Run diagnostics")
  run_diagnostics(model_df)
  timer$done()

  # Step 5: write output
  timer$start("Write model_ready.parquet")
  arrow::write_parquet(model_df, file.path(model_dir, "model_ready.parquet"))
  timer$done()
  cat("\nSaved model_ready.parquet:", nrow(model_df), "rows,", ncol(model_df), "columns\n")

  # Print the full timing breakdown
  timer$summary()
  total_elapsed <- (proc.time() - pipeline_start)[["elapsed"]]
  message(sprintf("\nTotal pipeline wall time: %.1fs (%.1f min)", total_elapsed, total_elapsed / 60))

  model_df
}
