# ============================================================
# data_preparation.R
# Home Credit Default Risk - Data Preparation (Reusable Functions)
#
# Goals:
# - Clean and transform application data (train/test) consistently
# - Engineer features (demographic + financial ratios + missing indicators + bins)
# - Aggregate supplementary tables to applicant level (SK_ID_CURR)
# - Join aggregated features back to application data
# - Ensure train/test consistency by fitting parameters ONLY on train
#   and reusing them for test (prevents leakage)
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
})

# ---------------------------
# Utility helpers
# ---------------------------

safe_read_csv <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  readr::read_csv(path, show_col_types = FALSE)
}

# Winsorize numeric vector using training quantiles
winsorize_vec <- function(x, lo, hi) {
  x <- ifelse(is.na(x), NA, x)
  pmin(pmax(x, lo), hi)
}

# Make missing indicator columns for selected variables
add_missing_indicators <- function(df, cols) {
  for (nm in cols) {
    if (nm %in% names(df)) {
      df[[paste0(nm, "_NA")]] <- as.integer(is.na(df[[nm]]))
    }
  }
  df
}

# Ensure columns alignment: add missing cols with NA, order consistently
align_columns <- function(df, reference_cols) {
  missing_cols <- setdiff(reference_cols, names(df))
  for (mc in missing_cols) df[[mc]] <- NA
  df <- df[, reference_cols, drop = FALSE]
  df
}

# ---------------------------
# 1) Clean + base transforms
# ---------------------------

clean_application_base <- function(df) {
  # Fix placeholder anomaly DAYS_EMPLOYED == 365243 -> NA
  if ("DAYS_EMPLOYED" %in% names(df)) {
    df <- df %>%
      mutate(DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED))
  }

  # Convert negative day-based fields into positive years (common in this dataset)
  # DAYS_BIRTH is negative: age in years = -DAYS_BIRTH / 365.25
  if ("DAYS_BIRTH" %in% names(df)) {
    df <- df %>%
      mutate(AGE_YEARS = (-DAYS_BIRTH) / 365.25)
  }

  # Employment duration: usually negative days (past), convert to years
  if ("DAYS_EMPLOYED" %in% names(df)) {
    df <- df %>%
      mutate(EMPLOYED_YEARS = abs(DAYS_EMPLOYED) / 365.25)
  }

  # Registration / ID / etc. can be negative too; convert to positive years if present
  day_cols <- c("DAYS_REGISTRATION", "DAYS_ID_PUBLISH", "DAYS_LAST_PHONE_CHANGE")
  for (cname in day_cols) {
    if (cname %in% names(df)) {
      df[[paste0(cname, "_YEARS")]] <- abs(df[[cname]]) / 365.25
    }
  }

  df
}

# ---------------------------
# 2) Feature engineering on application data
# ---------------------------

engineer_application_features <- function(df) {
  # Financial ratios (only if columns exist)
  df <- df %>%
    mutate(
      CREDIT_INCOME_RATIO = ifelse(
        all(c("AMT_CREDIT", "AMT_INCOME_TOTAL") %in% names(df)) & AMT_INCOME_TOTAL > 0,
        AMT_CREDIT / AMT_INCOME_TOTAL,
        NA_real_
      ),
      ANNUITY_INCOME_RATIO = ifelse(
        all(c("AMT_ANNUITY", "AMT_INCOME_TOTAL") %in% names(df)) & AMT_INCOME_TOTAL > 0,
        AMT_ANNUITY / AMT_INCOME_TOTAL,
        NA_real_
      ),
      CREDIT_ANNUITY_RATIO = ifelse(
        all(c("AMT_CREDIT", "AMT_ANNUITY") %in% names(df)) & AMT_ANNUITY > 0,
        AMT_CREDIT / AMT_ANNUITY,
        NA_real_
      ),
      GOODS_PRICE_CREDIT_RATIO = ifelse(
        all(c("AMT_GOODS_PRICE", "AMT_CREDIT") %in% names(df)) & AMT_CREDIT > 0,
        AMT_GOODS_PRICE / AMT_CREDIT,
        NA_real_
      ),
      INCOME_PER_FAM = ifelse(
        all(c("AMT_INCOME_TOTAL", "CNT_FAM_MEMBERS") %in% names(df)) & CNT_FAM_MEMBERS > 0,
        AMT_INCOME_TOTAL / CNT_FAM_MEMBERS,
        NA_real_
      )
    )

  # Simple interaction terms (example)
  if (all(c("AGE_YEARS", "EMPLOYED_YEARS") %in% names(df))) {
    df <- df %>%
      mutate(AGE_EMPLOYED_INTERACTION = AGE_YEARS * EMPLOYED_YEARS)
  }

  df
}

# Binning using training thresholds only (passed in as params)
apply_binning <- function(df, bin_params) {
  # bin_params is a list with thresholds for AGE_YEARS and INCOME
  if ("AGE_YEARS" %in% names(df) && !is.null(bin_params$age_breaks)) {
    df$AGE_BIN <- cut(
      df$AGE_YEARS,
      breaks = bin_params$age_breaks,
      include.lowest = TRUE,
      right = FALSE
    )
  }

  if ("AMT_INCOME_TOTAL" %in% names(df) && !is.null(bin_params$income_breaks)) {
    df$INCOME_BIN <- cut(
      df$AMT_INCOME_TOTAL,
      breaks = bin_params$income_breaks,
      include.lowest = TRUE,
      right = FALSE
    )
  }

  df
}

# ---------------------------
# 3) Supplementary aggregations to SK_ID_CURR
# ---------------------------

aggregate_bureau <- function(bureau_df) {
  # Example features:
  # - number of bureau records
  # - count active vs closed
  # - max/mean days past due (if present)
  out <- bureau_df %>%
    group_by(SK_ID_CURR) %>%
    summarise(
      BUREAU_COUNT = n(),
      BUREAU_ACTIVE_COUNT = sum(CREDIT_ACTIVE == "Active", na.rm = TRUE),
      BUREAU_CLOSED_COUNT = sum(CREDIT_ACTIVE == "Closed", na.rm = TRUE),
      BUREAU_DPD_MEAN = if ("CREDIT_DAY_OVERDUE" %in% names(bureau_df))
        mean(CREDIT_DAY_OVERDUE, na.rm = TRUE) else NA_real_,
      BUREAU_DPD_MAX = if ("CREDIT_DAY_OVERDUE" %in% names(bureau_df))
        max(CREDIT_DAY_OVERDUE, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
  out
}

aggregate_previous_application <- function(prev_df) {
  # Example features:
  # - count previous applications
  # - approval rate / refusal rate
  out <- prev_df %>%
    group_by(SK_ID_CURR) %>%
    summarise(
      PREV_APP_COUNT = n(),
      PREV_APPROVED_COUNT = sum(NAME_CONTRACT_STATUS == "Approved", na.rm = TRUE),
      PREV_REFUSED_COUNT = sum(NAME_CONTRACT_STATUS == "Refused", na.rm = TRUE),
      PREV_APPROVAL_RATE = ifelse(PREV_APP_COUNT > 0, PREV_APPROVED_COUNT / PREV_APP_COUNT, NA_real_),
      PREV_REFUSAL_RATE = ifelse(PREV_APP_COUNT > 0, PREV_REFUSED_COUNT / PREV_APP_COUNT, NA_real_),
      .groups = "drop"
    )
  out
}

aggregate_installments <- function(inst_df) {
  # Late payment percent and payment trend-like summaries
  # Common fields: DAYS_ENTRY_PAYMENT, DAYS_INSTALMENT, AMT_PAYMENT, AMT_INSTALMENT
  # Late if DAYS_ENTRY_PAYMENT > DAYS_INSTALMENT
  out <- inst_df %>%
    mutate(
      LATE_DAYS = ifelse(
        all(c("DAYS_ENTRY_PAYMENT", "DAYS_INSTALMENT") %in% names(inst_df)),
        DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
        NA_real_
      ),
      IS_LATE = as.integer(!is.na(LATE_DAYS) & LATE_DAYS > 0),
      PAYMENT_DIFF = ifelse(
        all(c("AMT_PAYMENT", "AMT_INSTALMENT") %in% names(inst_df)),
        AMT_PAYMENT - AMT_INSTALMENT,
        NA_real_
      )
    ) %>%
    group_by(SK_ID_CURR) %>%
    summarise(
      INST_COUNT = n(),
      LATE_PAY_PCT = ifelse(INST_COUNT > 0, mean(IS_LATE, na.rm = TRUE), NA_real_),
      LATE_DAYS_MEAN = mean(LATE_DAYS, na.rm = TRUE),
      PAYMENT_DIFF_MEAN = mean(PAYMENT_DIFF, na.rm = TRUE),
      PAYMENT_DIFF_MEDIAN = median(PAYMENT_DIFF, na.rm = TRUE),
      .groups = "drop"
    )
  out
}

# ---------------------------
# 4) Fit/Transform to ensure train/test consistency
# ---------------------------

fit_prep_params <- function(train_df) {
  # EXT_SOURCE imputation values (median is robust)
  ext_cols <- intersect(c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3"), names(train_df))
  ext_medians <- map_dbl(ext_cols, ~ median(train_df[[.x]], na.rm = TRUE))
  names(ext_medians) <- ext_cols

  # Numeric winsorization thresholds for a few heavy-tailed variables (optional but safe)
  num_cols <- intersect(
    c("AMT_INCOME_TOTAL", "AMT_CREDIT", "AMT_ANNUITY", "AMT_GOODS_PRICE",
      "CREDIT_INCOME_RATIO", "ANNUITY_INCOME_RATIO", "CREDIT_ANNUITY_RATIO"),
    names(train_df)
  )

  winsor_bounds <- map(num_cols, function(cn) {
    x <- train_df[[cn]]
    lo <- quantile(x, probs = 0.01, na.rm = TRUE, names = FALSE)
    hi <- quantile(x, probs = 0.99, na.rm = TRUE, names = FALSE)
    c(lo = lo, hi = hi)
  })
  names(winsor_bounds) <- num_cols

  # Binning thresholds (use quantiles from train ONLY)
  age_breaks <- NULL
  if ("AGE_YEARS" %in% names(train_df)) {
    qs <- quantile(train_df$AGE_YEARS, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE, names = FALSE)
    age_breaks <- unique(qs)
    if (length(age_breaks) < 3) age_breaks <- c(min(train_df$AGE_YEARS, na.rm = TRUE), max(train_df$AGE_YEARS, na.rm = TRUE))
  }

  income_breaks <- NULL
  if ("AMT_INCOME_TOTAL" %in% names(train_df)) {
    qs <- quantile(train_df$AMT_INCOME_TOTAL, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
    income_breaks <- unique(qs)
    if (length(income_breaks) < 3) income_breaks <- c(min(train_df$AMT_INCOME_TOTAL, na.rm = TRUE), max(train_df$AMT_INCOME_TOTAL, na.rm = TRUE))
  }

  list(
    ext_medians = ext_medians,
    winsor_bounds = winsor_bounds,
    bin_params = list(age_breaks = age_breaks, income_breaks = income_breaks)
  )
}

apply_prep_params <- function(df, params) {
  # Impute EXT_SOURCE_* with training medians
  for (cn in names(params$ext_medians)) {
    if (cn %in% names(df)) {
      med <- params$ext_medians[[cn]]
      df[[cn]] <- ifelse(is.na(df[[cn]]), med, df[[cn]])
    }
  }

  # Winsorize selected numeric columns
  for (cn in names(params$winsor_bounds)) {
    if (cn %in% names(df)) {
      lo <- params$winsor_bounds[[cn]]["lo"]
      hi <- params$winsor_bounds[[cn]]["hi"]
      df[[cn]] <- winsorize_vec(df[[cn]], lo, hi)
    }
  }

  # Apply binning using train-derived breaks
  df <- apply_binning(df, params$bin_params)

  df
}

# Main function to build final dataset with supplementary joins
prepare_dataset <- function(app_df, supp_list, params, is_train = FALSE) {
  # app_df is already read (train or test)
  # supp_list: list(bureau_agg=..., prev_agg=..., inst_agg=...)
  # params: fitted params from train
  # is_train: whether df contains TARGET

  df <- app_df %>%
    clean_application_base() %>%
    engineer_application_features()

  # Missing indicators (include the ones teacher mentioned + common important)
  mi_cols <- intersect(
    c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3",
      "AMT_ANNUITY", "AMT_GOODS_PRICE", "DAYS_EMPLOYED"),
    names(df)
  )
  df <- add_missing_indicators(df, mi_cols)

  # Apply train-fitted params (impute, winsorize, binning)
  df <- apply_prep_params(df, params)

  # Join supplementary aggregated features
  if (!is.null(supp_list$bureau_agg)) {
    df <- df %>% left_join(supp_list$bureau_agg, by = "SK_ID_CURR")
  }
  if (!is.null(supp_list$prev_agg)) {
    df <- df %>% left_join(supp_list$prev_agg, by = "SK_ID_CURR")
  }
  if (!is.null(supp_list$inst_agg)) {
    df <- df %>% left_join(supp_list$inst_agg, by = "SK_ID_CURR")
  }

  # If train, keep TARGET; if test, ensure TARGET not present
  if (!is_train && "TARGET" %in% names(df)) {
    df <- df %>% select(-TARGET)
  }

  df
}

# Orchestrator: read, aggregate, fit on train, transform both, align columns, save outputs
run_data_preparation <- function(
  data_dir,
  out_dir = data_dir,
  write_outputs = TRUE
) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # ---- Read application data
  train_path <- file.path(data_dir, "application_train.csv")
  test_path  <- file.path(data_dir, "application_test.csv")

  app_train <- safe_read_csv(train_path)
  app_test  <- safe_read_csv(test_path)

  # ---- Read supplementary tables (if files exist, else skip gracefully)
  bureau_path <- file.path(data_dir, "bureau.csv")
  prev_path   <- file.path(data_dir, "previous_application.csv")
  inst_path   <- file.path(data_dir, "installments_payments.csv")

  bureau_agg <- NULL
  prev_agg <- NULL
  inst_agg <- NULL

  if (file.exists(bureau_path)) {
    bureau_df <- safe_read_csv(bureau_path)
    bureau_agg <- aggregate_bureau(bureau_df)
  }

  if (file.exists(prev_path)) {
    prev_df <- safe_read_csv(prev_path)
    prev_agg <- aggregate_previous_application(prev_df)
  }

  if (file.exists(inst_path)) {
    inst_df <- safe_read_csv(inst_path)
    inst_agg <- aggregate_installments(inst_df)
  }

  supp_list <- list(bureau_agg = bureau_agg, prev_agg = prev_agg, inst_agg = inst_agg)

  # ---- Fit params on TRAIN ONLY (prevents leakage)
  # Do base cleaning + feature engineering first so params cover engineered cols
  tmp_train <- app_train %>%
    clean_application_base() %>%
    engineer_application_features()

  params <- fit_prep_params(tmp_train)

  # ---- Transform train/test using same params
  train_prepared <- prepare_dataset(app_train, supp_list, params, is_train = TRUE)
  test_prepared  <- prepare_dataset(app_test,  supp_list, params, is_train = FALSE)

  # ---- Ensure identical columns for modeling (except TARGET)
  # Reference cols: train without TARGET
  ref_cols <- setdiff(names(train_prepared), "TARGET")
  test_prepared <- align_columns(test_prepared, ref_cols)

  # Reorder train to have TARGET last (optional, but tidy)
  train_prepared <- train_prepared %>%
    select(all_of(ref_cols), TARGET)

  # ---- Optionally save outputs (DO NOT commit these files)
  if (write_outputs) {
    saveRDS(train_prepared, file.path(out_dir, "train_prepared.rds"))
    saveRDS(test_prepared,  file.path(out_dir, "test_prepared.rds"))
    saveRDS(params,         file.path(out_dir, "prep_params.rds"))
  }

  list(
    train_prepared = train_prepared,
    test_prepared  = test_prepared,
    params         = params
  )
}

# ---------------------------
# If you want to run from command line / source():
# Uncomment and edit your paths
# ---------------------------
# results <- run_data_preparation(
#   data_dir = "path/to/your/data_folder",
#   out_dir  = "path/to/your/output_folder",
#   write_outputs = TRUE
# )
# str(results$train_prepared)
