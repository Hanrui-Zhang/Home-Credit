# Home Credit – Data Preparation

## Overview

This repository contains a reusable data preparation script for the Home Credit Default Risk project (IS 6850).

The purpose of this script is to translate EDA findings into production-ready, reusable data cleaning and feature engineering functions.

The script ensures consistent transformations for both training and test datasets to prevent data leakage.

---

## What the Script Does

`data_preparation.R` performs the following steps:

### 1. Cleans Application Data
- Fixes placeholder anomaly: `DAYS_EMPLOYED = 365243`
- Converts day-based variables into interpretable year-based features
- Handles missing values
- Adds missing-data indicators

### 2. Feature Engineering
- Creates demographic features (e.g., age in years, employment duration)
- Creates financial ratios:
  - CREDIT_INCOME_RATIO
  - ANNUITY_INCOME_RATIO
  - CREDIT_ANNUITY_RATIO
- Adds interaction terms
- Creates binned features using training-set thresholds

### 3. Aggregates Supplementary Tables
Aggregates data to applicant level (`SK_ID_CURR`):

- `bureau.csv`
  - Count of prior credits
  - Active vs. closed credits
  - Delinquency summaries

- `previous_application.csv`
  - Number of prior applications
  - Approval and refusal rates

- `installments_payments.csv`
  - Late payment percentage
  - Payment difference summaries

### 4. Ensures Train/Test Consistency
- All imputation values and binning thresholds are fit on training data only
- The same transformations are applied to test data
- Prevents data leakage
- Produces identical feature columns for modeling

---

## How to Run

Place the dataset files in a folder.

Then in R:

```r
source("data_preparation.R")

results <- run_data_preparation(
  data_dir = "path/to/data_folder",
  out_dir  = "path/to/output_folder",
  write_outputs = TRUE
)
```

---

## Inputs

Expected files in `data_dir`:

- application_train.csv
- application_test.csv
- bureau.csv (optional)
- previous_application.csv (optional)
- installments_payments.csv (optional)

---

## Outputs

The script generates:

- train_prepared.rds
- test_prepared.rds
- prep_params.rds

Note:
Data files and output files are excluded from version control using `.gitignore`.
