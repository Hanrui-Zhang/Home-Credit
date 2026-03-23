# Home Credit Default Risk Portfolio

## Overview

This repository presents my individual work completed as part of the Home Credit Default Risk analytics project for IS 6850. The goal of the project was to use borrower and credit-related data to better understand default risk and support more informed lending decisions.

This portfolio highlights the notebooks and scripts I personally created during the project. It is intended to demonstrate my ability to perform data cleaning, exploratory data analysis, feature engineering, modeling documentation, and business-focused communication.

---

## Business Problem and Project Objective

Home Credit works with applicants who may have limited or non-traditional credit histories. This creates a challenge for assessing borrower risk accurately. If the company approves too many high-risk borrowers, default losses increase. If it rejects too many creditworthy applicants, it loses business opportunities.

The objective of this project was to analyze borrower data and develop data-driven methods to better predict default risk. The broader goal was to support smarter lending decisions by improving risk evaluation.

---

## Our Group's Solution

Our group approached this problem by building a machine learning workflow for credit risk prediction. The overall solution included:

- cleaning and preparing raw application and supplementary credit data,
- exploring important patterns in borrower characteristics,
- engineering features related to income, credit burden, and repayment behavior,
- training and evaluating predictive models,
- documenting the final model and its practical implications.

This solution aimed to improve the ability to distinguish between lower-risk and higher-risk applicants using historical data.

---

## My Contribution to the Project

My individual contributions focused on the following areas:

- preparing and transforming application-level data for analysis,
- engineering interpretable features from demographic and financial variables,
- aggregating supplementary tables such as bureau, previous applications, and installment payment history,
- creating reusable preprocessing logic to ensure train/test consistency,
- documenting the modeling approach and results in a model card,
- producing notebooks that communicate both technical work and business meaning.

The files in this repository reflect the work I personally completed.

---

## Business Value of the Solution

A more accurate default-risk prediction system can create business value in several ways:

- reduce financial losses from high-risk loans,
- improve approval decisions for applicants who are likely to repay,
- support fairer and more consistent lending processes,
- help risk teams prioritize the most informative borrower attributes,
- improve operational efficiency by providing a repeatable analytics workflow.

In a business setting, this type of solution can help balance growth and risk management.

---

## Challenges and Difficulties Encountered

Several challenges came up during the project:

- handling missing values across multiple data sources,
- combining large supplementary tables at the applicant level,
- preventing data leakage during feature engineering,
- keeping train and test transformations consistent,
- balancing technical model performance with interpretability,
- communicating the results clearly for a non-technical audience.

These difficulties required careful preprocessing decisions and clear documentation.

---

## What I Learned

Through this project, I strengthened my skills in:

- data cleaning and preprocessing for real-world datasets,
- exploratory data analysis and interpretation,
- feature engineering for predictive modeling,
- building reusable analysis workflows,
- thinking about model transparency and fairness,
- communicating analytics work in a professional portfolio format.

I also learned how important it is to connect technical work to business value rather than focusing only on code or model metrics.

---

## Repository Contents

### 1. `data_preparation.R`
Reusable data preparation script for cleaning raw files, engineering features, aggregating external tables, and producing modeling-ready datasets.

### 2. `model_card.ipynb`
Model documentation notebook describing model purpose, inputs, evaluation considerations, limitations, and responsible-use discussion.

### 3. `EDA.qmd`
Exploratory data analysis notebook examining data distributions, missing values, class imbalance, and key relationships between borrower characteristics and default risk. The notebook also provides initial business insights that guide feature engineering and modeling decisions.

---

## Highlights of My Work

Some important elements of my work in this repository include:

- reusable preprocessing logic,
- applicant-level aggregation from multiple source tables,
- interpretable financial ratio features,
- documentation of modeling decisions,
- a focus on business communication alongside technical analysis.

---

## Tools and Technologies

- R
- Python
- Jupyter Notebook
- GitHub
- Data visualization libraries
- Machine learning and preprocessing workflows

---

## How to Use This Repository

Review the notebooks and script in the order listed below:

1. `eda_home_credit.ipynb`
2. `data_preparation.R`
3. `feature_engineering.ipynb` *(if included)*
4. `model_exploration.ipynb` *(if included)*
5. `model_card.ipynb`

These files show my workflow from data understanding through preparation and model documentation.

---

## Data Availability

The dataset used in this project (Home Credit Default Risk) is not included in this repository due to size limitations. To reproduce the analysis, please download the dataset from Kaggle and place the files in the working directory before running the notebooks.

Large raw data files and intermediate outputs are intentionally excluded from version control.

---

## Notes

- This repository contains portfolio materials and selected project files only.
- Large raw datasets and generated outputs are excluded from version control.
- The work shown here reflects my individual contributions created during the Home Credit project.

---
