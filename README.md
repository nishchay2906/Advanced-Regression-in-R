# Ames house price regression in R

An R coursework project that engineers housing features and compares lasso and ridge regression on log sale prices. The model exports predicted sale prices with their property IDs.

## Run

1. Obtain `train.csv` and `test.csv` from the [House Prices competition](https://www.kaggle.com/competitions/house-prices-advanced-regression-techniques/data). The script expects the original competition schema and column order.
2. Install the R packages with `install.packages(c("caTools", "glmnet"))`.
3. Run from the repository root.

```sh
Rscript "House Price Model (1).R" train.csv test.csv
```

The script writes `lasso_prediction.csv` and `ridge_testing.csv` to the working directory. [The archived ridge predictions](Predicted%20House%20Prices%20ridge.csv) are included for reference. The original workflow imputes missing values separately in the training and test sets, which limits how it can be applied to individual new records.
