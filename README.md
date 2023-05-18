# Reproducibility-CondEmpiricalCopulaProcesses
Replication for the article 'Conditional empirical copula processes and generalized measures of association'

# Structure of this repository

- Files that only contains functions:
  * `lib_computationReturnsInnov.R`: computations of returns and innovations
  * `lib_GARCH.R`: GARCH filtering (necessary for computing the innovations)
  * `lib_CKT.R`: computations of conditional Kendall's tau and its confidence interval via the nonparametric bootstrap

- Script files:
  * `01_loadingData.R`: loading the data and preprocessing
  * `02_estimation.R`: computing CKT, making plots
