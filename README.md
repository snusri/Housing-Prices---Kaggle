Housing Price Prediction - Kaggle Competition



This repo contains R scripts for the feature engineering and modeling of the kaggle competition dataset to predict Boston's house prices.

For full report visit [article] (http://rpubs.com/saeed_nusri/290664) on RPubs

The report presents the analysis and modeling done on the House Pricing dataset provided on Kaggle for advanced regression technique.

The goal of this report is to show how the data was transformed into numeric for modeling, while keeping all the relevant features for effective prediction. Since gradient boosting cannot extrapolate, ridge, lasso and elastic-net regularization will be used in ensemble. This will also account for a lot of the multicollinearity in the data. Prediction from all four models will be averaged to get final prediction.
