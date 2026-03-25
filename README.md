## Weekly Work Hours Prediction

**Dataset:** Work Hour Prediction Dataset (Kaggle, 18,881 observations, 15 variables)  
**Tools:** R (ggplot2, dplyr, FactoMineR, randomForest, xgboost, tidyverse)

### Objective
Predict whether an individual works more than 40 hours per week based on 
demographic and occupational factors. Secondary objective: identify overworked individuals.

### Methods
- Exploratory Data Analysis (EDA)
- Feature Engineering: recategorized Work Class, Education, Marital Status, Race, Native Country
- Spearman Rank Correlation and Cramér's V heatmap for categorical associations
- Generalized VIF (GVIF) for multicollinearity detection
- Factor Analysis for Mixed Data (FAMD)
- Outlier Detection: Isolation Forest + Robust Mahalanobis Distance
- SMOTENC to handle class imbalance with mixed data types
- Models: Random Forest, XGBoost, Logistic Regression

### Key Results
- Random Forest achieved the best overworked detection: F1-score 0.72 (test set)
- Strong multicollinearity identified between Relationship and Marital Status (GVIF > 5)
- Top predictors: sex, relationship status, education level, work class
