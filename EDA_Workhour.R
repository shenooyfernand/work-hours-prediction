# ===============================
# 1. Load Libraries
# ===============================
library(tidyverse)
library(corrplot)
library(reshape2)
library(vcd)
library(GGally)

# ===============================
# 2. Load Dataset
# ===============================
dataset <- read.csv("content/work_hours.csv", stringsAsFactors = FALSE)

# ===============================
# 3. Basic Exploration
# ===============================
str(dataset)
summary(dataset)
head(dataset)

# Missing values
colSums(is.na(dataset))

# Duplicates
sum(duplicated(dataset))

# ===============================
# 4. Data Cleaning & Recoding
# ===============================

# Remove unnecessary categories (example from your code)
dataset <- dataset %>% filter(workclass != "Never-worked")

# Workclass grouping
dataset$workclass <- recode(dataset$workclass,
                            "Federal-gov" = "Government",
                            "Local-gov"   = "Government",
                            "State-gov"   = "Government",
                            "Self-emp-inc"= "Self-employed",
                            "Self-emp-not-inc" = "Self-employed"
)

# Education grouping
dataset$education <- recode(dataset$education,
                            "Bachelors" = "degree-holder",
                            "Masters"   = "degree-holder",
                            "Doctorate" = "degree-holder",
                            "Assoc-acdm"= "assoc-degree-holder",
                            "Assoc-voc" = "assoc-degree-holder"
)

# Marital status grouping
dataset$marital.status <- recode(dataset$marital.status,
                                 "Married-civ-spouse" = "married",
                                 "Married-spouse-absent" = "married",
                                 "Never-married" = "unmarried",
                                 "Divorced" = "separated",
                                 "Separated" = "separated",
                                 "Widowed" = "separated"
)

# Race grouping
dataset$race <- recode(dataset$race,
                       "Asian-Pac-Islander" = "Other",
                       "Amer-Indian-Eskimo" = "Other"
)

# Native country grouping
dataset$native.country <- ifelse(dataset$native.country == "United-States", "USA", "Non-USA")

# Convert categorical variables to factors
dataset <- dataset %>% mutate_if(is.character, as.factor)

# ===============================
# 5. Univariate Analysis
# ===============================

# Histogram of target
ggplot(dataset, aes(x = hours.per.week)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Distribution of Working Hours")

# Age distribution
ggplot(dataset, aes(x = age)) +
  geom_histogram(fill = "orange", bins = 30) +
  labs(title = "Distribution of Age")

# ===============================
# 6. Bivariate Analysis
# ===============================

# Age vs Work Hours
plot(dataset$age, dataset$hours.per.week,
     main = "Work Hours vs Age",
     xlab = "Age", ylab = "Hours per Week")
lines(lowess(dataset$age, dataset$hours.per.week), col="red")

# Boxplots (key variables)
boxplot(hours.per.week ~ workclass, data = dataset, main="Work Hours by Workclass")
boxplot(hours.per.week ~ education, data = dataset, main="Work Hours by Education")
boxplot(hours.per.week ~ marital.status, data = dataset, main="Work Hours by Marital Status")
boxplot(hours.per.week ~ race, data = dataset, main="Work Hours by Race")
boxplot(hours.per.week ~ sex, data = dataset, main="Work Hours by Gender")
boxplot(hours.per.week ~ native.country, data = dataset, main="Work Hours by Country")
boxplot(hours.per.week ~ income, data = dataset, main="Work Hours by Income")

# Income vs Gender
tbl <- table(dataset$income, dataset$sex)
barplot(tbl, beside = TRUE, col = c("lightblue","pink"),
        main = "Gender vs Income")

# ===============================
# 7. Correlation Analysis (Numeric)
# ===============================
num_data <- dataset %>% select_if(is.numeric)

cor_matrix <- cor(num_data)

corrplot(cor_matrix,
         method = "color",
         type = "lower",
         addCoef.col = "black")

# ===============================
# 8. Scatterplot Matrix
# ===============================
ggpairs(num_data)

# ===============================
# 9. Categorical Association (Cramer's V)
# ===============================

cat_vars <- dataset %>% select_if(is.factor)

cramers_matrix <- matrix(NA, ncol = ncol(cat_vars), nrow = ncol(cat_vars))
colnames(cramers_matrix) <- colnames(cat_vars)
rownames(cramers_matrix) <- colnames(cat_vars)

for (i in 1:ncol(cat_vars)) {
  for (j in 1:ncol(cat_vars)) {
    if (i == j) {
      cramers_matrix[i,j] <- 1
    } else {
      tbl <- table(cat_vars[[i]], cat_vars[[j]])
      cramers_matrix[i,j] <- assocstats(tbl)$cramer
    }
  }
}

# Heatmap
corrplot(cramers_matrix, method="color", type="lower")

# ===============================
# 10. Spearman Correlation
# ===============================
cor_spear <- cor(num_data, method = "spearman")
corrplot(cor_spear, method="color", type="lower")

# ===============================
# 11. Multicollinearity Check
# ===============================
library(car)

model <- lm(hours.per.week ~ ., data = dataset)
vif(model)

