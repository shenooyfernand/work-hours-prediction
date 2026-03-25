# ============================================
# C. CLUSTER ANALYSIS
# ============================================

library(FactoMineR)
library(factoextra)

# Use original dataset (before dummy encoding)
cluster_data <- dataset

# Convert characters to factors
char_cols <- sapply(cluster_data, is.character)
cluster_data[, char_cols] <- lapply(cluster_data[, char_cols], as.factor)

# ----------------------------
# FAMD
# ----------------------------

famd_model <- FAMD(cluster_data, graph = FALSE)

# Scree plot
fviz_screeplot(famd_model)

# Variable contribution
fviz_famd_var(famd_model, repel = TRUE)

# ----------------------------
# Extract coordinates
# ----------------------------

coords <- get_famd_ind(famd_model)$coord

# ----------------------------
# Optimal clusters
# ----------------------------

fviz_nbclust(coords, kmeans, method = "silhouette")

# ----------------------------
# K-means clustering
# ----------------------------

set.seed(123)
kmeans_model <- kmeans(coords, centers = 5, nstart = 25)

cluster_data$cluster <- as.factor(kmeans_model$cluster)

# Visualization
fviz_cluster(kmeans_model, data = coords)

# ----------------------------
# Cluster interpretation
# ----------------------------

cluster_summary <- cluster_data %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean))

print(cluster_summary)