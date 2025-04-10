# ------ Load Libraries --------
library(FactoMineR)
library(factoextra)
library(dplyr)
library(cluster)
library(ggplot2)

# ------ Load and Inspect Dataset --------
spotify_data <- read.csv("spotify_tracks.csv")

if ("X" %in% colnames(spotify_data)) {
  cat("something else happened")
  spotify_data <- spotify_data %>% select(-X)
}

head(spotify_data)

# ------ Prepare Full Dataset for PCA --------
# Select only numerical features for PCA (Temp Solution)
numerical_features <- c("popularity", "duration_ms", "danceability", "energy", "loudness",
                        "speechiness", "acousticness", "instrumentalness", "liveness", 
                        "valence", "tempo")

pca_data <- spotify_data[, numerical_features]
na_rows <- complete.cases(pca_data)
pca_data <- pca_data[na_rows, ]
genre_labels <- spotify_data$track_genre[na_rows]  # Ensure alignment

# ------ Run PCA --------
res_pca <- PCA(pca_data, scale.unit = TRUE, ncp = 2, graph = FALSE)

pca_coords <- as.data.frame(res_pca$ind$coord)
pca_coords$genre <- genre_labels

# ------ Print PCA Loadings and Explained Variance --------
cat("PCA Loadings (Component Contributions):\n")
print(round(res_pca$var$coord, 3))

cat("\nExplained Variance (%):\n")
print(round(res_pca$eig[, 2], 2))  # Second column is % of variance

# ------ Plot PCA with Colored Genres --------

unique_genres <- unique(genre_labels)
n_colors <- length(unique_genres)
color_palette <- hcl.colors(n_colors, palette = "Dynamic")

ggplot(pca_coords, aes(x = Dim.1, y = Dim.2, color = genre)) +
  geom_point(size = 0.5, shape = 16) +  # shape 16 = filled circle
  scale_color_manual(values = color_palette) +
  ggtitle("PCA of Spotify Tracks Colored by Genre") +
  theme_minimal() +
  theme(legend.position = "none")  +
  xlim(-10, 5) +
  ylim(-5, 5) +
  labs(x = "Principal Component 1 (26.14% Variance Captured)", y = "Principal Component 2 (13.86% Captured)")

# ------ Mixed Data Factor Analysis --------

# ------ Add genre labels BEFORE dropping NAs --------
spotify_data$explicit <- as.factor(spotify_data$explicit)
spotify_data$key <- as.factor(spotify_data$key)
spotify_data$mode <- as.factor(spotify_data$mode)
spotify_data$time_signature <- as.factor(spotify_data$time_signature)

famd_features <- c("popularity", "duration_ms", "danceability", "energy", "loudness",
                   "speechiness", "acousticness", "instrumentalness", "liveness",
                   "valence", "tempo", 
                   "explicit", "mode", "key", "time_signature", "track_genre")  # Include genre here

famd_data <- spotify_data[, famd_features]
famd_data <- na.omit(famd_data)

# Save genre and drop from data matrix
genre_labels <- famd_data$track_genre
famd_data <- famd_data %>% select(-track_genre)

# ------ Run FAMD --------
res_famd <- FAMD(famd_data, ncp = 2, graph = FALSE)

# ------ Print Loadings for Variables --------
cat("\nFAMD Loadings (Quantitative Variables):\n")
print(round(res_famd$var$coord, 3))

cat("\nFAMD Loadings (Categorical Variable Levels):\n")
print(round(res_famd$quali.var$coord, 3))

# ------ Print Explained Variance --------
cat("\nExplained Variance by Dimension (%):\n")
print(round(res_famd$eig[, 2], 2))

# ------ Extract Coordinates and Add Genre --------
famd_coords <- as.data.frame(res_famd$ind$coord)
famd_coords$genre <- as.factor(genre_labels)  # Safe to convert here

# ------ Define color palette --------
unique_genres <- levels(famd_coords$genre)
color_palette <- hcl.colors(length(unique_genres), palette = "Dynamic")
names(color_palette) <- unique_genres

# ------ Plot FAMD Result --------
ggplot(famd_coords, aes(x = Dim.1, y = Dim.2, color = genre)) +
  geom_point(size = 0.5, shape = 16) +
  scale_color_manual(values = color_palette) +
  ggtitle("FAMD of Spotify Tracks (Mixed Data)") +
  labs(x = "FAMD Dimension 1", y = "FAMD Dimension 2") +
  theme_minimal() +
  theme(legend.position = "none") +
  xlim(-5, 5) +
  ylim(-5, 5)

# ------ Reduce Visual Cluttering through Heirearchal Clustering --------

# There's too many genres, lets add clarity and better viz by grouping similar genres together

# ------ Define Audio Feature Columns --------
audio_features <- c("popularity", "duration_ms", "danceability", "energy", "key",
                    "loudness", "mode", "speechiness", "acousticness", "instrumentalness",
                    "liveness", "valence", "tempo", "time_signature")

# ------ Compute Genre-Wise Feature Averages --------

genre_means <- spotify_data %>%
  group_by(track_genre) %>%
  summarise(across(all_of(audio_features), \(x) mean(x, na.rm = TRUE)))

# ------ Prepare Data for Clustering --------
genre_data <- as.data.frame(genre_means %>% select(-track_genre))
row.names(genre_data) <- genre_means$track_genre  # set genre names as row names

# ------ Hierarchical Clustering --------
genre_data_scaled <- scale(genre_data)
distance_matrix <- dist(genre_data_scaled, method = "euclidean")
hc <- hclust(distance_matrix, method = "ward.D2")

# ------ Plot Dendrogram --------
plot(hc, main = "Hierarchical Clustering of Spotify Genres", xlab = "", sub = "", cex = 0.9)

# ------ Optional: Assign Cluster Labels --------
k <- 25
clusters <- cutree(hc, k = k)
genre_cluster_map <- data.frame(track_genre = names(clusters), cluster = clusters)

# ------ Print Genres in Each Cluster --------
cat("Genres in Each Cluster:\n")
genre_groups <- split(genre_cluster_map$track_genre, genre_cluster_map$cluster)

for (i in 1:k) {
  cat(paste0("\nCluster ", i, ":\n"))
  print(sort(genre_groups[[i]]))
}

