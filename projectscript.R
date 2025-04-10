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
distance_matrix <- dist(genre_data, method = "euclidean")
hc <- hclust(distance_matrix, method = "ward.D2")

# ------ Plot Dendrogram --------
plot(hc, main = "Hierarchical Clustering of Spotify Genres", xlab = "", sub = "", cex = 0.9)

# ------ Optional: Assign Cluster Labels --------
k <- 5
clusters <- cutree(hc, k = k)
genre_cluster_map <- data.frame(track_genre = names(clusters), cluster = clusters)

# ------ Genre Recommendation Function --------
recommend_similar_genres <- function(target_genre, cluster_map) {
  if (!(target_genre %in% cluster_map$track_genre)) {
    stop("Genre not found.")
  }
  cluster_id <- cluster_map$cluster[cluster_map$track_genre == target_genre]
  similar <- cluster_map$track_genre[cluster_map$cluster == cluster_id]
  setdiff(similar, target_genre)
}

# ------ Prepare Data for FAMD (Factor Analysis of Mixed Data) --------
famd_input <- spotify_data %>%
  select(all_of(audio_features), key, mode, time_signature, track_genre) %>%
  mutate(
    key = as.factor(key),
    mode = as.factor(mode),
    time_signature = as.factor(time_signature),
    track_genre = as.factor(track_genre)
  )


# ------ Run Factor Analysis Mixed Data (FAMD) for Dimension Reduction --------

famd_result <- FAMD(famd_input %>% select(-track_genre), ncp = 5, graph = FALSE)

# ------ Visualize FAMD Results (Tracks Colored by Genre) --------
fviz_famd_ind(famd_result,
              label = "none",
              habillage = famd_input$track_genre,
              addEllipses = TRUE,
              ellipse.level = 0.95,
              repel = TRUE) +
  ggtitle("FAMD: Spotify Tracks Colored by Genre")
