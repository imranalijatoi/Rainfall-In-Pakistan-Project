# Load required libraries
library(igraph)
library(stringr)
library(readr)
library(dplyr)
library(ggplot2)

# Explicitly define the path to your dataset
file_path <- "C:/Users/DELL/Downloads/Rainfall in Pakistan/rainfall.csv"

# Step 1: Load the dataset with error handling
cat("Loading dataset...\n")
rainfall_data <- tryCatch({
  read_csv(file_path)
}, error = function(e) {
  stop("Error loading the dataset. Check the file path or format.")
})

# Step 2: Debugging - Check dataset structure
cat("Dataset preview:\n")
print(head(rainfall_data))
cat("\nDataset structure:\n")
print(str(rainfall_data))

# Step 3: Clean column names (remove extra spaces)
colnames(rainfall_data) <- str_trim(colnames(rainfall_data))

# Step 4: Validate required columns
required_columns <- c("Year", "Month", "Rainfall - (MM)")
missing_columns <- setdiff(required_columns, colnames(rainfall_data))
if (length(missing_columns) > 0) {
  stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
}

# Step 5: Extract unique years and months
years <- unique(rainfall_data$Year)
months <- unique(rainfall_data$Month)

# Debugging: Check extracted years and months
cat("\nUnique years:\n")
print(years)
cat("\nUnique months:\n")
print(months)

# Step 6: Prepare edges and nodes
edges <- rainfall_data %>%
  select(Year, Month, `Rainfall - (MM)`) %>%
  rename(weight = `Rainfall - (MM)`) %>%
  mutate(Year = as.character(Year), Month = as.character(Month))

nodes <- data.frame(
  id = c(as.character(years), as.character(months)),
  type = c(rep(TRUE, length(years)), rep(FALSE, length(months))) # TRUE for years, FALSE for months
)

# Debugging: Check edges and nodes
cat("\nEdges preview:\n")
print(head(edges))
cat("\nNodes preview:\n")
print(head(nodes))

# Step 7: Create the bipartite graph
cat("\nCreating the bipartite graph...\n")
g <- tryCatch({
  igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
}, error = function(e) {
  stop("Error creating the graph. Check the input data.")
})

# Validate the graph
if (!is.igraph(g)) stop("The graph object was not created correctly.") else cat("\nGraph successfully created.\n")

# Step 8: Check bipartite structure
if (!is_bipartite(g)) stop("The graph is not bipartite. Please check the input data.") else cat("\nBipartite graph structure confirmed.\n")

# Step 9: Compute Structural Properties
cat("\nComputing structural properties...\n")
degree_centrality <- igraph::degree(g, mode = "all")
density <- igraph::edge_density(g)
betweenness_centrality <- igraph::betweenness(g)
closeness_centrality <- igraph::closeness(g)
global_clustering <- igraph::transitivity(g, type = "global")

# Debugging: Print structural properties
cat("\n=== Structural Properties ===\n")
cat("Degree Centrality:\n")
print(degree_centrality)
cat("Density:", density, "\n")
cat("Betweenness Centrality:\n")
print(betweenness_centrality)
cat("Closeness Centrality:\n")
print(closeness_centrality)
cat("Global Clustering Coefficient:\n")
print(global_clustering)

# Step 10: Bipartite Projections
cat("\nCreating bipartite projections...\n")
bipartite_projections <- tryCatch({
  igraph::bipartite_projection(g)
}, error = function(e) {
  stop("Error creating bipartite projections.")
})

binary_projection <- bipartite_projections[[2]] # Select one projection

# Debugging: Validate projections
if (!is.igraph(binary_projection)) stop("Binary projection failed.") else cat("\nBinary projection successfully created.\n")

# Step 11: Calculate Additional Metrics
assortativity_score <- igraph::assortativity(g, types1 = nodes$type)
eigenvector_centrality <- igraph::eigen_centrality(g)$vector

# Step 12: Results Summary
results <- list(
  StructuralProperties = list(
    DegreeCentrality = degree_centrality,
    Density = density,
    BetweennessCentrality = betweenness_centrality,
    ClosenessCentrality = closeness_centrality,
    GlobalClusteringCoefficient = global_clustering
  ),
  Projections = list(
    BinaryProjection = binary_projection
  ),
  AdditionalMetrics = list(
    Assortativity = assortativity_score,
    EigenvectorCentrality = eigenvector_centrality
  )
)

# Print results to console
cat("=== Structural Properties ===\n")
print(results$StructuralProperties)

cat("\n=== Additional Metrics ===\n")
cat("Assortativity: ", assortativity_score, "\n")
cat("Eigenvector Centrality:\n")
print(eigenvector_centrality)

# Step 13: Save Results for Reporting
saveRDS(results, "network_analysis_results.rds")
cat("\nResults saved successfully.\n")

# Step 14: Saving Plots
cat("\nSaving plots...\n")
# 1. Degree Distribution for Years
degree_years <- degree_centrality[1:length(years)]
degree_months <- degree_centrality[(length(years) + 1):length(degree_centrality)]

# Plot for Years
degree_years_plot <- ggplot(data = data.frame(Year = years, Degree = degree_years), aes(x = Year, y = Degree)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Degree Distribution (Years)", x = "Year", y = "Degree") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
print(degree_years_plot)

# Save Degree Distribution for Years as PNG
ggsave("degree_distribution_years.png", plot = degree_years_plot)

# Plot for Months
degree_months_plot <- ggplot(data = data.frame(Month = months, Degree = degree_months), aes(x = Month, y = Degree)) +
  geom_bar(stat = "identity", fill = "orange") +
  theme_minimal() +
  labs(title = "Degree Distribution (Months)", x = "Month", y = "Degree") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
print(degree_months_plot)

# Save Degree Distribution for Months as PNG
ggsave("degree_distribution_months.png", plot = degree_months_plot)

# 2. Betweenness Centrality
betweenness_df <- data.frame(Node = names(betweenness_centrality), Betweenness = betweenness_centrality)
betweenness_plot <- ggplot(betweenness_df, aes(x = reorder(Node, -Betweenness), y = Betweenness)) +
  geom_bar(stat = "identity", fill = "green") +
  theme_minimal() +
  labs(title = "Betweenness Centrality", x = "Node", y = "Betweenness") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability
print(betweenness_plot)

# Save Betweenness Centrality as PNG
ggsave("betweenness_centrality.png", plot = betweenness_plot)

# 3. Closeness Centrality
closeness_df <- data.frame(Node = names(closeness_centrality), Closeness = closeness_centrality)
closeness_plot <- ggplot(closeness_df, aes(x = reorder(Node, -Closeness), y = Closeness)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme_minimal() +
  labs(title = "Closeness Centrality", x = "Node", y = "Closeness") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability
print(closeness_plot)

# Save Closeness Centrality as PNG
ggsave("closeness_centrality.png", plot = closeness_plot)

# 4. Global Clustering Coefficient
barplot(global_clustering, main = "Global Clustering Coefficient", col = "lightblue", ylab = "Value")
dev.flush()

# Save Global Clustering Coefficient plot as PNG
png("global_clustering_coefficient.png")
barplot(global_clustering, main = "Global Clustering Coefficient", col = "lightblue", ylab = "Value")
dev.off()

# 5. Graph Density
# Plot Density
density_plot <- ggplot(data.frame(Density = density), aes(x = "", y = Density)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Graph Density", y = "Density") +
  theme(axis.text.x = element_blank())  # Remove x-axis labels
print(density_plot)

# Save Graph Density Plot as PNG
ggsave("graph_density.png", plot = density_plot)

# 6. Bipartite Network Visualization
plot(g, vertex.label = NA, vertex.size = 5, main = "Rainfall Bipartite Network", layout = layout.bipartite)

# Save Bipartite Network as PNG
png("bipartite_network.png")
plot(g, vertex.label = NA, vertex.size = 5, main = "Rainfall Bipartite Network", layout = layout.bipartite)
dev.off()

# 7. Binary Projection Network
plot(binary_projection, vertex.label = NA, vertex.size = 5, main = "Binary Projection Network", edge.color = "blue")

# Save Binary Projection Network as PNG
png("binary_projection.png")
plot(binary_projection, vertex.label = NA, vertex.size = 5, main = "Binary Projection Network", edge.color = "blue")
dev.off()

# 8. Sum Projection Network
plot(sum_projection, vertex.label = NA, edge.width = E(sum_projection)$weight, main = "Sum Projection Network", edge.color = "red")

# Save Sum Projection Network as PNG
png("sum_projection.png")
plot(sum_projection, vertex.label = NA, edge.width = E(sum_projection)$weight, main = "Sum Projection Network", edge.color = "red")
dev.off()

# 9. Weighted Newman Projection Network
plot(weighted_projection, vertex.label = NA, edge.width = E(weighted_projection)$weight, main = "Weighted Newman Projection Network", edge.color = "green")

# Save Weighted Newman Projection Network as PNG
png("weighted_newman_projection.png")
plot(weighted_projection, vertex.label = NA, edge.width = E(weighted_projection)$weight, main = "Weighted Newman Projection Network", edge.color = "green")
dev.off()

# 10. Assortativity Plot
assortativity_plot <- ggplot(data.frame(Assortativity = assortativity_score), aes(x = "Assortativity", y = Assortativity)) +
  geom_bar(stat = "identity", fill = "cyan") +
  theme_minimal() +
  labs(title = "Assortativity", x = "", y = "Assortativity Value")
print(assortativity_plot)

# Save Assortativity Plot as PNG
ggsave("assortativity.png", plot = assortativity_plot)

# 11. Eigenvector Centrality
eigenvector_df <- data.frame(Node = names(eigenvector_centrality), EigenvectorCentrality = eigenvector_centrality)
eigenvector_plot <- ggplot(eigenvector_df, aes(x = reorder(Node, -EigenvectorCentrality), y = EigenvectorCentrality)) +
  geom_bar(stat = "identity", fill = "pink") +
  theme_minimal() +
  labs(title = "Eigenvector Centrality", x = "Node", y = "Eigenvector Centrality") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability
print(eigenvector_plot)

# Save Eigenvector Centrality as PNG
ggsave("eigenvector_centrality.png", plot = eigenvector_plot)

# Step 12: Close PDF device
dev.off()  # All plots are now saved in "network_analysis_plots.pdf"
