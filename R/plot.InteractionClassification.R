#' Plot InteractionClassification Clusters
#'
#' This function visualizes the clusters formed by the \code{InteractionClassification} object. 
#' It displays the viral load differences (\code{vl_diff}) and CD4 count differences (\code{cd_diff}) 
#' as points, with each point color-coded by its cluster. Cluster centers are shown as red stars.
#'
#' Additionally, it can evaluate the optimal number of clusters using the elbow method by plotting 
#' the within-cluster sum of squares (WSS) using \code{fviz_nbclust}.
#'
#' @param x An object of class \code{InteractionClassification} created using the 
#'   \code{InteractionClassification()} function. It contains the clustering results and cluster centers.
#' @param plot_clusters Logical. If TRUE, plots the cluster visualization. Default is TRUE.
#' @param evaluate_clusters Logical. If TRUE, plots the WSS to evaluate the optimal number of clusters.
#'   Default is FALSE.
#' @param max_clusters Integer. The maximum number of clusters to evaluate if \code{evaluate_clusters} 
#'   is set to TRUE. Default is 10.
#' @param n_clusters Integer. The number of clusters to suggest when plotting the elbow method. 
#'   It determines where the vertical line (xintercept) is drawn in the plot. Default is 4.
#' @param ... Additional arguments (currently unused).
#'
#' @return A ggplot object that visualizes the clusters and their centers, or the WSS plot to evaluate 
#'   the optimal number of clusters.
#' @export
#'
#' @examples
#' data(vl_3)
#' data(cd_3)
#' interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
#' class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
#' plot(class_obj)
plot.InteractionClassification <- function(x, plot_clusters = TRUE, evaluate_clusters = FALSE, max_clusters = 10, n_clusters = 4, ...) {
  
  # Extract the classification result and cluster centers
  classification_result <- x$data
  centers <- x$centers
  
  # If evaluate_clusters is TRUE, plot the WSS for different cluster numbers
  if (evaluate_clusters) {
    return(
      factoextra::fviz_nbclust(
        classification_result[, c("vl_diff", "cd_diff")], 
        stats::kmeans, 
        method = "wss", 
        k.max = max_clusters
      ) +
        ggplot2::geom_vline(xintercept = n_clusters, linetype = 2) +  # Suggested number of clusters
        ggplot2::ggtitle("Elbow Method for Evaluating Number of Clusters")
    )
  }
  
  # If plot_clusters is TRUE, plot the clusters with their centers
  if (plot_clusters) {
    return(
      factoextra::fviz_cluster(
        list(data = classification_result[, c("vl_diff", "cd_diff")], cluster = classification_result$classification),
        geom = "point",
        ggtheme = ggplot2::theme_minimal(),
        show.clust.cent = TRUE,  # Display cluster centers
        main = "Interaction Classification Clusters") +
        
        ggplot2::geom_point(
          data = as.data.frame(centers), 
          ggplot2::aes(x = vl_diff, y = cd_diff), 
          color = "red", size = 3, shape = 8
        )
    )
  }
}

# Global variables for tidy evaluation in ggplot
utils::globalVariables(c("cd_diff", "vl_diff"))