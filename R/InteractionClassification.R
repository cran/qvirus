#' Interaction Classification for Viral Load and CD4 Differences
#'
#' This function performs k-means clustering on viral load and CD4 count 
#' differences to classify interactions into distinct groups. It returns 
#' an S3 object containing the clustering results and means for each 
#' cluster.
#'
#' @param vl_diff A numeric vector of viral load differences.
#' @param cd_diff A numeric vector of CD4 count differences.
#' @param k An integer specifying the number of clusters (default is 4).
#' @param ns An integer specifying the number of random starts for the 
#'   k-means algorithm (default is 100).
#' @param seed An integer seed for reproducibility of the clustering 
#'   results (default is 123).
#'
#' @return An S3 object of class \code{InteractionClassification}, containing:
#'   \item{data}{A data frame with the original differences and their 
#'   corresponding cluster classifications.}
#'   \item{kmeans_result}{The result of the k-means clustering, including 
#'   cluster centers and within-cluster sum of squares.}
#'   \item{centers}{A matrix of the cluster centers.}
#'   \item{k}{The number of clusters used in the clustering.}
#'
#' @export
#'
#' @examples
#' data(vl_3)
#' data(cd_3)
#' interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
#' class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
InteractionClassification <- function(vl_diff, cd_diff, k = 4, ns = 100, seed = 123) {
  set.seed(seed)
  
  # Create a data frame with the input viral load and CD4 differences
  diffs <- data.frame(vl_diff = vl_diff, cd_diff = cd_diff)
  
  # Perform k-means clustering
  km_res <- stats::kmeans(diffs, centers = k, nstart = 100)
  
  # Create a classification result data frame
  classification_result <- diffs
  classification_result$classification <- km_res$cluster
  
  # Create S3 object
  structure(
    list(
      data = classification_result,
      kmeans_result = km_res,
      centers = km_res$centers,
      k = k
    ),
    class = "InteractionClassification"
  )
}