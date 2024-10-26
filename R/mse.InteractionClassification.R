#' Mean Squared Errors for Interaction Classification
#'
#' Mean squared errors (MSE) for viral load differences
#' and CD4 count differences by comparing the actual values with the group means 
#' from the classification.
#'
#' @param object An object of class \code{InteractionClassification} containing 
#'               the classified data and clustering results.
#' @param ... Additional arguments passed to other methods (currently not used).
#'
#' @return A list containing the MSE for viral load differences
#'         (\code{vlogs_mse}) and CD4 count differences (\code{cds_mse}).
#' @export
#'
#' @examples
#' data(vl_3)
#' data(cd_3)
#' interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
#' class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
#' mse(class_obj)
#' 
#' @export
mse <- function(object, ...) {
  UseMethod("mse")
}

#' Mean Squared Errors method for the InteractionClassification class
#'
#' This method computes the mean squared errors (MSE) for viral load and CD4 differences
#' based on the classification results from an InteractionClassification object.
#'
#' @param object An object of class \code{InteractionClassification}.
#' @param ... Additional arguments (currently not used).
#' @exportS3Method mse InteractionClassification
mse.InteractionClassification <- function(object, ...) {
  group_means <- summary(object)
  
  merged_result <- object$data |> 
    dplyr::left_join(group_means, by = "classification") |> 
    dplyr::mutate(
      vlogs_diff_e = vl_diff - vlogs_diff_mean, 
      cds_diff_e = cd_diff - cds_diff_mean,
      vlogs_diff_se = (vlogs_diff_e)^2, 
      cds_diff_se = (cds_diff_e)^2
    )
  
  list(
    vlogs_mse = mean(merged_result$vlogs_diff_se, na.rm = TRUE),
    cds_mse = mean(merged_result$cds_diff_se, na.rm = TRUE)
  )
}

utils::globalVariables(c("cd_diff", "cds_diff_mean", "cds_diff_e", "vl_diff", "vlogs_diff_mean", "vlogs_diff_e"))