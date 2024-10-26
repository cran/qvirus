#' Summarize Interaction Classification Results
#'
#' This function calculates and summarizes the mean viral load differences
#' and CD4 count differences for each classification in the Interaction
#' Classification object.
#'
#' @param object An object of class \code{InteractionClassification} containing 
#'          the classified data and clustering results.
#' @param ... Additional arguments passed to other methods (currently not used).
#'
#' @return A data frame summarizing the mean viral load differences and CD4
#'         count differences for each classification, along with the count 
#'         of observations in each classification.
#' @export
#'
#' @examples
#' data(vl_3)
#' data(cd_3)
#' interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
#' class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
#' summary(class_obj)
summary.InteractionClassification <- function(object, ...) {
  group_means <- object$data |> 
    dplyr::group_by(classification) |> 
    dplyr::summarise(
      vlogs_diff_mean = mean(vl_diff), 
      cds_diff_mean = mean(cd_diff), 
      n = dplyr::n(),
      .groups = 'drop'
    )
  
  return(group_means)
}

utils::globalVariables(c("classification", "cd_diff", "vl_diff"))