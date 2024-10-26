#' Summarize Payoffs
#'
#' This function summarizes the payoffs object by classification.
#'
#' @param object A payoffs object.
#' @param ... Additional arguments (not used).
#'
#' @return A tibble summarizing the estimated payoffs.
#' @export
#' 
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(earth)
#' library(baguette)
#' library(rules)
#' library(kknn)
#' library(viralmodels)
#' # Load required data
#' data(vl_3)
#' data(cd_3)
#' 
#' # Create interaction object and classification
#' interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
#' class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
#' 
#' # Prepare predictions
#' traindata <- interaction_obj[c(2, 5)] |> as_tibble()
#' target <- "vlogs_diff"
#' predictions_df <- viralpreds(target, 2, 1, 2, 123, traindata)
#' 
#' # Estimate payoffs
#' payoffs_results <- estimate_payoffs(class_obj, predictions_df)
#' summary(payoffs_results)
#' }
summary.payoffs <- function(object, ...) {
  summary_list <- object$data |>
    dplyr::group_by(classification) |>
    dplyr::summarise(
      vlogs_diff_mean = mean(vl_diff, na.rm = TRUE), # Update to vlogs_diff_e
      cds_diff_mean = mean(cd_diff, na.rm = TRUE),     # Update to cds_diff_e
      payoff_mean = mean(payoffs, na.rm = TRUE),
      n = dplyr::n()
    )
  
  # Return the structured list of summaries
  return(summary_list)
}

utils::globalVariables("payoffs")