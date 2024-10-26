#' Estimate Payoff Parameters for HIV Phenotype Interactions
#'
#' This function estimates the payoff parameters for HIV phenotype interactions
#' based on the provided classification object and predictions from a viral
#' load model. It calculates the mean differences in viral loads and CD4 counts,
#' as well as the average payoffs for each classification.
#'
#' @param class_obj An object of class `InteractionClassification` containing
#' the data on viral load differences and CD4 counts.
#' 
#' @param predictions_df A data frame containing predictions of viral loads,
#' with a column named `predictions`. This data frame should align with the
#' classifications in `class_obj`.
#'
#' @return A payoffs object containing the estimated payoff parameters
#'
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
#' }
estimate_payoffs <- function(class_obj, predictions_df) {
  # Calculate the payoffs and group by classification
  results <- class_obj$data |>
    dplyr::left_join(summary(class_obj), by = "classification") |>
    dplyr::mutate(
      vlogs_diff_e = vl_diff - vlogs_diff_mean,
      cds_diff_e = cd_diff - cds_diff_mean,
      vlogs_diff_se = (vl_diff - vlogs_diff_mean)^2,
      cds_diff_se = (cd_diff - cds_diff_mean)^2
    ) |>
    dplyr::bind_cols(payoffs = predictions_df$predictions)
  
  # Create Payoffs S3 object
  payoffs_object <- structure(list(data = results), class = "payoffs")
  
  return(payoffs_object)
}

