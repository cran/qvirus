#' Summary Method for Interaction Class Objects
#'
#' @description
#' This function provides a summary of various statistics for an interaction 
#' object, including raw and standardized differences for CD4 lymphocyte counts 
#' and viral loads. It returns a structured list of summary statistics, useful 
#' for understanding the differences in immune response and viral suppression 
#' between HIV phenotypes.
#'
#' @param object An object of class \code{interaction} that contains CD4 
#' lymphocyte and viral load differences (raw and log-transformed).
#' @param ... Additional arguments (currently not used).
#'
#' @return
#' A list with the following components:
#' \describe{
#'   \item{\code{cd_diff_raw}}{Summary statistics for the raw differences in CD4 lymphocyte counts.}
#'   \item{\code{cd_diff_standardized}}{Summary statistics for the standardized CD4 lymphocyte differences.}
#'   \item{\code{vl_diff_raw}}{Summary statistics for the raw differences in viral loads.}
#'   \item{\code{vl_diff_log_transformed}}{Summary statistics for the log-transformed viral load differences.}
#'   \item{\code{vl_diff_log_standardized}}{Summary statistics for the standardized log-transformed viral load differences.}
#' }
#'
#' @export
#'
#' @examples
#' data(cd_3)
#' data(vl_3)
#' interaction_obj <- create_interactions(cd_3, vl_3)
#' summary(interaction_obj)
summary.interaction <- function(object, ...) {
  # Create a list to hold the summaries
  summary_list <- list(
    cd_diff_raw = summary(unlist(object$cd_diff)),
    cd_diff_standardized = summary(unlist(object$cds_diff)),
    vl_diff_raw = summary(unlist(object$vl_diff)),
    vl_diff_log_transformed = summary(unlist(object$vlog_diff)),
    vl_diff_log_standardized = summary(unlist(object$vlogs_diff))
  )
  
  # Return the structured list of summaries
  return(summary_list)
}