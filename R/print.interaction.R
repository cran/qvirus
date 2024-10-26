#' Print Summary of CD4 and Viral Load Differences
#'
#' This method prints a summary of the CD4 and viral load differences contained 
#' in the interaction object. It provides both raw and standardized values for 
#' CD4 differences, as well as raw, log10-transformed, and log10-standardized 
#' values for viral load differences.
#'
#' @param x An object of class \code{interaction} containing CD4 and viral load 
#' differences.
#' @param ... Additional arguments passed to the summary function.
#'
#' @return This function does not return a value; it prints the summary 
#' statistics directly to the console.
#' @export
#'
#' @examples
#' data("vl_3")
#' data("cd_3")
#' interaction_obj <- create_interactions(cd_3, vl_3)
#' print(interaction_obj)
print.interaction <- function(x, ...) {
  # Use the summary method to generate the statistics
  summary_data <- summary(x, ...)
  
  message("Summary of CD4 and Viral Load Differences\n")
  
  message("\nCD4 Differences (raw):")
  print(summary_data$cd_diff_raw)
  
  message("\nCD4 Differences (standardized):")
  print(summary_data$cd_diff_standardized)
  
  message("\nViral Load Differences (raw):")
  print(summary_data$vl_diff_raw)
  
  message("\nViral Load Differences (log-transformed):")
  print(summary_data$vl_diff_log_transformed)
  
  message("\nViral Load Differences (log-standardized):")
  print(summary_data$vl_diff_log_standardized)
}