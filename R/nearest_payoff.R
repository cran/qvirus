#' Find Nearest Payoff
#'
#' This function computes the nearest simulated payoff from a given list of payoffs 
#' based on a viral load difference (vl_diff). It returns both the nearest payoff value 
#' and its corresponding payoff name.
#'
#' @param vl_diff Numeric value representing the viral load difference for which the 
#' nearest payoff will be found.
#' @param payoffs_list A named list of payoff values, where the names correspond to 
#' specific payoffs and the values are the associated payoff values.
#'
#' @return A list with two elements: `value` (the nearest payoff value) and `name` 
#' (the name of the nearest payoff).
#' @export
#'
#' @examples
#' # Load data
#' library(dplyr)
#' library(qsimulatR)
#' data(vl_3)
#' data(cd_3)
#' 
#' # Create interaction object and classification
#' interaction_obj <- create_interactions(cd_3[,-1], vl_3[,-1])
#' class_obj <- InteractionClassification(interaction_obj$vlogs_diff, interaction_obj$cds_diff)
#' 
#' # Define gates and parameters for payoffs
#' gates <- list(
#'   T = Tgate(2),
#'   X = X(2),
#'   Id = Id(2),
#'   H = H(2),
#'   Z = Z(2),
#'   S = S(2),
#'   Y = Y(2)
#' )
#' alpha <- -0.4518303; beta <- -1.654192; gamma <- -0.2638399; theta <- -0.5619246
#' alpha2 <- -0.04186888; beta2 <- -3.01931; gamma2 <- 0.3922753; theta2 <-1.055114
#' 
#' # Generate the payoffs list
#' payoffs_list <- payoffs_list(gates, alpha, beta, gamma, theta, alpha2, beta2, gamma2, theta2)
#' 
#' # Filtered data based on specific patient IDs
#' filtered_data <- class_obj$data |> 
#'   bind_cols(id = cd_3$ID) |> 
#'   relocate(id) |> 
#'   filter(id %in% c(37, 102, 148, 174, 180, 205))
#' 
#' # Apply the nearest_payoff function to each vl_diff in the filtered data
#' filtered_data <- filtered_data |>
#'   rowwise() |>
#'   mutate(nearest = list(nearest_payoff(vl_diff, payoffs_list))) |>
#'   mutate(nearest_payoff = nearest$value,
#'          payoff_name = nearest$name) |>
#'   select(-nearest) # Remove the intermediate column
#' 
#' # Display the updated filtered data with nearest payoff and its name
#' print(filtered_data)
nearest_payoff <- function(vl_diff, payoffs_list) {
  # Extract the payoff values and corresponding names
  payoff_values <- unlist(payoffs_list)
  payoff_names <- names(payoffs_list)
  
  # Compute the absolute differences between vl_diff and the payoffs
  differences <- abs(vl_diff - payoff_values)
  
  # Find the index of the minimum difference
  min_index <- which.min(differences)
  
  # Return a list containing the nearest payoff value and its corresponding name
  return(list(value = payoff_values[min_index], name = payoff_names[min_index]))
}