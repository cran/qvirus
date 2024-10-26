#' Create Interactions from CD4 and Viral Load Data
#'
#' This function computes the differences in CD4 counts and viral load from the provided datasets. 
#' It returns standardized values and logs transformed viral loads, enabling interaction analysis 
#' for further modeling.
#'
#' @param cd_data A data frame containing CD4 counts. Each row should represent an individual, 
#'                and each column should represent a different time point or measurement.
#' @param vl_data A data frame containing viral load measurements. It should have the same 
#'                 structure as `cd_data`, with rows for individuals and columns for time points.
#'
#' @return A list of class "interaction" containing:
#' \describe{
#'   \item{cd_diff}{A numeric vector of CD4 differences computed from the input `cd_data`.}
#'   \item{cds_diff}{A numeric vector of standardized CD4 differences.}
#'   \item{vl_diff}{A numeric vector of viral load differences computed from the input `vl_data`.}
#'   \item{vlog_diff}{A numeric vector of log-transformed viral load differences.}
#'   \item{vlogs_diff}{A numeric vector of standardized log-transformed viral load differences.}
#' }
#' @export
#'
#' @examples
#' # Example data frames for CD4 counts and viral loads
#' data(cd_3)
#' data(vl_3)
#'
#' # Create interactions
#' create_interactions(cd_3[, -1], vl_3[, -1])
create_interactions <- function(cd_data, vl_data) {
  
  # Compute CD4 differences
  cd_diff <- cd_data |>
    dplyr::rowwise() |>
    dplyr::mutate(cd_diff_values = mean(diff(stats::na.omit(dplyr::c_across(dplyr::everything()))))) |>
    dplyr::ungroup() |>
    dplyr::select(cd_diff_values)
  
  # Standardize CD4 data
  cd_std <- cd_data |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ scale(.) |> as.vector())) 
  cds_diff <- cd_std |>
    dplyr::rowwise() |>
    dplyr::mutate(cd_std_diff_values = mean(diff(stats::na.omit(dplyr::c_across(dplyr::everything()))))) |>
    dplyr::ungroup() |>
    dplyr::select(cd_std_diff_values)
  
  # Compute Viral Load differences
  vl_diff <- vl_data |>
    dplyr::rowwise() |>
    dplyr::mutate(vl_diff_values = mean(diff(stats::na.omit(dplyr::c_across(dplyr::everything()))))) |>
    dplyr::ungroup() |>
    dplyr::select(vl_diff_values)
  
  # Log transform for viral load data
  vl_log <- vl_data |>
    dplyr::transmute(dplyr::across(dplyr::everything(), ~ ifelse(. == 0, 0, log10(.)), .names = "log_{col}"))
  vlog_diff <- vl_log |>
    dplyr::rowwise() |>
    dplyr::mutate(vlog_diff_values = mean(diff(stats::na.omit(dplyr::c_across(dplyr::everything()))))) |>
    dplyr::ungroup() |>
    dplyr::select(vlog_diff_values)
  
  # Standardize log-transformed viral load data
  vlog_std <- vl_log |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ scale(.) |> as.vector())) 
  vlogs_diff <- vlog_std |>
    dplyr::rowwise() |>
    dplyr::mutate(vlogs_std_diff_values = mean(diff(stats::na.omit(dplyr::c_across(dplyr::everything()))))) |>
    dplyr::ungroup() |>
    dplyr::select(vlogs_std_diff_values)
  
  # Return as a list (S3 object)
  structure(list(cd_diff = unlist(cd_diff$cd_diff_values), 
                 cds_diff = unlist(cds_diff$cd_std_diff_values), 
                 vl_diff = unlist(vl_diff$vl_diff_values), 
                 vlog_diff = unlist(vlog_diff$vlog_diff_values), 
                 vlogs_diff = unlist(vlogs_diff$vlogs_std_diff_values)), 
            class = "interaction")
}

utils::globalVariables(c("cd_diff_values", "cd_std_diff_values", "vl_diff_values", "vlog_diff_values", "vlogs_std_diff_values"))
