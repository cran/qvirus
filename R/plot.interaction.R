#' Plot Interaction Differences
#'
#' This function generates histograms and QQ plots for the differences in CD4 
#' and viral load values contained in an interaction object. Users can choose to 
#' plot all differences or specify which type to plot.
#'
#' @param x An interaction object containing the differences to be plotted. 
#' This object should include fields for CD4 differences (raw and standardized) 
#' and viral load differences (raw, log10-transformed, and log10-standardized).
#' @param type A character string indicating the type of plot to generate. 
#' Options include: "cd_diff", "cds_diff", "vl_diff", "vlog_diff", "vlogs_diff", 
#' or "all" (the default) to plot all types of differences.
#' @param ... Additional arguments passed to the plot function.
#'
#' @return Plots histograms and QQ plots for the specified differences. If 
#' `type` is "all", all plots are arranged in a grid layout.
#' @export
#'
#' @examples
#' \donttest{
#' data("vl_3")
#' data("cd_3")
#' interaction_obj <- create_interactions(cd_3, vl_3)
#' plot(interaction_obj, type = "cd_diff")    
#' }
plot.interaction <- function(x, type = "all", ...) {
  # Helper function to create individual plots
  create_plots <- function(data, title) {
    p1 <- ggplot2::ggplot(data, ggplot2::aes(x = value)) + 
      ggplot2::geom_histogram(bins = 30, fill = "blue", alpha = 0.7) + 
      ggplot2::labs(title = paste(title)) +
      ggplot2::theme_minimal()
    
    p2 <- ggplot2::ggplot(data, ggplot2::aes(sample = value)) + 
      ggplot2::stat_qq() + 
      ggplot2::stat_qq_line(color = "red") + 
      ggplot2::labs(title = paste(title)) +
      ggplot2::theme_minimal()
    
    return(list(p1 = p1, p2 = p2))
  }
  
  # Prepare the data
  cd_diff_data <- data.frame(value = unlist(x$cd_diff))  # Changed object to x
  cds_diff_data <- data.frame(value = unlist(x$cds_diff)) # Changed object to x
  vl_diff_data <- data.frame(value = unlist(x$vl_diff))   # Changed object to x
  vlog_diff_data <- data.frame(value = unlist(x$vlog_diff)) # Changed object to x
  vlogs_diff_data <- data.frame(value = unlist(x$vlogs_diff)) # Changed object to x
  
  # Create individual plots for each type of difference
  cd_diff_plots <- create_plots(cd_diff_data, "CD4 (raw)")
  cds_diff_plots <- create_plots(cds_diff_data, "CD4 (std)")
  vl_diff_plots <- create_plots(vl_diff_data, "Viral Load (raw)")
  vlog_diff_plots <- create_plots(vlog_diff_data, "Viral Load (log10)")
  vlogs_diff_plots <- create_plots(vlogs_diff_data, "Viral Load (log10 std)")
  
  # Determine what to plot based on user input
  if (type == "cd_diff") {
    gridExtra::grid.arrange(cd_diff_plots$p1, cd_diff_plots$p2, nrow = 1)
  } else if (type == "cds_diff") {
    gridExtra::grid.arrange(cds_diff_plots$p1, cds_diff_plots$p2, nrow = 1)
  } else if (type == "vl_diff") {
    gridExtra::grid.arrange(vl_diff_plots$p1, vl_diff_plots$p2, nrow = 1)
  } else if (type == "vlog_diff") {
    gridExtra::grid.arrange(vlog_diff_plots$p1, vlog_diff_plots$p2, nrow = 1)
  } else if (type == "vlogs_diff") {
    gridExtra::grid.arrange(vlogs_diff_plots$p1, vlogs_diff_plots$p2, nrow = 1)
  } else {
    # Plot all in a grid (2x5 layout)
    gridExtra::grid.arrange(cd_diff_plots$p1, cd_diff_plots$p2, 
                            cds_diff_plots$p1, cds_diff_plots$p2, 
                            vl_diff_plots$p1, vl_diff_plots$p2, 
                            vlog_diff_plots$p1, vlog_diff_plots$p2, 
                            vlogs_diff_plots$p1, vlogs_diff_plots$p2, 
                            nrow = 5, ncol = 2)
  }
}

utils::globalVariables("value")