#' Save ggplot to a Standardized PowerPoint-Quality PNG
#'
#' Saves a ggplot object as a high-quality PNG in the current working
#' directory, using standardized dimensions suitable for PowerPoint (10" x 5.625"
#' at 300 DPI).
#'
#' @param plot_object The ggplot object to be saved.
#' @param file_name Character string: The base name for the output PNG file
#'   (e.g., "my_plot"). The ".png" extension is added automatically.
#'
#' @return Invisibly returns the full path to the saved PNG file.
#' @export
#' @importFrom ggplot2 ggsave is.ggplot
#' @examples
#' \dontrun{
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   my_chart <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) +
#'               ggplot2::geom_point() +
#'               ggplot2::ggtitle("Fuel Efficiency vs. Weight")
#'
#'   save_ggplot_std(my_chart, "mtcars_efficiency_chart")
#' }
#' }
save_ggplot_std <- function(plot_object, file_name) {

  # Standardized settings
  standard_path <- "." # Current working directory
  standard_width_in <- 10 # Standard width for 16:9 slide
  standard_height_in <- 5.625 # Standard height for 16:9 slide (10 * 9/16)
  standard_dpi <- 300 # High quality
  standard_bg <- "white" # Standard background

  # Check if plot_object is a ggplot object
  if (!ggplot2::is.ggplot(plot_object)) {
    stop("The 'plot_object' argument must be a ggplot object.")
  }

  # Validate file_name: must be a non-empty character string
  if (!is.character(file_name) || length(file_name) != 1 || nchar(trimws(file_name)) == 0) {
    stop("'file_name' must be a single, non-empty character string.")
  }

  # Ensure file_name does not inadvertently contain path separators
  if (grepl(.Platform$file.sep, file_name, fixed = TRUE)) {
    stop("'file_name' should not contain path separators. For saving to a specific path, use a different function.")
  }

  # Remove .png extension if user accidentally provided it, as ggsave adds it.
  if (grepl("\\.png$", file_name, ignore.case = TRUE)) {
    message("Note: '.png' extension in 'file_name' is redundant and will be handled.")
    file_name <- sub("\\.png$", "", file_name, ignore.case = TRUE)
  }

  # Construct the full file path for the current working directory
  # The paste0 function will correctly append ".png"
  full_file_path <- file.path(standard_path, paste0(file_name, ".png"))

  # Save the plot using ggplot2::ggsave with standardized settings
  ggplot2::ggsave(
    filename = full_file_path,
    plot = plot_object,
    width = standard_width_in,
    height = standard_height_in,
    dpi = standard_dpi,
    units = "in",
    bg = standard_bg
  )

  message("Plot saved to: ", normalizePath(full_file_path))
  return(invisible(normalizePath(full_file_path)))
}
