#' Build an Rmarkdown report into a PDF
#'
#' @param rmarkdown_path path of the rmarkdown file
#' @param output_name name of the report file on output, without file suffix
#' @param output_dir directory the report should be written to
#' @param params parameters to be passed on to the rmarkdown file
#' @param envir environment which should be used to execute rmarkdown::render;
#' often useful to specify a new environment including the reporting package, e.g. \code{new.env(parent = loadNamespace("packageName"))}
#'
#' @return NULL
#' @export
buildReport <- function(rmarkdown_path, output_name = "report", output_dir = getwd(), params = list(), envir = new.env()){
  install_casperjs()
  print("Successfully installed dependencies.")
  
  htmlPath <- paste0(output_dir, "/", output_file, ".html")
  pdfPath <- paste0(output_dir, "/", output_file, ".pdf")
  
  # Render HTML Report
  rmarkdown::render(rmarkdown_path, envir = envir,
                    output_dir = output_dir, output_file = paste0(output_name, ".html"),
                    params = params)
  
  # Render PDF Report
  casperHTMLtoPDF(paste0(output_dir, "/", output_name, ".html"),
    paste0(output_dir, "/", output_name, ".pdf"))
}
