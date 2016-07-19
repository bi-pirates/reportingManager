#' Build an Rmarkdown report into a PDF
#'
#' @param rmarkdown_path path of the rmarkdown file
#' @param output_file name of the report file on output
#' @param output_dir directory the report should be written to
#' @param params parameters to be passed on to the rmarkdown file
#' @param envir environment which should be used to execute rmarkdown::render; often useful to specify the reporting package, e.g. \code{loadNamespace("FourSeasons")}
#'
#' @return NULL
#' @export
buildReport <- function(rmarkdown_path, output_file, output_dir = getwd(), params = list(), envir = new.env()){
  install_casperjs()
  print("Successfully installed dependencies.")

  # Render HTML Report
  rmarkdown::render(rmarkdown_path, envir = envir,
                    output_dir = output_dir, output_file = output_file,
                    params = params)

  # Render PDF Report
  htmlPath <- paste0(output_dir, "/", output_file, ".html")
  pdfPath <- paste0(output_dir, "/", output_file, ".pdf")
  casperHTMLtoPDF(htmlPath, pdfPath)
}
