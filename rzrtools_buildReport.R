#' Build an Rmarkdown report into a PDF
#'
#' @param rmarkdown_path path of the rmarkdown file
#' @param output_dir directory the report should be written to
#' @param output_file name of the report file on output
#' @param params parameters to be passed on to the rmarkdown file
#'
#' @return RETURN DESCRIPTION
#' @examples
#' # ADD EXAMPLES HERE
buildReport <- function(rmarkdown_path, output_dir = getwd(), output_file, params){
  reportingManager::install_casperjs()
  print("Successfully installed dependencies.")

  # Render HTML Report
  rmarkdown::render(rmarkdown_path, envir = new.env(),
                    output_dir = output_dir, output_file = output_file,
                    params = params)

  # Render PDF Report
  htmlPath <- paste0(output_dir, "/", output_file, ".html")
  pdfPath <- paste0(output_dir, "/", output_file, ".pdf")
  reportingManager::casperHTMLtoPDF(htmlPath, pdfPath)
}
