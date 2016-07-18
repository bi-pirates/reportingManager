#' Converts HTML file to PDF using Casperjs
#' @export
casperHTMLtoPDF <- function(htmlPath, pdfPath){
  htmlPath <- gsub(" ", "\\\\ ", htmlPath)
  pdfPath <- gsub(" ", "\\\\ ", pdfPath)

  casperjs_call <- sprintf("%1$s/bin/casperjs %1$s/js/pdf_export.js --htmlPath=%2$s --pdfPath=%3$s"
          , system.file(package = "FourSeasons"), htmlPath, pdfPath)
  system(casperjs_call)
}

#' Installs Casperjs
#' @export
install_casperjs <- function(){
  system("printf 'Node version is ' && node -v")
  system(sprintf("npm install --prefix %s -g casperjs@1.1.0-beta4",
    system.file(package = "FourSeasons")))
}
