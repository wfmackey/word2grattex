#' Create a new line for each footnote .tex
#'
#' @param texPath The path to the .tex file
#'
#' @importFrom readr read_lines write_lines
#'
#' @export

add_footnote_newline <- function(texPath) {

  tex <- readr::read_lines(texPath)

  tex <- gsub("\\\\foot", "%\n\t\\\\foot", tex)

  readr::write_lines(tex, texPath)

}
