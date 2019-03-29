#' Create a new line for each footnote .tex
#'
#' @param texPath The path to a tex file, with the .tex extension.
#' Eg "report/myreport.tex"
#'
#' @importFrom readr read_lines write_lines
#'
#' @export

add_footnote_newline <- function(texPath) {

  tex <- readr::read_lines(texPath)

  # Line-by-line: does not start with \\footnote OR \t\\foot
  lbl <- function(x) {



    if (all(grepl("^(?!\\t\\\\foot)", tex[x], perl = TRUE),
            grepl("^(?!\\\\foot)", tex[x], perl = TRUE))) {

      message(paste("Fixing line", x))
      return(gsub("\\\\foot", "%\n\t\\\\foot", tex[x]))

    }

    message(paste("Skipping line", x))
    return(tex[x])

  }

  tex <- purrr::map(1:length(tex), .f = lbl)
  tex <- unlist(tex)

  readr::write_lines(tex, texPath)

}
