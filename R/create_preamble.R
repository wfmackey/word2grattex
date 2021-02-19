#' Generate and add the current grattex preamble to a pandoc-converted .tex file
#'
#' @param tex_file The path to the .tex file
#' @param removeReport.tex Remove the unnecessary Report.tex file? (default is TRUE)
#' @param isSegmented Is this part of a segmented report (i.e. a single chapter of a larger report?)
#'
#' @importFrom readr read_lines write_lines
#'
#' @export
#'


create_preamble <- function(tex_file,
                            removeReport.tex = TRUE,
                            isSegmented) {

  # Read tex file
  out_tex_lines <- readr::read_lines(tex_file)

  # Drop preamble and system chapters (Overview, Contents, Figures, Tables, Recommendations)
  message(paste0("Bulding a more ~Grattan-style~ .tex file"))
  ## Is there a preamble/system chapters?
  if (max(grepl("\\\\chapter\\{(Overview|Contents|Figures|Tables|Recommendations)", out_tex_lines)) > 0) {

    end.system.chapter.line <- max(grep("\\\\chapter\\{(Overview|Contents|Figures|Tables|Recommendations)", out_tex_lines)) + 1

  } else {

    end.system.chapter.line <- max(grep("\\\\begin\\{document\\}", out_tex_lines)) + 1

  }

  # Find non-system chapter lines
  if (max(grepl("\\\\chapter\\{[[:alpha:]]+", out_tex_lines))==1) {

    # Remove \texorpdfstring{X }{ from chapter lines
    out_tex_lines[end.system.chapter.line:length(out_tex_lines)]

    # grepl("\\\\chapter\\{\\\\texorpdfstring\\{[A-Za-z0-9\\s]*\\}\\{", a)
    # gsub("\\\\chapter\\{\\\\texorpdfstring\\{[[:print:]]*\\}\\{", "", a)
    #
    out_tex_lines <- gsub("\\\\chapter\\{\\\\texorpdfstring\\{[[:print:]]*\\}\\{",
                          "\\\\chapter\\{",
                          out_tex_lines[end.system.chapter.line:length(out_tex_lines)])

    chapter_lines <- grep("\\\\chapter\\{[[:alpha:]\\]+",
                          out_tex_lines[end.system.chapter.line:length(out_tex_lines)])

    first_chapter_line <- min(chapter_lines-1) # minus 1 to allow for hypertarget

    out_tex_lines <- out_tex_lines[first_chapter_line+end.system.chapter.line:length(out_tex_lines)]
  }


  # Drop references
  if (max(grepl("\\\\chapter\\{(References|Bibliography|Citations)", out_tex_lines))>0) {

    references <- cummax(grepl("\\\\chapter\\{(References|Bibliography|Citations)", out_tex_lines))
    out_tex_lines <- out_tex_lines[!(references)]

  }

  # Read current Report.tex for preamble
  getpreamble <- read_lines("Report.tex")

  if (removeReport.tex) file.remove("Report.tex")

  # The first line can carry over some cruft: remove closing double brace if found
  if (nchar(out_tex_lines[1]) - nchar(gsub("\\}", "", out_tex_lines[1])) !=  nchar(out_tex_lines[1]) - nchar(gsub("\\{", "", out_tex_lines[1])))  {
    out_tex_lines[1] <- gsub("\\}\\}", "\\}", out_tex_lines[1])
  }


  # Construct report framework based on segmentation status
  if (!isSegmented) {

    # Add preamble etc if NOT segmented

    out_tex_lines <-     c( getpreamble[(which(getpreamble == "\\documentclass[embargoed]{grattan}")):(which(getpreamble == "\\begin{document}"))],
                            "",
                            "\\contentspage",
                            "\\listoffigures",
                            "\\listoftables",
                            "",
                            "",
                            out_tex_lines,
                            "",
                            "\\printbibliography",
                            "",
                            "\\end{document}"
                          )
  }

  # Write
  readr::write_lines(out_tex_lines, tex_file)

  return(out_tex_lines)

}
