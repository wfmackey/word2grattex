#' Build table _environments_ for cross-references. But this does not build tables :(
#'
#' @param tex_file The path to the .tex file
#'
#' @importFrom readr read_lines write_lines
#'
#' @export
#'

build_table_environments <- function(tex_file) {

  # Read
  out_tex_lines <- readr::read_lines(tex_file)

  # Loop over all lines with \begin{longtable} to build Table environments
  message("Building table environments (to enable cross-references)")
  counter = 0

  tableSearch <- "\\\\begin\\{longtable\\}"

  numberOfTables <- sum(grepl(tableSearch, out_tex_lines))

  if (numberOfTables == 0) message("No tables found!")

  if (numberOfTables > 0) {

  for (l in grep(tableSearch, out_tex_lines)) {

    counter = counter + 1

    # Find the "Table" line above includegraphics
    tab <- suppressWarnings(max(grep("^\\s?Table[[:print:]]{0,20}\\:(.*)", out_tex_lines[(l-8):(l-1)])))
    tab <- 9 - tab
    if (!tab == Inf) {
      # Get table caption text
      tabletext <- gsub("^\\s?Table[[:print:]]{0,20}\\:\\s?(.*)", "\\1", out_tex_lines[l-tab])

      # Create table label
      tablelabel <- tolower(gsub("[[:punct:]]", "" , tabletext))
      tablelabel <- gsub("\\s", "-", tablelabel)
      tablelabel <- paste0("tbl:", tablelabel)

      # Open the table environment and add caption (on the same line, to be safe)
      out_tex_lines[(l-(tab))] <- paste0("\\begin{table}",
                                         "\\caption{", tabletext, "}",
                                         "\\label{", tablelabel, "}")

      # add to tabRefs to allow replacement
      if (counter == 1) tabRefs <- tablelabel

      if (counter >  1) tabRefs <- c(tabRefs, tablelabel)


      # find the closing longtable line
      close <- min(grep("\\\\end\\{longtable\\}" , out_tex_lines[l:length(out_tex_lines)]))

      # close the table environment
      out_tex_lines[l+(close+1)] <- "\\end{table}"
    }

    if (tab == Inf) {
    tabRefs <- ""
    }

  }  # end table loop

  # Replace in-text Table references with cross-references
  for (t in 1:numberOfTables) {
    message("d")
    out_tex_lines <- gsub(paste0("Tables?\\s?", t,"([^0-9])"), paste0("\\\\Cref\\{", tabRefs[t], "\\}\\1"), out_tex_lines)
  }

  }

  # Write
  readr::write_lines(out_tex_lines, tex_file)

}
