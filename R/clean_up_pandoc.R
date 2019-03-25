#' Clean up the mess created by pandoc during the conversion process.
#' 
#' This function is an _art_ rather than a _science_.
#'
#' @param tex_file The path to the .tex file
#' 
#' @importFrom readr read_lines write_lines
#'
#' @export
#'


clean_up_pandoc <- function(tex_file) {
  
  # Read lines
  out_tex_lines <- readr::read_lines(tex_file)
  
  message(paste0("Cleaning up after the messy Pandoc conversion"))
  
  # Remove numbers from sections and subsections
  out_tex_lines <- gsub("\\\\section\\{\\s*[0-9]\\.[0-9](.*?)\\}", "\\\\section{\\1}", out_tex_lines)
  out_tex_lines <- gsub("\\\\subsection\\{\\s*[0-9]\\.[0-9](.*?)\\}", "\\\\subsection{\\1}", out_tex_lines)
  
  # Remove sout
  out_tex_lines <- gsub("(\\\\sout\\{)(.*)(\\})", "\\2", out_tex_lines)
  
  # Remove \protect\hypertarget{}{...}
  out_tex_lines <- gsub("\\\\protect\\\\hypertarget\\{[A-z0-9\\_\\(\\-]*\\}\\{([A-z0-9\\_\\(\\-]*)\\}\\{([A-z0-9\\_\\(\\-]*)\\}", "\\1\\2", out_tex_lines)
  
  
  # Remove \protect\hyperlink{}{...}
  out_tex_lines <- gsub("\\\\protect\\\\hyperlink\\{.*?\\}\\{(.*?)\\}", "\\1", out_tex_lines)
  
  # Remove \texorpdfstring.
  ##  This can infect its own line and next. Eg:
  ##  \hypertarget{help-borrowing-and-repayment-trends}{%
  ##  \subsubsection{\texorpdfstring{\\
  ##  HELP borrowing and repayment trends }{ HELP borrowing and repayment trends }}\label{help-borrowing-and-repayment-trends}}
  ##  Tag each line in which texorpdfstring is found; perform an action on next line; then remove texorpdfstring
  
  
  ## Case 1: same line: \section{\texorpdfstring{xxx}{...}}\label...
  out_tex_lines <- gsub("(\\\\[a-z]*\\{)\\\\texorpdfstring\\{.*\\}\\{(.*\\})\\}(\\\\label)","\\1\\2\\3", out_tex_lines)
  
  
  ## Case 2: two lines, with or without emph: \section{\texorpdfstring{\emph{\\
  ### tag proceding line
  tag <- c(FALSE, grepl("(.*)\\\\texorpdfstring\\{\\\\?e?m?p?h?\\{?\\\\\\\\", out_tex_lines)[1:(length(out_tex_lines)-1)])
  ### tag preceding line
  tagback <- c(tag[3:length(tag)], FALSE, FALSE)
  ### fix \\texorpdfstring
  out_tex_lines <- gsub("(.*)\\\\texorpdfstring\\{\\\\?e?m?p?h?\\{?\\\\\\\\", "\\1", out_tex_lines)
  
  out_tex_lines <- ifelse(tag,
                          gsub("(.*\\})\\{.*?\\}\\}(\\\\label.*?\\})\\}", "\\1\\2", out_tex_lines),
                          out_tex_lines)
  
  out_tex_lines <- ifelse(tagback,
                          gsub("\\\\hypertarget\\{.*\\}\\{\\%?", "", out_tex_lines),
                          out_tex_lines)
  
  
  
  
  # texorpdfstring leads to a three-line hypertarget issue. Fix this hypertarget before generic hypertarget fix.
  ## Remove \\hypertarget{ and proceding }
  ## Tag \\hypertarget elements and following line
  tag <- c(FALSE, grepl("\\hypertarget", out_tex_lines, fixed = TRUE)[1:(length(tag)-1)])
  ## Replace \\hypertarget elements with empty
  out_tex_lines <- gsub("\\\\hypertarget\\{.*\\}\\{", "", out_tex_lines)
  ## Remove the additional } in proceding lines
  out_tex_lines <- ifelse(tag,
                          gsub("(.*)\\}", "\\1", out_tex_lines),
                          out_tex_lines)
  
  # Comment out longtable
  out_tex_lines <- ifelse(1 == cumsum(as.integer(grepl("\\begin{longtable}", out_tex_lines, fixed = TRUE))
                                      - as.integer(c(FALSE, grepl("\\end{longtable}", out_tex_lines, fixed = TRUE)[1:(length(out_tex_lines)-1)]))),
                          paste0("%%",out_tex_lines),
                          out_tex_lines)
  
  
  # Remove headings that contain an empty chapter, section, subsection, etc
  out_tex_lines <- gsub("\\\\chapter\\{\\}\\\\label.*", "", out_tex_lines)  # section is default label
  out_tex_lines <- gsub("\\\\section\\{\\}\\\\label.*", "", out_tex_lines)  # section is default label
  out_tex_lines <- gsub("\\\\subsection\\{\\}\\\\label.*", "", out_tex_lines)  # section is default label
  
  # Add "\appendix" line
  # find the earliest Chapter title that contains "Appendix", and add "\\appendix" to the preceding line
  appendixSearch <- "\\chapter\\{[[:print:]]*Appendix[[:print:]]*"
  
  if (max(grepl(appendixSearch, out_tex_lines))) {
    
    message(paste0("Appendix found -- adding \\appendix tag"))
    
    out_tex_lines[min(grep(appendixSearch, out_tex_lines)) - 1] <- "\\appendix"
  }
  
  
  # Replace ie and etc with \ie and \etc
  message(paste0("Replacing ie, etc with the proper \\ie and \\etc"))
  
  out_tex_lines <- gsub("(E|e)tc\\.\\s?", "\\\\etc ", out_tex_lines)  # this is unlikely to cause issue: http://www.thefreedictionary.com/words-that-end-in-etc
  out_tex_lines <- gsub("(I|i)\\.e\\.\\s?", "\\\\ie ", out_tex_lines)
  out_tex_lines <- gsub("([\\s\\(]{1})(I|i)e\\.\\s?" , "\\1\\\\ie ", out_tex_lines) # only look for _ie. or (ie. to avoid natural words ending in ie (eg chippie.)
  out_tex_lines <- gsub("(\\(|\\s|\\{)(E|e)\\.?g\\.\\s?", "\\1\\\\eg ", out_tex_lines)  # this is unlikely to cause issue: http://www.thefreedictionary.com/words-that-end-in-etc
  
  # Write lines
  readr::write_lines(out_tex_lines, tex_file)
  
  return(out_tex_lines)
  
}