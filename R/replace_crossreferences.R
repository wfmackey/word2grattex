#' Replace Word and hard-coded cross-references with LaTeX `\\Cref`s
#'
#' @param tex_file The path to the .tex file
#'
#' @importFrom readr read_lines write_lines
#'
#' @export
#'


replace_crossreferences <- function(tex_file) {

  # Read
  out_tex_lines <- readr::read_lines(tex_file)

  # Add appropriate prefix to labels for chapters, sections, subsections
  out_tex_lines <- gsub("(\\\\chapter\\{[^\\}]*\\}\\\\label\\{)([^\\}]*)\\}","\\1chap\\:\\2\\}", out_tex_lines)
  out_tex_lines <- gsub("(\\\\section\\{[^\\}]*\\}\\\\label\\{)([^\\}]*)\\}","\\1sec\\:\\2\\}", out_tex_lines)
  out_tex_lines <- gsub("(\\\\subsection\\{[^\\}]*\\}\\\\label\\{)([^\\}]*)\\}","\\1subsec\\:\\2\\}", out_tex_lines)
  out_tex_lines <- gsub("(\\\\subsubsection\\{[^\\}]*\\}\\\\label\\{)([^\\}]*)\\}","\\1subsubsec\\:\\2\\}", out_tex_lines)


  # Apply cross-references to current in-text cross references

  # Part 1: count chapters, sections and subsections and determine correspondence between number and label
  ## Define chapter areas

  level <- "chapter"
  chapter.starts <- grep(paste0("\\\\",level,"\\{.*\\}\\\\label\\{(.*)\\}"), out_tex_lines)
  chapter.labels <- gsub("\\\\chapter\\{.*\\}\\\\label\\{(.*)\\}", "\\1", out_tex_lines[chapter.starts])
  chapter.total  <- length(chapter.starts)
  chapter.ends   <- c(chapter.starts[2:chapter.total]-2, length(out_tex_lines))
  chapters       <- seq(1, chapter.total, 1)
  chapter.text   <- as.character(chapters)


  level <- "section"
  section.starts <- grep(paste0("\\\\",level,"\\{(.*)\\}\\\\label\\{(.*)\\}"), out_tex_lines)
  section.labels <- gsub(paste0("\\\\",level,"\\{(.*)\\}\\\\label\\{(.*)\\}"), "\\2", out_tex_lines[section.starts])
  section.ends   <- c(section.starts[2:length(section.starts)]-2, length(out_tex_lines))
  section.text <- ""

  level <- "subsection"
  subsection.starts <- grep(paste0("\\\\",level,"\\{(.*)\\}\\\\label\\{(.*)\\}"), out_tex_lines)
  subsection.labels <- gsub(paste0("\\\\",level,"\\{(.*)\\}\\\\label\\{(.*)\\}"), "\\2", out_tex_lines[subsection.starts])
  subsection.ends   <- c(subsection.starts[2:length(subsection.starts)]-2, length(out_tex_lines))
  subsection.text <- ""


  # Part 2: Loop sections over chapters to get relative section numbering
  first = 0
  for (chap in chapters) {

    # Retrieve sections in this chapter
    current.section <- section.starts[section.starts >= chapter.starts[chap] & section.starts <= chapter.ends[chap]]
    section.total  <- length(current.section)

    if (section.total>0) {
      current.sections       <- seq(1, section.total, 1)
      first = first + 1
      if (first == 1) {
        sections      <- current.sections
        section.text  <- paste0(chap,".", current.sections)
      } else {
        sections      <- c(sections, current.sections)
        section.text  <- c(section.text, paste0(chap,".", current.sections))
      }
    }

  }

  # Loop subsections over sections to get relative subsection numbering
  first = 0
  for (sec in 1:length(section.starts)) {

    # Retrieve sections in this section
    current.subsection <- subsection.starts[subsection.starts >= section.starts[sec] & subsection.starts <= section.ends[sec]]
    subsection.total  <- length(current.subsection)

    if (subsection.total>0) {
      current.subsections       <- seq(1, subsection.total, 1)
      first = first + 1
      if (first == 1) {
        subsections       <- current.subsections
        subsection.text     <- paste0(section.text[sec],".", current.subsections)
      } else {
        subsections     <- c(subsections, current.subsections)
        subsection.text <- c(subsection.text, paste0(section.text[sec],".", current.subsections))
      }
    }

  }

  # Part 3: Create 'find' and 'replace', then replace
  type <- c(rep("Chapter",    length(chapter.text)),
            rep("Section",    length(section.text)),
            rep("Subsection", length(subsection.text)))

  text    <- c(chapter.text, section.text, subsection.text)
  find <- gsub("\\.", "\\\\\\.", text)
  replace <- c(chapter.labels, section.labels, subsection.labels)

  for (n in length(find):1) {

    # Match chapter: finding n
    # Chapters n and 2
    # to do
    # Chapters 1 and n
    # to do

    message(paste0("Replacing x-ref '", type[n], " ", gsub("\\\\", "", find[n]), "' with \\Cref{", replace[n]), "}")

    if (type[n] == "Chapter") {
      # Chapter n
      out_tex_lines <- gsub(paste0("(C|c)hapters?\\s",find[n]), paste0("\\\\Cref\\{", replace[n] ,"}\\2"), out_tex_lines)
    }

    if (type[n] == "Section") {
      # Section n.n (or n.n.n)
      out_tex_lines <- gsub(paste0("(S|s)ections?\\s",find[n]), paste0("\\\\Cref\\{", replace[n] ,"}\\2"), out_tex_lines)
    }

    if (type[n] == "Subsection") {
      # Subsection n.n.n
      out_tex_lines <- gsub(paste0("(S|s)ubsections?\\s",find[n]), paste0("\\\\Cref\\{", replace[n] ,"}\\2"), out_tex_lines)
      out_tex_lines <- gsub(paste0("(S|s)ections?\\s",find[n]), paste0("\\\\Cref\\{", replace[n] ,"}\\2"), out_tex_lines)
    }


  }

  # Write
  readr::write_lines(out_tex_lines, tex_file)

}
