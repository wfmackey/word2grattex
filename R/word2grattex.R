#' Convert a Grattan report-style Word document to .tex
#'
#' @param path The path of the folder containing your Word document.
#' @param overwrite Overwrite an existing .tex file.
#' @param downloadGrattex Download the most recent version of Grattex. Set to TRUE if the Word document is not located within a Grattex folder.
#' @param removeReport.tex Delete the default Grattex .tex file.
#' @param bibReplace Add in-text citations using bib2grattex.
#' @param buildFigures Automatically build figure environments.
#' @param crossReferences Automatically create cross-references. Eg: “See Section 2.2” to "See \Cref{subsec:section-name}".
#' @param segmented Set to TRUE if this is just a chapter or section of a larger document.
#' @param sobSectionName Set to TRUE if State Orange Book section names are desired.
#' @param testRun To test new features of word2grattex. Leave as FALSE.
#' @examples
#' \dontrun{
#' library(word2grattex)
#' word2grattex(path = path/to/worddoc/folder)
#' }
#' @export



word2grattex <- function(path = ".",
                         overwrite = TRUE,
                         downloadGrattex = FALSE,
                         removeReport.tex = TRUE,
                         bibReplace = TRUE,
                         buildFigures = TRUE,
                         crossReferences = TRUE,
                         segmented = FALSE,
                         sobSectionName = "",
                         testRun = FALSE
                         ) {


# ---- Convert Word document to .tex using pandoc ---- #

  if (!nzchar(Sys.which("pandoc"))) {
    stop("pandoc not found on the system path.")
  }

  current_wd <- getwd()
  setwd(normalizePath(path.expand(path)))
  on.exit(setwd(current_wd))

# ---- Download grattex template ---- #
  ##-- only for testing; actual conversions should take place within a GitHub clone of grattex ##

  # Get grattex
  if (downloadGrattex) {
    download.file(url ="https://github.com/HughParsonage/grattex/archive/master.zip",
                  destfile = "grattex.zip" )
    # unzip the .zip file
    unzip(zipfile = "grattex.zip")
    file.remove("grattex.zip")
  }


# ---- Convert Word document in path ---- #

  # Search for Word document
  file.docx <- dir(path = ".",
                   pattern = "\\.docx$",
                   full.names = TRUE)

  # Issue error if !=1 Word doc found
  if (length(file.docx) != 1L) {
    if (length(file.docx) == 0L) {
      stop("No .docx files found in `path`.")
    } else {
      if (any(startsWith(basename(file.docx), "~$"))) {
        warning(".docx file found starting with '~$'. ",
                "Likely reason: file is open in Word. ",
                "Close the file and try again.")
      }
      stop("Multiple .docx files found in `path`.")
    }
  }

  # Generate name for .tex file
  out.tex <- sprintf("%s.tex", tools::file_path_sans_ext(file.docx))


  # Check if .tex file already exists and replace is permitted
  if (!overwrite && file.exists(out.tex)) {
    stop("`overwrite = FALSE` but `", out.tex, "` is present in `path`.")
  }

  # Convert using pandoc via bash
  if (tolower(.Platform$OS.type) == "windows") {
    shell(sprintf("pandoc --wrap=none --top-level=chapter -s %s -o %s", file.docx, out.tex))
  } else {
    system(sprintf("pandoc --wrap=none --top-level=chapter -s %s -o %s", file.docx, out.tex))
  }


# ---- Read tex lines and set construct report framework ---- #

  # Read lines from pandoc output
  out_tex_lines <- read_lines(out.tex)


  # Drop preamble and system chapters (Overview, Contents, Figures, Tables, Recommendations)
  ## Is there a preamble/system chapters?
  if (max(grepl("\\\\chapter\\{(Overview|Contents|Figures|Tables|Recommendations)", out_tex_lines)) > 0) {

    end.system.chapter.line <- max(grep("\\\\chapter\\{(Overview|Contents|Figures|Tables|Recommendations)", out_tex_lines)) + 1

  } else {

    end.system.chapter.line <- max(grep("\\\\begin\\{document\\}", out_tex_lines)) + 1

  }

  if (max(grepl("\\\\chapter\\{[[:alpha:]]+", out_tex_lines))==1) {
  out_tex_lines <- out_tex_lines[((min(grep("\\\\chapter\\{[[:alpha:]]+", out_tex_lines[end.system.chapter.line:length(out_tex_lines)]))-1)+end.system.chapter.line:length(out_tex_lines))]
  }


  # Drop references
  if (max(grepl("\\\\chapter\\{(References|Bibliography|Citations)", out_tex_lines))>0) {

    references <- cummax(grepl("\\\\chapter\\{(References|Bibliography|Citations)", out_tex_lines))
    out_tex_lines <- out_tex_lines[!(references)]

  }

  # Search for Report.tex for preamble
  if (!file.exists("Report.tex")) {
    # Download and add current Grattex preamble if not present
    download.file(url ="https://raw.githubusercontent.com/HughParsonage/grattex/master/Report.tex", destfile = "Report.tex")
  }

  getpreamble <- read_lines("Report.tex")

  if (removeReport.tex) file.remove("Report.tex")

  # The first line can carry over some cruft: remove closing double brace if found
  if (nchar(out_tex_lines[1]) - nchar(gsub("\\}", "", out_tex_lines[1])) !=  nchar(out_tex_lines[1]) - nchar(gsub("\\{", "", out_tex_lines[1])))  out_tex_lines[1] <- gsub("\\}\\}", "\\}", out_tex_lines[1])


  # Construct report framework based on segmentation status
  if (!segmented) {

      out_tex_lines <-     c( getpreamble[(which(getpreamble == "\\documentclass{grattan}")):(which(getpreamble == "\\begin{document}"))],
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


# ---- Clean up pandoc conversion annoyances

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
    if (max(grepl(             "\\chapter\\{[[:print:]]*Appendix[[:print:]]*", out_tex_lines))) {

        out_tex_lines[min(grep("\\chapter\\{[[:print:]]*Appendix[[:print:]]*", out_tex_lines)) - 1] <- "\\appendix"
    }


  # Replace ie and etc with \ie and \etc
  out_tex_lines <- gsub("(E|e)tc\\.\\s?", "\\\\etc ", out_tex_lines)  # this is unlikely to cause issue: http://www.thefreedictionary.com/words-that-end-in-etc
  out_tex_lines <- gsub("(I|i)\\.e\\.\\s?", "\\\\ie ", out_tex_lines)
  out_tex_lines <- gsub("([\\s\\(]{1})(I|i)e\\.\\s?" , "\\1\\\\ie ", out_tex_lines) # only look for _ie. or (ie. to avoid natural words ending in ie (eg chippie.)
  out_tex_lines <- gsub("(\\(|\\s|\\{)(E|e)\\.?g\\.\\s?", "\\1\\\\eg ", out_tex_lines)  # this is unlikely to cause issue: http://www.thefreedictionary.com/words-that-end-in-etc




# ---- Replacing in-text citations in bib ---- #
    ## This needs to be done before Figure references
    ## to ensure external citation figure references
    ## are included withtin \textcite[][figure~1.1]{...}
    tomaster.tex <- paste0("grattex-master/",substring(out.tex,3))

    if (bibReplace) {
      print("Fixing references")


      # Write tex file temporarily
      write_lines(out_tex_lines, "outtex.tex")

      # Assign bib
      assign("bib",
             dir(path = ".", pattern = "\\.bib$", full.names = TRUE) %>%
             substring(., 3))


      # assign("bib", dir(path = ".",
      #                   pattern = "\\.bib$",
      #                   full.names = TRUE))
print(bib)
      if (downloadGrattex)  bibPath <- paste0("grattex-master/bib/", bib)
      if (!downloadGrattex) bibPath <- paste0("bib/", bib)


      bib2grattex(bibName = bib,
                  texName = "",
                  fromWord2grattex = TRUE)
print(100)
print(downloadGrattex)
print(bib)
print(bibPath)
print(getwd())
      file.copy(bib, bibPath)
print(100)
      file.remove(bib)

      # Read in again
      out_tex_lines <- read_lines("outtex.tex")
      # Drop temp file
      file.remove("outtex.tex")


      # Remove old "put-new-refs-here" file and replace with new
      if (downloadGrattex) {
        # tryCatch(file.remove("grattex-master/bib/put-new-refs-here.bib"))
        file.rename(bibPath, "grattex-master/bib/put-new-refs-here.bib")
      }
      if (!downloadGrattex) {
        # tryCatch(file.remove("bib/put-new-refs-here.bib"))
        file.rename(bibPath, "bib/put-new-refs-here.bib")
      }
    }


    message("Your bib file has been renamed 'put-new-refs-here.bib' and is stored in the bib/ folder")


# ---- Build Figure environments ---- #

  # Build the Figure environment where \includegraphics is found
  numberOfFigures <- sum(grepl("\\\\includegraphics", out_tex_lines))

  # Add graphics or comment out:
  if (!buildFigures) out_tex_lines <- gsub("\\\\includegraphics", "\\%\\%\\\\includegraphics", out_tex_lines)

  if (buildFigures) {


    # Look for a PDF file; if none use default "chartdeck.pdf"
    chartspath <- dir(path = ".", pattern = "\\.pdf$", full.names = TRUE)
    if (length(chartspath) == 0) {
      charts.pdf <- "chartdeck.pdf"
    } else {
      charts.pdf <- substring(chartspath, 3)
    }


    # Set counter for chart numbering
    counter = 0


    # Loop over all lines with charts to build Figure environments
    for (l in grep("(\\\\includegraphics[^\\{]*\\{.*?\\})(.*)$", out_tex_lines)) {

      # Find the "Figure" line above includegraphics
      if (max(grepl("\\s?Figure\\s[\\s\\.0-9]{0,3}\\:", out_tex_lines[(l-1):(l-8)]))) {

        fig <- min(grep("\\s?Figure\\s[\\s\\.0-9]{0,3}\\:", out_tex_lines[(l-1):(l-8)]))

      } else {

        fig <- 0

      }


      found.fig <- fig > 0

      if (sum(grepl("includegraphics", out_tex_lines[(l-1):(l-8)])) > 0) {
        previous.graphic <- min(grep("includegraphics", out_tex_lines[(l-1):(l-8)]))
      } else {
        previous.graphic <- 0
      }

      collision <- ifelse(previous.graphic==0 | previous.graphic>fig, FALSE, TRUE)

      counter = counter + 1
      print(counter)
      # only perform if Figure: is found and there is no collision; otherwise just comment out
      if (found.fig==TRUE & collision==FALSE) {

        # Extract figure text
        figtext  <- gsub("Figure\\s[\\.0-9]{0,3}\\:\\s?", "" , out_tex_lines[l-fig])
        # Remove any wayward \\
        figtext <- gsub("\\\\\\\\", "" , figtext)

        out_tex_lines[l-fig] <- ""  # remove figure text from original position

        figlabel <- tolower(gsub("[[:punct:]]", "" , figtext))
        figlabel <- gsub("\\s", "-", figlabel)
        figlabel <- paste0("fig:", figlabel)


        # Units
        ## Units sometimes precede includegraphics on the same line; move to preceding line
        ## Test for graphical characters before \\includegraphics and move them down a line if found
        if(grepl("[[:graph:]]+\\s?\\\\includegraphics", out_tex_lines[l])) {
          moveup <- gsub("([[:graph:]]+)\\s?\\\\.*", "\\1", out_tex_lines[l])
          moveline <- 1
          out_tex_lines[l-moveline] <- moveup
          out_tex_lines[l] <- gsub("([[:print:]]+)(\\\\.*)", "\\2", out_tex_lines[l])
        }

        ## Units can take up two lines, meaning 'Figure' is 7 lines above
        ### How many lines of non-empty text are between Figure and includegraphics?
        unitblock <- out_tex_lines[(l-fig+1):(l-1)]
        unittext <-  paste0(unitblock[grep("[[:graph:]]+",unitblock)], collapse = ". ")

        # Notes or sources sometimes procede includegraphics on the same line
        if(grepl("(Notes)|(Source)\\:(.*)", out_tex_lines[l])) {
          out_tex_lines[l+1] <- gsub("\\\\includegraphics.*((Notes)|(Source).*)", "\\1", out_tex_lines[l])
          out_tex_lines[l] <- "" # noting that we don't actually need the original includegraphics line
          # out_tex_lines[l+1] <- gsub(".*(emf\\})?(.*)", "\\2", out_tex_lines[l])
          # out_tex_lines[l]   <- gsub("(.*)(emf\\}).*", "\\1", out_tex_lines[l])
        }

        # Set includegraphics line
        includegraphics <- paste0("\\includegraphics[page= ",
                                  counter,
                                  ", width=1\\columnwidth]{atlas/",
                                  charts.pdf,
                                  "}")

        # Identify Notes and/or Sources below includegraphics
        if (max(grepl("Sources?\\:", out_tex_lines[(l+1):(l+6)]))) {

          sourceline <- min(grep("Sources?\\:", out_tex_lines[(l+1):(l+6)]))

        } else {

          sourceline <- 0

        }
        sourceline

        source   <- ifelse(sourceline>0, "source", "")
        source.s <- ifelse(grepl("Sources\\:", out_tex_lines[l+sourceline]), "s", "")

        if (max(grepl("Notes?\\:", out_tex_lines[(l+1):(l+6)]))) {

          noteline <- min(grep("Notes?\\:", out_tex_lines[(l+1):(l+6)]))

        } else {

          noteline <- 0

        }

        note   <- ifelse(noteline>0, "note", "")
        note.s <- ifelse(grepl("Notes\\:", out_tex_lines[l+noteline]), "s", "")

        with   <- ifelse(sourceline>0 & noteline>0, "with", "")

        subcom <- paste0(note, note.s, with, source, source.s)

        # Remove \\emph {} from notes/sources and extract
        if (sourceline>0) {
          # out_tex_lines[l+sourceline] <- gsub("\\{",      "", out_tex_lines[l+sourceline])   # to fix #44
          # out_tex_lines[l+sourceline] <- gsub("\\}",      "", out_tex_lines[l+sourceline])   # to fix #44
          out_tex_lines[l+sourceline] <- gsub("\\\\emph", "", out_tex_lines[l+sourceline])
          sourcetext <- as.character(out_tex_lines[l+sourceline])
          sourcetext <- gsub("Sources?\\:\\s?", "", sourcetext)
          sourcetext <- gsub("\\\\\\\\", "", sourcetext)
          out_tex_lines[l+sourceline] <- ""
        }

        if (noteline>0) {
          # out_tex_lines[l+noteline] <- gsub("\\{",      "", out_tex_lines[l+noteline])    # to fix #44
          # out_tex_lines[l+noteline] <- gsub("\\}",      "", out_tex_lines[l+noteline])    # to fix #44
          out_tex_lines[l+noteline] <- gsub("\\\\emph", "", out_tex_lines[l+noteline])
          notetext <- out_tex_lines[l+noteline]
          notetext <- gsub("Notes?\\:\\s?", "", notetext)
          notetext <- gsub("\\\\\\\\", "", notetext)
          out_tex_lines[l+noteline] <- ""
        }

        # Build figure environment
        if (fig == 2) start = 3 else start = 4
        if (grepl("[[:print:]]", out_tex_lines[(fig + start)])) start = start-1

        out_tex_lines[l-(start)] <-     paste0("    \\begin{figure} %% original Figure ", counter)
        out_tex_lines[l-(start-1)] <-   paste0("    \\caption{",figtext,"}\\label{", figlabel, "}")
        out_tex_lines[l-(start-2)] <-   paste0("    \\units{",unittext,"}")
        out_tex_lines[l-(start-3)] <-   paste0("    ", includegraphics)
        if (noteline>0  & sourceline>0 )  out_tex_lines[l-(start-4)] <- paste0("    \\",subcom,"{",notetext,"}{", sourcetext, "}")
        if (noteline==0 & sourceline>0 )  out_tex_lines[l-(start-4)] <- paste0("    \\",subcom,"{",sourcetext, "}")
        if (noteline>0  & sourceline==0)  out_tex_lines[l-(start-4)] <- paste0("    \\",subcom,"{",notetext, "}")
        out_tex_lines[l-(start-5)] <- "    \\end{figure}"

      } # end  if (found.fig==TRUE & collision==FALSE) {

      if (found.fig == FALSE | collision == TRUE) {
        out_tex_lines[l] <- gsub("\\\\includegraphics", "\\%\\%\\\\includegraphics", out_tex_lines[l])
        figlabel <- paste0("ref-to-FIGURE",counter)
      }


      if (counter == 1) figRefs <- figlabel else figRefs <- c(figRefs, figlabel)


    } # end loop over graphics lines


    # Replace in-text Figure references with cross-references
    for (f in 1:numberOfFigures) {
    out_tex_lines <- gsub(paste0("Figures?\\s?", f,"([^0-9])"), paste0("\\\\Cref\\{", figRefs[f], "\\}\\1"), out_tex_lines)
    }

    # Move chartdeck to
    if (length(chartspath) >0) {

      if (downloadGrattex)     file.copy(chartspath, paste0("grattex-master/atlas/",charts.pdf))
      if (!downloadGrattex)    file.copy(chartspath, paste0("atlas/",charts.pdf))

      file.remove(chartspath)
    }



  }   # end buildFigures




# ---- Build Table environments ---- #

  # Building Table environment
  table.environments <- TRUE
  # Loop over all lines with \begin{longtable} to build Table environments
  if (table.environments) {
    for (l in grep("\\\\begin\\{longtable\\}", out_tex_lines)) {

      # Find the "Table" line above includegraphics
      tab <- max(grep("^\\s?Table[[:print:]]{0,20}\\:(.*)", out_tex_lines[(l-8):(l-1)]))
      tab <- 9 - tab
      if (!tab==Inf) {
        # Get table caption text
        tabletext <- gsub("^\\s?Table[[:print:]]{0,20}\\:\\s?(.*)", "\\1", out_tex_lines[l-tab])

        # Open the table environment and add caption (on the same line, to be safe)
        out_tex_lines[(l-(tab))] <- paste0("\\begin{table} \\caption{", tabletext, "}")

        # find the closing longtable line
        close <- min(grep("\\\\end\\{longtable\\}" , out_tex_lines[l:length(out_tex_lines)]))

        # close the table environment
        out_tex_lines[l+(close+1)] <- "\\end{table}"
      }

    }
  }



# Write to tex file
  write_lines(out_tex_lines, out.tex)



# ---- Cross-references ---- #
if (crossReferences) {
  # CURRENT:
  # Add appropriate prefix to labels for chapters, sections, subsections
  out_tex_lines <- gsub("(\\\\chapter\\{[^\\}]*\\}\\\\label\\{)([^\\}]*)\\}","\\1chap\\:\\2\\}", out_tex_lines)
  out_tex_lines <- gsub("(\\\\section\\{[^\\}]*\\}\\\\label\\{)([^\\}]*)\\}","\\1sec\\:\\2\\}", out_tex_lines)
  out_tex_lines <- gsub("(\\\\subsection\\{[^\\}]*\\}\\\\label\\{)([^\\}]*)\\}","\\1subsec\\:\\2\\}", out_tex_lines)
  out_tex_lines <- gsub("(\\\\subsubsection\\{[^\\}]*\\}\\\\label\\{)([^\\}]*)\\}","\\1subsubsec\\:\\2\\}", out_tex_lines)

  if (testRun) {
  # PLANNED:
  # identifty headers
  headers      <- c("chapter", "section", "subsection", "subsubsection")
  shortHeaders <- c("chap"   , "sec"    , "subsec",     "subsubsec"    )
    ## for...

    ## for each header line
header      <- headers[2]
shortHeader <- shortHeaders[2]
out_tex_lines[51]
l <- 51
      ### pull out header title and create chapter-specific label
      thisHeader <- gsub(paste0("\\\\",header,"\\{([^\\}]*)\\}\\\\label.*"),"\\1", out_tex_lines[l])
      thisHeaderLabel <- gsub("[[:punct:]]", "", thisHeader)
      thisHeaderLabel <- paste0(shortHeader, ":", gsub("\\s", "-", tolower(thisHeader)))
      if (sobSectionName != "") thisHeaderLabel <- paste0(thisHeaderLabel, "-", sobSectionName)
      ### sub in label
      out_tex_lines[l] <- gsub(paste0("\\\\",header,"\\{([^\\}]*)\\}\\\\label.*"),"\\1", out_tex_lines[l])




  }




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
    print(paste0("Starting chapter ", chap))
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
    print(paste0("Starting section ", sec))
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


}

# ---- Create a new line when a footnote ENDS A SENTENCE ---- #
if (testRun) {

  # This might be difficult; so need to construct in `safe mode'
  # Could define a function to do this -> https://stackoverflow.com/questions/17623147/split-string-without-loss-of-characters?noredirect=1&lq=1
  a <- "\\footnote{this is text and {thisisreference1}[][section~2]{ref2}} And then this is the next sentence with a.\\footnote{this is text and {thisisreference2}[][33--34]{ref2}} And then -- boom -- more \\footcite{third {thisisreference3}[][]{ref3}}"
  b <- "\\footnote{this is text and {thisisreference1}[][section~2]{ref2}}"
  str_split(a, "\\\\foot(?:note|cite)\\{[a-zA-Z\\s\\\\]*")

  # in each line, after the footnote of a line that contains more than one, add "thispatternisunlikelytoappear"; then split with str_split and add below?
  findMe <- "LetsBreakThisLine"
  pattern <- "(\\\\foot(?:note|cite)[^\\\\foot]*)"

  ## contains more than one
  grepl(paste0(pattern, pattern, "+"),  a)
   gsub(paste0(pattern, pattern, "+"), "start\\1YES\\2YES\\3", a)
  grepl("\\\\foot(note|cite).*(\\\\foot(note|cite))+",  b)
  ### good
  ## for those that contain more than one,

}


# ---- Write file and place files in appropriate folders ---- #

  # Write to tex file
  write_lines(out_tex_lines, out.tex)


  # Move to grattex folder if grattex folder was downloaded
  if (downloadGrattex) {

    file.copy(out.tex, file.path(tomaster.tex))
    file.remove(out.tex)

    if (removeReport.tex) file.remove("./grattex-master/Report.tex")

  }

  if (!downloadGrattex)  tomaster.tex <- paste0(substring(out.tex,3))


  print("Conversion complete")



  # Be kind: set working directory back to normal
  setwd(current_wd)

}

