#' Build table _environments_ for cross-references. But this does not build tables :(
#'
#' @param tex_file The path to the .tex file
#' @param haveDownloadedGrattex logical: is this part of word2grattex AND are we downloading grattex?
#' 
#' @importFrom readr read_lines write_lines
#'
#' @export
#'


build_figure_environments <- function(tex_file,
                                      haveDownloadedGrattex = FALSE) {
  
  message("Building figure environments")

  # Read
  out_tex_lines <- readr::read_lines(tex_file)
  
  # Build the Figure environment where \includegraphics is found
  numberOfFigures <- sum(grepl("\\\\includegraphics", out_tex_lines))
  
    
    # Look for a PDF file; if none use default "chartdeck.pdf"
    chartspath <- dir(path = ".", pattern = "\\.pdf$", full.names = TRUE)
    if (length(chartspath) == 0) {
      message("No chart pack .pdf found, using chartpack.pdf in the \\includegraphics call")
      charts.pdf <- "chartdeck.pdf"
    } else {
      charts.pdf <- substring(chartspath, 3)
      message(paste0("Chart pack .pdf found, using ", charts.pdf, " in the \\includegraphics call"))
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
      
      # only perform if Figure: is found and there is no collision; otherwise just comment out
      if (found.fig == TRUE & collision == FALSE) {
        
        # Extract figure text
        figtext  <- gsub("Figure\\s[\\.0-9]{0,3}\\:\\s?", "" , out_tex_lines[l-fig])
        
        # Remove any wayward \\
        figtext <- gsub("\\\\\\\\", "" , figtext)
        
        # Remove figure text from original position
        out_tex_lines[l-fig] <- ""
        
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
        if(grepl("(Notes)|(Source)\\:", out_tex_lines[l])) {
          
          out_tex_lines[l+1] <- gsub("\\\\includegraphics.*((Notes)|(Source).*)", "\\1", out_tex_lines[l])
          out_tex_lines[l] <- "" # We don't need the original includegraphics line, good to remove
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
        } else {
          sourcetext = "#####NO SOURCE FOUND#####"
        }
        
        if (noteline>0) {
          # out_tex_lines[l+noteline] <- gsub("\\{",      "", out_tex_lines[l+noteline])    # to fix #44
          # out_tex_lines[l+noteline] <- gsub("\\}",      "", out_tex_lines[l+noteline])    # to fix #44
          out_tex_lines[l+noteline] <- gsub("\\\\emph", "", out_tex_lines[l+noteline])
          notetext <- out_tex_lines[l+noteline]
          notetext <- gsub("Notes?\\:\\s?", "", notetext)
          notetext <- gsub("\\\\\\\\", "", notetext)
          out_tex_lines[l+noteline] <- ""
        } else {
          notetext = "#####NO NOTES FOUND#####"
        }
        
        # Build figure environment
        if (fig == 2) start = 3 else start = 4
        if (grepl("[[:print:]]", out_tex_lines[(fig + start)])) start = start-1
        
        message(paste0("Building original figure ", counter,
                       "\n   with title ", figtext,
                       "\n   (and hence label ", figlabel, ")",
                       "\n   units of ", if (unittext == "") "#####NO UNITS FOUND#####" else unittext, ","),
                "\n   notes of ", notetext, ",",
                "\n   source of ", sourcetext)
        
        out_tex_lines[l-(start)] <-     paste0("    \\begin{figure} %% original Figure ", counter)
        out_tex_lines[l-(start-1)] <-   paste0("    \\caption{",figtext,"}\\label{", figlabel, "}")
        out_tex_lines[l-(start-2)] <-   paste0("    \\units{",unittext,"}")
        out_tex_lines[l-(start-3)] <-   paste0("    ", includegraphics)
        if (noteline >  0  & sourceline  > 0)  out_tex_lines[l-(start-4)] <- paste0("    \\",subcom,"{",notetext,"}{", sourcetext, "}")
        if (noteline == 0  & sourceline  > 0)  out_tex_lines[l-(start-4)] <- paste0("    \\",subcom,"{",sourcetext, "}")
        if (noteline >  0  & sourceline == 0)  out_tex_lines[l-(start-4)] <- paste0("    \\",subcom,"{",notetext, "}")
        out_tex_lines[l-(start-5)] <- "    \\end{figure}"
        
      } # end  if (found.fig==TRUE & collision==FALSE) {
      
      if (found.fig == FALSE | collision == TRUE) {
        out_tex_lines[l] <- gsub("\\\\includegraphics", "\\%\\%\\\\includegraphics", out_tex_lines[l])
        figlabel <- paste0("ref-to-FIGURE",counter)
      }
      
      
      if (counter == 1) figRefs <- figlabel
      if (counter >  1) figRefs <- c(figRefs, figlabel)
      
      
    } # end loop over graphics lines
    
    
    # Replace in-text Figure references with cross-references
    for (f in 1:numberOfFigures) {
      out_tex_lines <- gsub(paste0("Figures?\\s?", f,"([^0-9])"), paste0("\\\\Cref\\{", figRefs[f], "\\}\\1"), out_tex_lines)
    }
    
    # Move chartdeck to
    if (length(chartspath) > 0) {
      
      if ( haveDownloadedGrattex)  file.copy(chartspath, paste0("grattex-master/atlas/", charts.pdf))
      if (!haveDownloadedGrattex)  file.copy(chartspath, paste0("atlas/",charts.pdf))
        
      
    }
    
    # Write
    readr::write_lines(out_tex_lines, tex_file)
    
  } 