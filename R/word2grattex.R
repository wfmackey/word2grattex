#' Convert a Grattan report-style Word document to .tex
#'
#' @param path The path of the folder containing your Word document.
#' @param overwrite Overwrite an existing .tex file.
#' @param downloadGrattex Download the most recent version of Grattex. Set to TRUE if the Word document is not located within a Grattex folder.
#' @param removeReport.tex Delete the default Grattex .tex file.
#' @param bibReplace Add in-text citations using bib2grattex.
#' @param buildFigures Automatically build figure environments with titles, units, \\includegraphics, notes and sources.
#' @param buildTables Automatically build table environments to enable table cross-referencing. Note that this does _not_ build the table itself.
#' @param crossReferences Automatically create cross-references. Eg: “See Section 2.2” to "See \\Cref{subsec:section-name}".
#' @param footnoteNewLine Create a new line for each footnote in .tex
#' @param segmented Set to TRUE if this is just a chapter or section of a larger document.
#' @param testRun To test new features of word2grattex. Leave as FALSE.
#'
#' @importFrom readr read_lines write_lines
#' @importFrom stringr str_split
#' @importFrom utils download.file unzip
#'
#' @export
#'
#'



word2grattex <- function(path = ".",
                         overwrite = TRUE,
                         downloadGrattex = FALSE,
                         removeReport.tex = TRUE,
                         bibReplace = TRUE,
                         buildFigures = TRUE,
                         buildTables = TRUE,
                         crossReferences = TRUE,
                         footnoteNewLine = TRUE,
                         segmented = FALSE,
                         testRun = FALSE
                         ) {


# Set up and pre-run checks ---------------------------------------------------------------------------------

# Set working directory and store current to give back
  current_wd <- getwd()
  setwd(normalizePath(path.expand(path)))
  on.exit(setwd(current_wd))


# Search for Word document
  message(paste0("Looking for Word document in ", path))

  file.docx <- dir(path = ".",
                   pattern = "\\.docx$",
                   full.names = TRUE)

  # Issue error if more than one Word doc found
  if (length(file.docx) != 1) {
    if (length(file.docx) == 0) {
      stop("No .docx files found in `path`. I've got to have something to convert!")
    } else {
      if (any(startsWith(basename(file.docx), "~$"))) {
        warning(".docx file found starting with '~$'. ",
                "Likely reason: file is open in Word. ",
                "Close the file and try again.")
      }
      stop("Multiple .docx files found in `path` (I don't know which one to choose!.")
    }
  }

  # Generate name for .tex file
  out_tex_file <- sprintf("%s.tex", tools::file_path_sans_ext(file.docx))

  # If bibReplace is requested, make sure there is a bib file
  isBibHere <- dir(path = ".", pattern = "\\.bib$", full.names = TRUE)

  if (bibReplace & identical(isBibHere, character(0))) {
    stop(paste0("Oop -- you asked for bibReplace, but I can't find a .bib file in ", path))
    }


  # Check if .tex file already exists and replace is permitted
  if (!overwrite && file.exists(out_tex_file)) {
    stop("`overwrite = FALSE` but `", out_tex_file, "` is present in `path`.")
  }

  # Search for Report.tex for preamble
  if (!file.exists("Report.tex")) {
    # Download and add current Grattex preamble if not present
    download.file(url ="https://raw.githubusercontent.com/grattan/grattex/master/Report.tex", destfile = "Report.tex")
  }

# Download grattex template if required

  if (downloadGrattex) {
    message("Looking for or downloading Grattex")
    download.file(url ="https://github.com/grattan/grattex/archive/master.zip",
                  destfile = "grattex.zip" )
    # unzip the .zip file
    unzip(zipfile = "grattex.zip")
    file.remove("grattex.zip")
  }



# FUN Convert Word document to .tex using pandoc and read -----------------------------------------------------------------------

out_tex_lines <- convert_doc_to_tex(file.docx, out_tex_file)


# ---- Set construct report framework ---- #

out_tex_lines <- create_preamble(out_tex_file, isSegmented = segmented)


# ---- Clean up pandoc conversion annoyances ----------------------------------------

out_tex_lines <- clean_up_pandoc(out_tex_file)


# TO BE REFACTORED Replacing in-text citations in bib -------------------------------------------------
# needs to be refactored!
    ## This needs to be done before Figure references
    ## to ensure external citation figure references
    ## are included withtin \textcite[][figure~1.1]{...}
    tomaster.tex <- paste0("grattex-master/", substring(out_tex_file, 3))

    if (bibReplace) {

      message("Adding in-text citations from the .bib file")


      # Find bib file
      bib <- dir(path = ".", pattern = "\\.bib$", full.names = TRUE) %>%
             substring(., 3)


      if (downloadGrattex)  bibPath <- paste0("grattex-master/bib/", bib)

      if (!downloadGrattex) {
        if(!dir.exists("bib")) dir.create("bib")
        bibPath <- paste0("bib/", bib)
      }


      bib2grattex(bibName = bib,
                  texName = out_tex_file,
                  fromWord2grattex = TRUE)

      # Move bib file to ./bib folder
      file.copy(bib, bibPath)
      message("Your bib file has been copied to the bib/ folder")


      # Read updated .tex file post-bib2grattex
      message("Reading .tex file lines back in after bibReplacement")
      out_tex_lines <- read_lines(out_tex_file)


    }






# ---- Build Figure environments ------------------------------------------------------------------------

# If we're not building figures, comment out:
if (!buildFigures) {
  message("Not building figure environments")
  out_tex_lines <- gsub("\\\\includegraphics", "\\%\\%\\\\includegraphics", out_tex_lines)
}


if (buildFigures) out_tex_lines <- build_figure_environments(out_tex_file, haveDownloadedGrattex = downloadGrattex)


# Build Table environments ---------------------------------------------------------------------------------------

if (buildTables) out_tex_lines <- build_table_environments(out_tex_file)

# Cross-references ----------------------------------------------------------------------------------------------------------------

if (crossReferences) {
  replace_crossreferences(out_tex_file)
}


# Footnote new line ----------------------------------------------------------------------------------------------------------------

if (footnoteNewLine) {
  add_footnote_newline(out_tex_file)
}


# Testing ground ----------------------------------------------------------------------------------------------------------------

  if (testRun) {
    out_tex_lines <- run_testing_ground(out_tex_lines)
  }

# ---- Write file and place files in appropriate folders ---- #

  # Move to grattex folder if grattex folder was downloaded
  if (downloadGrattex) {

    file.copy(out_tex_file, file.path(tomaster.tex))
    file.remove(out_tex_file)

    if (removeReport.tex) file.remove("./grattex-master/Report.tex")

  }

  if (!downloadGrattex)  tomaster.tex <- paste0(substring(out_tex_file,3))


  # Print over-the-top completion message
  for (x in 1:10) {
    space <- paste0(rep(" ", x), collapse = "")
  message(paste0(space, "~Conversion complete~", collapse = ""))
  }

  message("~Wooh!~")

  for (x in 10:1) {
    space <- paste0(rep(" ", x), collapse = "")
    message(paste0(space, "~Conversion complete~", collapse = ""))
  }


  # Be kind: set working directory back to normal
  setwd(current_wd)

}
