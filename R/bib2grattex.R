#' Convert Word in-text citations to LaTeX \Cref citations using a .bib file.
#'
#' @param path The path of the folder containing your Word document.
#' @param bibName The name of the .bib file containing all bib entries.
#' @param texName The name fo the .tex file containing in-text citations to be replaced.
#' @param fromWord2grattex TRUE if the bib2grattex function is nested within word2grattex.
#' @param titleLength The number of characters of the document title to be used in the reference key. The reference key is formatted as: AuthorYearTitle. Eg. Norton2018Droppingoutthecostsa when titleLength = 20 (the default).
#' @param variousYears Phrases that have been used to denote various years (in place of a numerical year within the in-text citation).
#' @param ibid Search for instances of 'ibid' and replace citation with most recent citation. This _only_ works when "ibid" is in the same paragraph as the most recent citation.
#' @param testRun To test new features of word2grattex. Leave as FALSE.
#' @examples
#' \dontrun{
#' library(word2grattex)
#' bib2grattex(path = path/to/worddoc/folder, bibName = "mybib.bib", texName = "myReport.tex")
#' }
#' @export

# This function requires the yearcode of the bib file to MATCH the year code of the Word reference list.
  # this isn't _really_ ideal; but is the system for the time being.
# The function requires proper testing, which has yet to be done.

bib2grattex <- function(path = ".",
                        bibName,
                        texName,
                        fromWord2grattex = FALSE,
                        noBrackets = FALSE,
                        titleLength = 20,
                        variousYears     = c("multiple years", "Various years", "various years"),
                        ibid = TRUE,
                        testRun = FALSE
                        ) {
print(6)
  # Tidy path and bib if needed
  path = gsub("\\/$", "" , path)
  if (!grepl("\\.bib$", bibName)) bibName = paste0(bibName, ".bib")
  if (!grepl("\\.tex$", texName)) texName = paste0(texName, ".tex")
print(7)

  # Preserving user's working directory
  if (!fromWord2grattex) {
  current_wd <- getwd()
  setwd(normalizePath(path.expand(path)))
  on.exit(setwd(current_wd))
  }

print(8)
print(getwd())
print(bibName)
  # Lint the .bib file and read in lines
  lint_bib(bibName)
print(9)
  bib.all <- read_lines(bibName)
print(10)

  # Remove existing key and create temporary unique ID
  tempkey = 0
  for (bib in grep("^\\@", bib.all)) {
    tempkey = tempkey +1
  bib.all[bib] <- gsub("(\\@[a-zA-Z]*\\{).*", paste0("\\1",tempkey), bib.all[bib])
  }


  # Create dataframe and sort by AUTHOR YEAR TITLE
  ## first: tag each bib with a number
  bibNumber <-  cumsum(grepl("^\\@", bib.all))
  bibTotal  <- length(unique(bibNumber)) # 'there are this many bibs'

  # For each of those bibs, store the YEAR (or 9999 if missing) and AUTHOR (or editor if no author; error if neither)

      ## Year
      for (bib in 1:bibTotal) {

        ### Find
        thisYear <- gsub(paste0(".*year.*?\\{([0-9]{2,4})\\}\\,"), "\\1", bib.all[(bibNumber == bib & grepl("^\\s*year\\s*\\=", bib.all) == TRUE)])

        ### Fix if 'various years' or variant
        for (x in 1:length(variousYears)) {
          thisYear  <- gsub(paste0(".*year.*?\\{(([0-9]{2,4})|(", variousYears[x] ,"))\\}\\,"), "\\1", thisYear)
        }

        ### if a year isn't found, set thisYear to "9999"
        if (identical(thisYear, character(0))) {
          thisYear = "9999"
          print(paste0("Year not found for bib ", bib, ", using 9999 instead"))
          }

        ### Add to year list
        if (bib == 1) {
          yearList <- thisYear
        } else {
          yearList <- c(yearList, thisYear)
        }
      }

      ## authorList
      for (bib in 1:bibTotal) {

          # Find. Note that this will IGNORE the first set of braces, but include a second
          thisAuthor      <- gsub(paste0(".*author.*?\\{?\\{([^\\}]*)\\}?\\}\\,"), "\\1", bib.all[(bibNumber == bib & grepl("^\\s*author\\s*\\=", bib.all) == TRUE)])

          # If there are no authors, look for editors (Word uses editors for in-text ciation if no author is present)
          if (identical(thisAuthor, character(0))) {
            thisAuthor <- gsub(paste0(".*editor*?\\{([^\\}]*\\}?)\\}\\,"), "\\1", bib.all[(bibNumber == bib & grepl("^\\s*editor\\s*\\=", bib.all) == TRUE)])
            if (!identical(thisAuthor, character(0))) print(paste0("Author not found for bib ", bib, ", using editor instead"))

            # If no author and no editor, use title (the case for Acts)
            if (identical(thisAuthor, character(0))) {
              thisAuthor <- gsub(paste0(".*title*?\\{([^\\}]*\\}?)\\}\\,"), "\\1", bib.all[(bibNumber == bib & grepl("^\\s*title\\s*\\=", bib.all) == TRUE)])
            }
            if (!identical(thisAuthor, character(0))) print(paste0("Author not found for bib ", bib, ", using title instead (is this an Act? If not, there should be an author or editor)"))
          }


          # Add to authorList
          if (bib == 1) {
            authorList <- thisAuthor
          } else {
            authorList <- c(authorList, thisAuthor)
          }


      }



  # Generate 'findAuthor' options based on the number/type of author
  findAuthor <- authorList
  authorKey  <- authorList

    ## Originally single brace NO commas can be left as-is; "ABS", "Treasury", "Department of Education and Training"
    findAuthor <- findAuthor

      ### authorKey: "ABS", "Treasury", "DepartmentofEducationandTraining"  // retain proper capitalisation?
      authorKey <- ifelse( str_count(authorKey, ",") == 0, gsub("\\s", "", authorKey), authorKey)


    ## Originally single brace ONE comma; extract the author last-name: "Norton"
    findAuthor <- ifelse( str_count(findAuthor, ",") == 1,
                          gsub("^([^,]*),.*", "\\1", findAuthor),
                          findAuthor)

      ### authorKey: "Norton"
      authorKey <- ifelse(str_count(authorKey, ",") == 1, gsub("^([^,]*),.*", "\\1", authorKey), authorKey)


    ## Originally single brace TWO commas; extract the two authors' last names separated by "and": "Norton and Cherastidtham"
    findAuthor <- ifelse( str_count(findAuthor, ",") == 2,
                          gsub("^([^,]*),.*and\\s([^,]*).*", "\\1 and \\2", findAuthor),
                          findAuthor)

      ### authorKey: "Norton"
      authorKey <- ifelse(str_count(authorKey, ",") == 2,
                          gsub("^([^,]*),.*and\\s([^,]*).*", "\\1", authorKey),
                          authorKey)

    ## you can call me et al
        ### Originally single brace THREE or more commas; extract the first author last-name and add `et al.`: "Norton, et al." [variant 1]
        findAuthor1 <- ifelse( str_count(findAuthor, ",") > 2,
                               gsub("^([^,]*),.*", "\\1, et al\\.", findAuthor),
                               findAuthor)

        ### Originally single brace THREE or more commas; extract the first author last-name and add `et al.`: "Norton et al." [variant 2]
        findAuthor2 <- ifelse( str_count(findAuthor, ",") > 2,
                               gsub("^([^,]*),.*", "\\1 et al\\.", findAuthor),
                               findAuthor)

        ### Originally single brace THREE or more commas; extract the first author last-name and add italicised `et al.`: "Norton\\\\emph\\{, et al.}" [variant 3]
        findAuthor3 <- ifelse( str_count(findAuthor, ",") > 2,
                               gsub("^([^,]*),.*", "\\1\\\\emph{, et al.}", findAuthor),
                               findAuthor)


        ### ALT: Originally single brace THREE commas; extract the first three author names: "Norton, Cherastidtham and Mackey [variant 4]
        findAuthor4 <- ifelse( str_count(findAuthor, ",") == 3,
                               gsub("^([^,]*),.*and\\s([^,]*).*and\\s([^,]*).*", "\\1, \\2 and \\3", findAuthor),
                               findAuthor)

        ### ALT: Originally single brace FOUR commas; extract the first four author names: "Norton, Cherastidtham, Goss and Mackey [variant 4]
        findAuthor5 <- ifelse( str_count(findAuthor, ",") == 4,
                               gsub("^([^,]*),.*and\\s([^,]*).*and\\s([^,]*).*and\\s([^,]*).*", "\\1, \\2, \\3 and \\4", findAuthor),
                               findAuthor)

        ### ALT: Originally single brace FIVE commas; extract the first five author names: "Norton, Cherastidtham, Goss, Chivers and Mackey [variant 4]
        findAuthor6 <- ifelse( str_count(findAuthor, ",") == 5,
                               gsub("^([^,]*),.*and\\s([^,]*).*and\\s([^,]*).*and\\s([^,]*).*and\\s([^,]*).*", "\\1, \\2, \\3, \\4 and \\5", findAuthor),
                               findAuthor)

        ### authorKey: "Norton"
        authorKey <- ifelse(str_count(authorKey, ",") > 2,
                            gsub("^([^,]*),.*", "\\1", authorKey),
                            authorKey)

        ## Remove spaces and punctuation from author key
        authorKey <- gsub("[[:punct:]]", "", authorKey)
        authorKey <- gsub("\\s", "", authorKey)


  # Retrieve title for sorting and key generation
      for (bib in 1:bibTotal) {

        # Find. Note that this will IGNORE the first set of braces, but include a second
        thisTitle      <- gsub(paste0(".*title.*\\=.*?\\{(.*)\\},"), "\\1", bib.all[(bibNumber == bib & grepl("^\\s*title\\s*\\=", bib.all) == TRUE)])

        # If there is no title, send warning and set to ""
        if (identical(thisTitle, character(0))) {
          thisTitle <- ""
          print(paste0("Title not found for bib ", bib, "!"))
        }

        # Add to authorList
        if (bib == 1) {
          titleList <- thisTitle
        } else {
          titleList <- c(titleList, thisTitle)
        }


      }

      # Create findCitation
      findCitation <- tibble(
                              "fullAuthor"   = authorList,
                              "shortAuthor"  = findAuthor1,
                              "authorKey"    = authorKey,
                              "findAuthor1"  = findAuthor1,
                              "findAuthor2"  = findAuthor2,
                              "findAuthor3"  = findAuthor3,
                              "findAuthor4"  = findAuthor4,
                              "findAuthor5"  = findAuthor5,
                              "findAuthor6"  = findAuthor6,
                              "title"        = titleList,
                              "year"         = yearList,
                              "bibNumber"    = unique(bibNumber)    # BEFORE arrange
        ) %>%

        # Sort order: AUTHOR, YEAR, TITLE  (as seen in docx formatting)
        arrange(shortAuthor, year, title) %>%
        # Count number of obbservations by shortAuthor+year and generate yearCode
        group_by(shortAuthor, year) %>%
        mutate(
          count = seq(n()),
          total = n(),
          tempCode  = paste0(letters[count]),
          yearCode = case_when(total == 1 ~  paste0(year),
                               total  > 1 &  grepl("[0-9]{2,5}", year) ~ paste0(year, tempCode),
                               total  > 1 & !grepl("[0-9]{2,5}", year) ~ paste0(year, "-", tempCode) # exception for "multiple years" etc
                              ),
        # Create findCite1:6 variables
          findCitation1 = paste0(findAuthor1, " (", yearCode, ")"),
          findCitation2 = paste0(findAuthor2, " (", yearCode, ")"),
          findCitation3 = paste0(findAuthor3, " (", yearCode, ")"),
          findCitation4 = paste0(findAuthor4, " (", yearCode, ")"),
          findCitation5 = paste0(findAuthor5, " (", yearCode, ")"),
          findCitation6 = paste0(findAuthor6, " (", yearCode, ")"),

          # And create findCiteBrackets1:6 variables according to Cite Them Right 10th Edition, Harvard
          findCitationBrackets1 = paste0("(", findAuthor1, ", ", yearCode, ")"),
          findCitationBrackets2 = paste0("(", findAuthor2, ", ", yearCode, ")"),
          findCitationBrackets3 = paste0("(", findAuthor3, ", ", yearCode, ")"),
          findCitationBrackets4 = paste0("(", findAuthor4, ", ", yearCode, ")"),
          findCitationBrackets5 = paste0("(", findAuthor5, ", ", yearCode, ")"),
          findCitationBrackets6 = paste0("(", findAuthor6, ", ", yearCode, ")"),

        # Create Key variable
          shortTitle = substr(title, 1, titleLength) %>%
                        gsub("[[:punct:]]", "", .) %>%
                        gsub("\\s", "", .) %>%
                        tolower(.),

          citeKey    = case_when(grepl("[0-9]{2,5}", year) ~ paste0(authorKey, year, shortTitle),
                                !grepl("[0-9]{2,5}", year) ~ paste0(authorKey, shortTitle)), # exception for "multiple years" etc

        # Create Replace variable
          replaceCitation = paste0("\\textcite[][]{", citeKey, "}")
        ) %>%

        # Re-arrange according to bibNumber
        ungroup() %>%
        arrange(bibNumber)


        # Pull required vectors
        citeKey <- pull(findCitation, citeKey)
        find1   <- pull(findCitation, findCitation1)
        find2   <- pull(findCitation, findCitation2)
        find3   <- pull(findCitation, findCitation3)
        find4   <- pull(findCitation, findCitation4)
        find5   <- pull(findCitation, findCitation5)
        find6   <- pull(findCitation, findCitation6)
        findbrackets1   <- pull(findCitation, findCitationBrackets1)
        findbrackets2   <- pull(findCitation, findCitationBrackets2)
        findbrackets3   <- pull(findCitation, findCitationBrackets3)
        findbrackets4   <- pull(findCitation, findCitationBrackets4)
        findbrackets5   <- pull(findCitation, findCitationBrackets5)
        findbrackets6   <- pull(findCitation, findCitationBrackets6)
        replace <- pull(findCitation, replaceCitation)

        # Replace/add citation key to .bib file for each bib entry
        for (bib in 1:bibTotal) {
          bib.all[bibNumber == bib] <- gsub("(\\@[a-zA-Z]*\\{)[0-9]*", paste0("\\1",citeKey[bib], ","), bib.all[bibNumber == bib])
        }

        # Write updated bib file
        write_lines(bib.all, bibName)



# ---- Replace in-text citations ---- #
        # if (fromWord2grattex)  texFile <- out_tex_lines
        if ( fromWord2grattex) texFile <- read_lines("outtex.tex")
        if (!fromWord2grattex) texFile <- read_lines(texName)


        citationSearch <- function(findNumber, brackets) {

          if (number != 1 & !brackets) message(paste0("Replacing ",      get(paste0("find", findNumber))[bib], " with ", replace[bib]))
          if (number == 1 |  brackets) message(paste0("Also replacing ", get(paste0("find", findNumber))[bib], " with ", replace[bib]))

          texFile <- gsub(get(paste0("find", findNumber))[bib], replace[bib], texFile, fixed = TRUE)

        }

        for (bib in 1:bibTotal) {
          message(paste0("Replacing ", find1[bib], " with ", replace[bib]))
          texFile <- gsub(find1[bib], replace[bib], texFile, fixed = TRUE)

          if (find1[bib] != find2[bib]) {
            message(paste0("Replacing ", find2[bib], " with ", replace[bib]))
            texFile <- gsub(find2[bib], replace[bib], texFile, fixed = TRUE)
          }

          if (find2[bib] != find3[bib]) {
            message(paste0("Replacing ", find3[bib], " with ", replace[bib]))
            texFile <- gsub(find3[bib], replace[bib], texFile, fixed = TRUE)
          }

          if (find3[bib] != find4[bib]) {
            message(paste0("Replacing ", find4[bib], " with ", replace[bib]))
            texFile <- gsub(find4[bib], replace[bib], texFile, fixed = TRUE)
          }

          if (find4[bib] != find5[bib]) {
            message(paste0("Replacing ", find5[bib], " with ", replace[bib]))
            texFile <- gsub(find5[bib], replace[bib], texFile, fixed = TRUE)
          }

          if (find5[bib] != find6[bib]) {
            message(paste0("Replacing ", find6[bib], " with ", replace[bib]))
            texFile <- gsub(find6[bib], replace[bib], texFile, fixed = TRUE)
          }
        }


        # ---- ibid functionality ---- #

if (ibid) {
  # Two processes:
    ## one line process (via simple gsub)
  texFile <- gsub("(.*textcite.*?\\{([a-zA-Z0-9]*)\\}.*?)(?:i|I)bid\\.?",
                  "\\1\\\\textcite[][]\\{\\2\\}",
                  texFile)

    ## multi-line process (harder --> might need to number)



}

    # ---- Managing textcites; creating footcites ---- #

        # Remove \emph from texcites in footnotes: if \emph surrounds a \textcite, remove it
        texFile <- gsub("\\\\emph\\{(\\\\textcite\\[\\]\\[\\]\\{[^\\}]*\\})\\}",
               "\\1",
               texFile)


        # Add page(s), section and chapter references to \textcite
        # p. num  WORKS WELL
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?\\(?p\\.?\\s?([0-9]{1,6})\\)?",
        				        "\\\\textcite\\[\\]\\[\\2\\]\\{\\1\\}",
        				        texFile)

        # p. letter; need to be more cautious so enforcing p. or p\\s
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?\\(?p[\\.\\s]{1,2}\\s?([a-zA-Z]{1,6})\\)?",
                        "\\\\textcite\\[\\]\\[\\2\\]\\{\\1\\}",
                        texFile)


        # pp. num-num   WORKS WELL
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?\\(?pp\\.?\\s?([0-9]{1,6})\\-\\-?([0-9]{1,6})\\)?",
                        "\\\\textcite\\[\\]\\[\\2--\\3\\]\\{\\1\\}",
                        texFile)

        # pp. letter-letter   WORKS WELL
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?\\(?pp\\.\\s?([a-zA-Z]{1,6})\\-\\-?([a-zA-Z]{1,6})\\)?",
                        "\\\\textcite\\[\\]\\[\\2--\\3\\]\\{\\1\\}",
                        texFile)


        # Single chapter, section, appendix, table, figure, chart   WORKS WELL
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(C|c)hapter\\s([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[chapter~\\3]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(S|s)ection\\s([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[section~\\3]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(A|a)ppendix\\s([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[appendix~\\3]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(T|t)able\\s([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[table~\\3]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(F|f)igure\\s([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[figure~\\3]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(C|c)hart\\s([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[chart~\\3]\\{\\1\\}", texFile)

        # Ranges chapter, section, appendix, table   NOT YET TESTED properly
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(C|c)hapters\\s([0-9\\.]{1,8})\\s?([andto,\\-]{1,5})\\s?([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[chapters~\\3~\\4~\\5]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(S|s)ections\\s([0-9\\.]{1,8})\\s?([andto,\\-]{1,5})\\s?([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[sections~\\3~\\4~\\5]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(A|a)ppendices\\s([0-9\\.]{1,8})\\s?([andto,\\-]{1,5})\\s?([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[appendices~\\3~\\4~\\5]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(T|t)ables\\s([0-9\\.]{1,8})\\s?([andto,\\-]{1,5})\\s?([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[tables~\\3~\\4~\\5]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(F|f)igures\\s([0-9\\.]{1,8})\\s?([andto,\\-]{1,5})\\s?([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[figures~\\3~\\4~\\5]\\{\\1\\}", texFile)
        texFile <- gsub("\\\\textcite\\[\\]\\[\\]\\{([^\\}]*)\\},?\\s?(C|c)harts\\s([0-9\\.]{1,8})\\s?([andto,\\-]{1,5})\\s?([0-9\\.]{1,8})",
                        "\\\\textcite\\[\\]\\[charts~\\3~\\4~\\5]\\{\\1\\}", texFile)



# ----- Collapse \textcites (logical that this comes before \footcite replacement) ---- #

    ## Define a single textcite match
    findTextciteOneTwo <-    "\\\\textcite(\\[[a-zA-Z0-9\\-\\~\\.]*\\]\\[[a-zA-Z0-9\\-\\~\\.]*\\]\\{[a-zA-Z0-9\\-]*\\})"
    findTextciteLater  <- "(?:\\\\textcite(\\[[a-zA-Z0-9\\-\\~\\.]*\\]\\[[a-zA-Z0-9\\-\\~\\.]*\\]\\{[a-zA-Z0-9\\-]*\\}))?"

    # Combine; looking for TWO or more consecutive textcites separated by [; , and]
    texFile <-  gsub(paste0(findTextciteOneTwo, "[\\s\\;\\,and]*",
                            findTextciteOneTwo, "[\\s\\;\\,and]*",
                            findTextciteLater,  "[\\s\\;\\,and]*",
                            findTextciteLater,  "[\\s\\;\\,and]*",
                            findTextciteLater,  "[\\s\\;\\,and]*",
                            findTextciteLater,  "[\\s\\;\\,and]*",
                            findTextciteLater,  "[\\s\\;\\,and]*",
                            findTextciteLater,  "[\\s\\;\\,and]*",
                            findTextciteLater,  "[\\s\\;\\,and]*"),
                    "\\\\textcites\\1\\2\\3\\4\\5\\6\\7\\8\\9",
                    texFile, perl = TRUE)


# ----- # Replace \footnote{\textcite[][]{...}} with \footcite[][]{...} ---- #
    findCitePatternFirst <- "(\\[[a-zA-Z0-9\\-\\~\\.]*\\]\\[[a-zA-Z0-9\\-\\~\\.]*\\]\\{[a-zA-Z0-9\\-]*\\})"
    findCitePatternLater <- "(\\[[a-zA-Z0-9\\-\\~\\.]*\\]\\[[a-zA-Z0-9\\-\\~\\.]*\\]\\{[a-zA-Z0-9\\-]*\\})*"

    texFile <- gsub(paste0("\\\\footnote\\{\\\\textcite(s?)",
                           findCitePatternFirst,
                           findCitePatternLater,
                           "\\s?\\.?\\s?\\}"),
                    "\\\\footcite\\1\\2\\3",
                    texFile, perl = TRUE)




# ----- # Write tex file with updated bib citations ---- #
    if ( fromWord2grattex) write_lines(texFile, "outtex.tex")
    if (!fromWord2grattex) write_lines(texFile, paste0(path,"/", texName))



message("End of bib2grattex")

}   # end function

