#' Check bib duplicates
#'
#' @param bibPath Path to the bib file
#' @param authorDistance A number between 0 and 1 for the loose (0) or tighter (1) match on authors (default is 0.4)
#' @param titleDistance  A number between 0 and 1 for the loose (0) or tighter (1) match on titles (default is 0.2)
#' @param matchMethod "jw" is the default. See https://www.rdocumentation.org/packages/stringdist/versions/0.9.5.1/topics/stringdist-metrics for extensive options.

#'
#' @importFrom readr read_lines write_lines
#' @import dplyr
#' @importFrom stringdist amatch
#' @importFrom purrr map map2
#'
#' @export




check_bib_duplicates <- function(bibPath,
                                 authorDistance = 0.4,
                                 titleDistance = 0.2,
                                 matchMethod = "jw"
                         ) {

bibFile <- readr::read_lines(bibPath)

# Get titles and authors
entry_lines   <- grep("\\@[a-zA-Z]*\\{", bibFile)
end_entry_lines <- c(entry_lines[2:length(entry_lines)]-1, length(bibFile))

entry_keys   <- gsub("\\@[a-zA-Z]*\\{(.*)", "\\1", bibFile[entry_lines])
entry_keys   <- gsub(",", "", entry_keys)


get_all_authors <- function(x) {

  entry <- bibFile[entry_lines[x]:end_entry_lines[x]]
  author <- entry[grep("\\s(author)\\s*\\=", entry)]
  author <- gsub("\\s*author\\s*=\\s*", "", author)
  author <- gsub("\\{", "", author)
  author <- gsub("\\}", "", author)
  author <- gsub("\\\\", "", author)
  author <- gsub(",", "", author)

  if(identical(author, character(0))) return("")

  return(author)
}

get_all_titles <- function(x) {

  entry <- bibFile[entry_lines[x]:end_entry_lines[x]]
  title  <- entry[grep("\\s(title)\\s*\\=", entry)]
  title <- gsub(".*\\{\\{?(.*?)\\}\\}?", "\\1", title)
  title <- gsub(",", "", title)

  if(identical(title, character(0))) return("")
  return(title)
}

get_all_years <- function(x) {

  entry <- bibFile[entry_lines[x]:end_entry_lines[x]]
  year  <- entry[grep("\\s(year|date)\\s*\\=", entry)]
  year <- gsub(".*\\{\\{?(.*?)\\}\\}?", "\\1", year)
  year <- gsub(",", "", year)

  if(identical(year, character(0))) return("")
  return(year)
}

entr <- entry_lines
keys <- entry_keys
auth <- map(1:length(entry_lines), get_all_authors) %>% unlist()
titl <- map(1:length(entry_lines), get_all_titles) %>% unlist()
year <- map(1:length(entry_lines), get_all_years) %>% unlist()

com <- cbind(entry = entr,
             key = keys,
             author = auth,
             title = titl,
             year = year)


# Compare each author + title with every other author + title
compare_author_title <- function(x, y) {

  ax <- com[x, 3]
  ay <- com[y, 3]


  author_dup <- stringdist::amatch(ax, ay, maxDist = authorDistance, method = matchMethod)
  if(is.na(author_dup)) author_dup <- F


  tx <- com[x, 4]
  ty <- com[y, 4]
  title_dup <- stringdist::amatch(tx, ty, maxDist = titleDistance, method = matchMethod)
  if(is.na(title_dup)) title_dup <- F


  yx <- com[x, 5]
  yy <- com[y, 5]
  year_dup  <- substr(yx, 1, 4) == substr(yy, 1, 4)



  kx <- com[x, 2]
  ky <- com[y, 2]
  if(author_dup & title_dup & year_dup) {
    mes <- paste0("\nBib entry ", kx, " with\n\tauthor:",
                  "\n\t\t", ax, " \n\tin:\t", yx,
                  "\n\twith title:\n\t\t", tx,
                  "\n\tat line\t", entry_lines[x],
                  " of your .bib file",
                  "\n",
                  "\nlooked the same as the bib entry ",  ky, ", with author:",
                  "\n\t\t", ay,  " \n\tin:\t", yy,
                  " \n\twith title:\n\t\t", ty,
                  "\n\tat line\t", entry_lines[y])

    stop(mes, call. = F)
  }
}


compare_all_author_title <- function(x) {
  y = (x + 1):nrow(com)
  map2(x, y, compare_author_title)
}

results <- map(1:length(entry_lines), compare_all_author_title)


}
