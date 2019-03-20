#' Run the 'testing ground' functions in word2grattex
#'
#' @param tex_file The path to the .tex file
#'
#'
#' @export
#'


run_testing_ground <- function(tex_file) {
  
  # Read
   out_tex_lines <- readr::read_lines(tex_file)
  
  
  # ---- Create a new line when a footnote ENDS A SENTENCE ---- #
  
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
  
  
  # Write
  readr::write_lines(out_tex_lines, tex_file)
  
}