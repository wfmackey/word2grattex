#' Use pandoc to convert a Word document to a .tex file.
#'
#' @param file.docx The path to the .docx file to be converted.
#' @param tex_file The desired path of the exported .tex file
#' 
#' @importFrom readr read_lines
#'
#' @export
#'

convert_doc_to_tex <- function(file.docx,
                               tex_file) {
  


  # Check for pandoc
  if (!nzchar(Sys.which("pandoc"))) {
    stop("pandoc not found on the system path. See https://pandoc.org/installing.html to download it.")
  }

  # Convert using pandoc via bash
  message(paste0("Converting .docx to .tex using Pandoc"))
  
  if (tolower(.Platform$OS.type) == "windows") {
    
    shell(sprintf("pandoc --wrap=none --top-level=chapter -s %s -o %s", file.docx, tex_file))
    
  } else {
    
    system(sprintf("pandoc --wrap=none --top-level=chapter -s %s -o %s", file.docx, tex_file))
  }
  
  
  # Read lines from pandoc output
  message(paste0("Reading .tex lines for processing"))
  out_tex_lines <- readr::read_lines(tex_file)
  
  return(out_tex_lines)
}

