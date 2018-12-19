

setwd("~/Google Drive/git/word2grattex")
# Test run

devtools::load_all()

path = "./dontrun/data/test_retirement"

word2grattex(path = path, bibReplace = FALSE)

