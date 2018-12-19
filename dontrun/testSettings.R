library(dplyr)
library(grattanReporter)
library(readr)
library(stringr)
library(tools)

# Test settings

  # word2grattex
    path = "./dontrun/data/test_retirement"
    overwrite = TRUE
    downloadGrattex = FALSE
    removeReport.tex = FALSE
    bibReplace = FALSE
    buildFigures = TRUE
    crossReferences = TRUE
    segmented = FALSE
    sobSectionName = ""
    testRun = FALSE


  # bib2grattex
    # path = "."
    bibName = "mybib"
    texName = "mapping18"
    fromWord2grattex = TRUE
    noBrackets = FALSE
    titleLength = 20
    variousYears     = c("multiple years", "Various years", "various years")
    ibid = TRUE
    testRun = FALSE
