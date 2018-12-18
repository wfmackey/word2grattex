library(dplyr)
library(grattanReporter)
library(readr)
library(stringr)
library(tools)

# Test settings

  # word2grattex
    path = "./dontrun/data/test_w2g"
    overwrite = TRUE
    downloadGrattex = FALSE
    removeReport.tex = TRUE
    bibReplace = TRUE
    buildFigures = TRUE
    crossReferences = TRUE
    segmented = FALSE
    sobSectionName = ""
    testRun = FALSE


  # bib2grattex

    path = "."
    bibName =
    texName =
    fromWord2grattex = FALSE
    noBrackets = FALSE
    titleLength = 20
    variousYears     = c("multiple years", "Various years", "various years")
    ibid = TRUE
    testRun = FALSE
