# Learn how to produce report in RMarkdown from within the function
a <- 1:5
markobj <- c('---',
             'title: "test"',
             'output: html_document',
             '---',
             '',
             '## R Markdown',
             '',
             'This is an R Markdown document.',
             '```{r}',
             'b <- 11:15',
             'print(a)',
             'print(b)',
             '```')

markdown::markdownToHTML(text = knitr::knit(text = markobj), output = 'test.html')

rmarkdown

browseURL("test.html")
