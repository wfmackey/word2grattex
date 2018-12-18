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

render(markobj, output = "html_document")
?render
browseURL("test.html")
