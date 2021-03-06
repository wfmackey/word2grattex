# word2grattex

`word2grattex` is an `R` packaged deisgned to make Word to LaTeX conversion faster and more accurate. It is relatively flexible but is most effective when used within the wider Grattan report production ecosystem.

Importantly: because of unpredictable (and usually accidental) Word formatting choices, this tool is certainly more of an _art_ than it is a _science_. For example, there may be loose brackets or braces in your post-conversion document that you'll need to tidy yourself (sorry!). 

There may also be incorrect conversions of references when using `bib2grattex` (via `word2grattex(bibReplace = TRUE)`, the default), because of 'Not Quite Twins' [see here](https://blog.apastyle.org/apastyle/2011/10/reference-twins.html_).

That being said: it will do most of the work for you, and the problems it can create are -- in 25 full conversions and counting -- fewer than the problems it solves. 

It **requires**:

* `R` and `pandoc` (which usually ships with `R`, otherwise download [here](https://pandoc.org/installing.html))
* A Word document in the Grattan report template.
    * Eg _Mapping Australian higher education 2018.docx_


It can do the **most** if it also has:

* The `.bib` file associated with the report.
    - Eg _mapping2018.bib_.
* In-text citations in Word, formatted as _The Style of Quiet Achievers_ or _Harvard Reference Format 1_. 
    - Eg _Norton & Cherastidtham (2018)_;
    - or _(Norton & Cherastidtham 2018)_.
* The `.pdf` chart deck in order of appearance in the report.
    - Eg _mapping2018.pdf_
    - Note that if this is missing, the figure environments will default to calling _chartdeck.pdf_, which can easily be manually fixed using find-replace.
* Linked or manual cross-references (not _broken_).
    - Eg _See figure 14 in section 3.4_.



### Pre-function set up: before you use it
Before you use the function, follow the steps described under **Starting a new report** on the [`Grattex` homepage](https://github.com/HughParsonage/grattex). 

Once you have created a repository for your new report, add your `.docx`, `.bib` and `.pdf` files. Your repository should look something like this, using the _Mapping Australian higher education 2018_ example:

```
./he-mapping-2018/
    - atlas
    - bib
    - doc
    - logos
    - tests
    - travis
    - Report.tex
    - mapping2018.docx
    - mapping2018.bib
    - mapping2018.pdf
    etc

```


Note that you can run the function before you set up a report repository. Just point the `word2grattex` function to any folder  that contains your `.docx` (and `.bib`, `.pdf`) files.


### Install and call: how to use it
The package is run through `R` and can be installed using `remotes::github_install`:

```R
# Install devtools if you haven't already (remove the comment):
# install.packages(remotes)

remotes::install_github("wfmackey/word2grattex")

library(word2grattex)

```

The package contains two large functions: `word2grattex` and `bib2grattex`. Note that, by default, `word2grattex` runs `bib2grattex`. To run all features described in the **what it does** section below, point `word2grattex` to a folder that contains a `.docx` document and a `.bib` file (and, preferably, a `.pdf` file containing charts. If this is absent, figure environments will be built using the default `chartpack.pdf`).

For our _Mapping Australian higher education 2018_ example, we can then run:

```R
word2grattex(path = "Dropbox (Grattan Institute)/Apps/Overleaf/mapping2018")
```

**Note: if you do not have a bib file, set bibReplace = F:**
```R
word2grattex(path = "Dropbox (Grattan Institute)/Apps/Overleaf/mapping2018",
             bibReplace = F)
```
Also, there is a yet-to-be-addressed issue with Tables that pops up. If you receive an error about tables, please set `buildTables = F`:

```R
word2grattex(path = "Dropbox (Grattan Institute)/Apps/Overleaf/mapping2018",
             bibReplace = F, buildTables = F)
```


This will produce a `.tex` file.

If there are any issues, please get in touch with Will.



### Process and output: what it does

The function takes a Word document in the Grattan template and: 

* Converts it to LaTeX using `pandoc`.
* Adds the current LaTeX preamble of the Grattan report template.
* Cleans up after the `pandoc` conversion:
    - removes `\protect`, `\hyperlink`, `\hypertarget`, `\texorpdfstring`;
    - removes empty headings.
* Adds graphics where an image was found in the Word document:
    - builds Figure environment;
    - converts Word metadata to LaTeX `\caption`, `\unit`, `\noteswithsource` (or `\notes`/`\source` only, or `\noteswithsources`, etc);
    - creates `\insertgraphics` for a standard Grattan chart size and inserts the n<sup>th</sup> page of the name of your PDF chart deck, or defaults to the n<sup>th</sup> page of `chartdeck.pdf` if a PDF is not provided (this can be quickly fixed afterwards with find/replace);
    - labels charts with `fig:figure-caption`.
* Builds Table environments.
    - _It doesn't build your tables_ sorry :(.
    - But, look, it has a proper crack at it. It will make the rows (kind of) but will use a `\longtable` format by default. i.e. it does a bit. The table will be commented out for compiling convenience, but if it's an easy table it might work (kind of) out-of-the-box.
    - See the `kableExtra` package in R, or `Excel2LaTeX` Excel plugin for assistance.
* Applies appropriate labels to chapters, sections, subsections, etc.:
    - `chap:`, `sec:`, `subsec:`, etc.
* Replaces cross-references with appropriate labels.
    - "See Section 2.2" &rarr; `See \Cref{subsec:section-name}`.
* Replaces figure-references with appropriate labels.
    - "Figure 19 shows" &rarr; `\Cref{fig:figure-caption} shows`.
* Replaces in-text citations with appropriate `cite` commands.
    - Uses `bib2grattex` function.
    - `.bib` keys are automatically generated in the format `AuthorYearTitle` (default is to cap the title to 20 characters). 
		- Handles (all-but-one) citation complications:
        - 'Norton _et al._ (2018a).' &rarr; `\footcite{Norton2018droppingoutthecostsa}`
        - 'See discussion in Terrill (2018), p. 10.' &rarr; `\footnote{See discussion in \textcite[][10]{Terrill2018unfreezingdiscountra}.}`. 
        - 'Daley and Wood (2016), chapter 1; Smith (1776e), chapter 3.' &rarr; `\footcites[][chapter~3]{Daley2018hotproperty}[][chapter~3]{Smith1776thewealthofnat}.}`. 


Note: as our _Style of Quiet Achievers_ fails to distinguish between '[not-quite twins](http://blog.apastyle.org/apastyle/2011/10/reference-twins.html)', the `bib2grattex` conversion can't tell the difference. A solution to this problem is being considered. For now, manual identification of not-quite twins is required. Mainly: **check your _Daley et al._ references**.



### Finalisation: what you do now

When `word2grattex` is finished, it will produce a `.tex` file that can be built out of the box. Some things need to be done manually:

* Check for errors.
* Add Tables.
* Add Box environments.
    - This feature can't be added because there is no way to tell when a box ends.
* Add Overview and Recommendations. Update Acknowledgements, ISBN, report number, FrontPage.
* Optimise figure placement. 
    - Use `FigurePlacementScore` to help.
* Proofread lots.

Then, run through `checkGrattan`/`Travis` and release. 



### A continually improved tool
This is a work-in-progress. You will notice errors or think something could be improved (these ideas usually come when you're doing something repetively after the conversion and think "heck I wish this could be done automatically"). 

If you do, please get in touch. This can be done by raising an issue on the `word2grattex` Github page (my preference -- it helps keep everything in one place!), or by emailing william.mackey@grattaninstitute.edu.au.
