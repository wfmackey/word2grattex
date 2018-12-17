#word2grattex

`word2grattex` is deisgned to make Word to LaTeX conversion faster and more accurate. It is relatively flexible but is most effective when used within the wider Grattan report production ecosystem.

It **requires**:

* `R` and `pandoc` (which usually ships with `R`, otherwise download [here](https://pandoc.org/installing.html))
* A Word document in the Grattan report template.

It can do the **most** if it also has:

* The `.bib` file associated with the report.
* In-text citations in Word formatted as per _The Style of Quiet Achievers_ (Note that Harvard)
* The `.pdf` chart deck in order of appearance in the report.
* Linked or manual cross-references (not _broken_, ala ). 



### Process and output: what it does

The function takes a Word document in the Grattan template and: 

* Converts it to LaTeX using `pandoc`.
* Adds the current LaTeX preamble of the Grattan report template.
* Cleans up after the `pandoc` conversion:
    * removes `\protect`, `\hyperlink`, `\hypertarget`, `\texorpdfstring`;
    * removes empty headings.
* Adds graphics where a chart is found:
    * builds Figure environment;
    * converts Word metadata to LaTeX `\caption`, `\unit`, `\noteswithsource` (or `\notes`/`\source` only, or `\noteswithsources`, etc);
    * creates `\insertgraphics` for a standard Grattan chart size and inserts the $n^{th}$ page of the name of your PDF chart deck, or defaults to the n<sup>th</sup> page of `chartdeck.pdf` if a PDF is not provided (this can be quickly fixed afterwards with find/replace);
    * labels charts with `fig:figure-caption`.
* Builds Table environments.
   * _But doesn't build your tables_ :( See the `Excel2LaTeX` Excel plugin for assistance there.
* Applies appropriate labels to chapters, sections, subsections, etc.:
    * `chap:`, `sec:`, `subsec:`, etc.
* Replaces cross-references with appropriate labels.
    * "See Section 2.2" &rarr; `See \Cref{subsec:section-name}`.
* Replaces figure-references with appropriate labels.
    * "Figure 9.1 shows" &rarr; `\Cref{fig:figure-caption} shows`.
* Replaces in-text citations with appropriate `cite` commands.
    * Uses `bbib2grattex` function.
		* `.bib` keys are automatically generated in the format `AuthorYearTitle` (default is to cap the title to 20 characters). 
		* Handles (all-but-one) citation complications:
        * 'Norton _et al._ (2018a).' &rarr; `\<fieldset></fieldset>ootcite{Norton2018droppingoutthecostsa}`
        * 'See discussion in Terrill (2018), p. 10.' &rarr; `\footnote{See discussion in \textcite[][10]{Terrill2018unfreezing-disc}.}`. 
        * 'Daley and Wood (2016), chapter 1; Smith (1776e), chapter 3.' &rarr; `\footcites[][chapter~3]{Daley2018hot-property}[][chapter~3]{Smith1776the-wealth-of-n}.}`. 


Note: as our _Style of Quiet Achievers_ fails to distinguish between '[not-quite twins](http://blog.apastyle.org/apastyle/2011/10/reference-twins.html)', the `bib2grattex` conversion tool does too. A solution to this problem is being considered. For now, manual identification of not-quite twins is required. Key point: **check your _Daley et al._ references**.



### Finalisation: what you do

When `word2grattex` is finished, it will produce a `.tex` file that can be built out of the box. Some things need to be done manually:

* Add Tables.
* Add Box environments.
* Add Overview and Recommendations. Update Acknowledgements, ISBN, report number, FrontPage.
* Optimise figure placement. 
    * Use `FigurePlacementScore` to help.
* Proofread.

Then, run through `chreckGrattan`/`Travis` and release. 


