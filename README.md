# Video game play is positively correlated with well being

Here, we report the data processing and analyses related to the paper "Video game play is positively correlated with well-being" (Johannes, Vuorre, & Przybylski, in prep). We explain all steps we took from raw data processing to final analysis and plots.

Preprint: <>

## Raw data

The raw data are in zip compressed files at <https://osf.io/fev95/>. Those files were minimally processed to e.g. remove test survey sessions, as described in the following sections. The files are excel files and have a second tab which serves as a codebook.

## Analyses and reproducibility

The data analyses are organized into separate [R Markdown](https://rmarkdown.rstudio.com/) files for processing, describing, and modelling. The source code is on GitHub: <https://github.com/digital-wellbeing/gametime>. Those source files are organized as an R [bookdown](https://bookdown.org/yihui/bookdown/) project, whose results are at <https://digital-wellbeing.github.io/gametime>.

To reproduce our analyses, download the source files from Github, open the R Project, then build the book (e.g. in RStudio click the "Build Book" button). The code in the following sections will automatically download the data from the OSF. The results are rendered to `docs/index.html`. If you double click that file, it will open a web browser, which is identical to the book on <https://digital-wellbeing.github.io/gametime>. Alternatively, you can also open each R Markdown file and run it separately (e.g., in RStudio). 
