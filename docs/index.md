--- 
title: "OII Gaming Study"
author: "Niklas Johannes, Matti Vuorre, Andrew Przybylski"
date: "2020-11-10"
site: bookdown::bookdown_site
documentclass: book
bibliography: []
biblio-style: apalike
link-citations: yes
description: ""
---

# Preface

Data analyses related to "Video game play is positively correlated with well-being" (Johannes, Vuorre, Przybylski, in prep)

Preprint: <>

## Raw data

The raw data are in zip compressed files at <https://osf.io/fev95/>. Those files were minimally processed to e.g. remove test survey sessions, as described in the following sections. 

## Analyses and reproducibility

The data analyses are organized into separate [R Markdown](https://rmarkdown.rstudio.com/) files for processing, describing, and modelling. The source code is on GitHub: <https://github.com/digital-wellbeing/gametime>. Those source files are organized as a R [bookdown](https://bookdown.org/yihui/bookdown/) project, whose results are at <https://digital-wellbeing.github.io/gametime>.

To reproduce our analyses, download the source files and build the book (e.g. in RStudio click the "Build Book" button). The results are rendered to `docs/index.html` and can be viewed in a web browser. 
