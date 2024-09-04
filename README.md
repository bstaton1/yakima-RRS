
> This repository stores the code used to perform the GLM analyses
> presented in the manuscript *Evaluating the long-term net reproductive
> benefits and costs of supplementation in upper Yakima River Chinook
> salmon* by I.J. Koch, B.A. Staton, H.M. Nuetzel, T.R. Seamons, A.P.
> Matala, K.I. Warheit, M.V. Johnston, C.R. Strom, S.R. Narum, and W.J.
> Bosch.

[![ArticleDOI](https://img.shields.io/badge/Article-PLACEHOLDER%20IF%20ACCEPTED-blue?logo=doi&logoColor=f5f5f5)]()  
[![GitHub Repo Archive
DOI](https://img.shields.io/badge/GitHub%20Repo%20Archive-PLACEHOLDER%20WHEN%20MINTED-blue?logo=github)]()

## Data Ownership

*The data file needed to run these analyses has not yet been approved
for public sharing. Please contact the manuscript authors to receive a
copy of the data set.*

## Repo Organization

This repository contains an assortment of GLM analyses intended to
address research questions about the relative reproductive success of
spawning Chinook salmon in the upper Yakima River.

| Subdirectory         | Description                                                                                                                                     |
|:---------------------|:------------------------------------------------------------------------------------------------------------------------------------------------|
| `01-1gen-RRS`        | Compares progeny produced by HOR and NOR spawners reproducing naturally.                                                                        |
| `02a-2gen-RRS`       | Compares grand-progeny produced by HOR and NOR spawners reproducing naturally.                                                                  |
| `02b-2gen-RRS-wF1`   | Compares grand-progeny produced by HOR and NOR spawners reproducing naturally, while considering the number of progeny produced as a predictor. |
| `03-cross-type-RRS`  | Compares progeny produced by spawning pairs in which the parents are of differing origin types.                                                 |
| `04-acc-site-RRS`    | Compares progeny produced by HOR spawners reproducing naturally based on the site they were acclimated at as smolts.                            |
| `05-ancestry-RRS`    | Compares progeny produced by NOR spawners reproducing naturally based on the origin types of their parents.                                     |
| `06-1gen-demo-boost` | Compares progeny produced by NOR spawners based on where they reproduced – either naturally or in the hatchery as broodstock.                   |
| `07-2gen-demo-boost` | Compares grand-progeny produced by NOR spawners based on where they reproduced – either naturally or in the hatchery as broodstock.             |
| `08-attr-v-origin`   | Compares length and return date by origin.                                                                                                      |

Additional scripts include:

- `common-functions.R`: Stores custom functions used to streamline
  performing replicate tasks across multiple analyses.
- `run-one.R`: Script to execute one of the analyses shown in the table
  above.
- `run-all.R`: Script to execute all of the analyses shown in the table
  above, and build manuscript figures, tables, and supplemental
  material.
- `ms-content/ms-figs.R`: Script to build manuscript figures that rely
  on the output of GLM analyses.
- `ms-content/ms-tables.Rmd`: Rmarkdown to build manuscript tables.
- `ms-content/supplement`: Contains multiple Rmarkdown source code files
  for building the manuscript supplemental material.

## Reproducibility

The output files are too large to track in a git repo, so this code will
need to be executed locally to reproduce the output presented in the
manuscript and supplement.

All analyses that use random number generators (e.g., parametric
bootstrapping) include a `set.seed()` statement, so our results should
be exactly reproducible if all defaults are used.

Clone the repository, acquire the data (see *Data Ownership* above), and
navigate to the repository location on your computer.

**All Analyses At Once**

The exact output presented in the manuscript can be most easily
reproduced via command line, e.g.:

``` bash
Rscript run-all.R
```

Or from within R:

``` r
source("run-all.R")
```

This will run each of the GLM analyses in the table above sequentially,
then build the manuscript figures and tables and the supplement. When
complete, the output will be found the `ms-content/output` subdirectory,
and in the `/output` subdirectories of each analysis-specific directory.

> *The default is to use 8 CPU cores to run 1,000 bootstrap iterations
> per GLM analysis. This takes approximately 9 hours on a current modern
> laptop computer (RAM: 64GB; Processor: 20 threads, 5.40 GHz Max Turbo,
> 24 MB Cache). Similar results to those we present can be obtained with
> fewer bootstrap iterations – change the value of the `nboot` variable
> in the `run-all.R` script to, e.g., 100 to obtain results faster.*

**One-by-One**

Any of the specific GLM analyses can be executed with all defaults,
e.g.,

``` bash
Rscript run-one.R 05-ancestry-RRS
```

Or execute the script with the `-h` flag to see how to change the
defaults:

``` bash
Rscript run-one.R -h
```

**Interactively**

Of course, you can open the `yakima-RRS.Rproj` file in [RStudio
Desktop](https://posit.co/download/rstudio-desktop/) and run the scripts
in each of the subdirectories interactively to gain insights about how
the code works.

## Dependencies

The table below shows all R packages (and their versions at time of
publication) that are specifically called (typically with `pkg::fn()`)
or loaded (using `library(pkg)`).

| Package                                                       | Version | Description                                                                   |
|:--------------------------------------------------------------|--------:|:------------------------------------------------------------------------------|
| [`argparser`](https://CRAN.R-project.org/package=argparser)   |   0.7.2 | Command-Line Argument Parser                                                  |
| [`bookdown`](https://CRAN.R-project.org/package=bookdown)     |    0.40 | Authoring Books and Technical Documents with R Markdown                       |
| [`DHARMa`](https://CRAN.R-project.org/package=DHARMa)         |   0.4.6 | Residual Diagnostics for Hierarchical (Multi-Level / Mixed) Regression Models |
| [`dplyr`](https://CRAN.R-project.org/package=dplyr)           |   1.1.4 | A Grammar of Data Manipulation                                                |
| [`emmeans`](https://CRAN.R-project.org/package=emmeans)       |  1.10.3 | Estimated Marginal Means, aka Least-Squares Means                             |
| [`flextable`](https://CRAN.R-project.org/package=flextable)   |   0.9.6 | Functions for Tabular Reporting                                               |
| [`glmmTMB`](https://CRAN.R-project.org/package=glmmTMB)       |   1.1.7 | Generalized Linear Mixed Models using Template Model Builder                  |
| [`htmltools`](https://CRAN.R-project.org/package=htmltools)   | 0.5.8.1 | Tools for HTML                                                                |
| [`kableExtra`](https://CRAN.R-project.org/package=kableExtra) |   1.4.0 | Construct Complex Table with ‘kable’ and Pipe Syntax                          |
| [`knitr`](https://CRAN.R-project.org/package=knitr)           |    1.48 | A General-Purpose Package for Dynamic Report Generation in R                  |
| [`lubridate`](https://CRAN.R-project.org/package=lubridate)   |   1.9.3 | Make Dealing with Dates a Little Easier                                       |
| [`parallel`](https://CRAN.R-project.org/package=parallel)     |   4.4.1 | Support for Parallel Computation in R                                         |
| [`pdftools`](https://CRAN.R-project.org/package=pdftools)     |   3.4.0 | Text Extraction, Rendering and Converting of PDF Documents                    |
| [`readxl`](https://CRAN.R-project.org/package=readxl)         |   1.4.3 | Read Excel Files                                                              |
| [`reshape2`](https://CRAN.R-project.org/package=reshape2)     |   1.4.4 | Flexibly Reshape Data: A Reboot of the Reshape Package                        |
| [`rmarkdown`](https://CRAN.R-project.org/package=rmarkdown)   |    2.27 | Dynamic Documents for R                                                       |
| [`rprojroot`](https://CRAN.R-project.org/package=rprojroot)   |   2.0.4 | Finding Files in Project Subdirectories                                       |
| [`scales`](https://CRAN.R-project.org/package=scales)         |   1.3.0 | Scale Functions for Visualization                                             |
| [`shiny`](https://CRAN.R-project.org/package=shiny)           |   1.9.1 | Web Application Framework for R                                               |
| [`snow`](https://CRAN.R-project.org/package=snow)             |   0.4.4 | Simple Network of Workstations                                                |
| [`stringr`](https://CRAN.R-project.org/package=stringr)       |   1.5.1 | Simple, Consistent Wrappers for Common String Operations                      |

Running the code below will display the packages not already installed
on your machine:

``` r
pkgs <- c("argparser", "bookdown", "DHARMa", "dplyr", "emmeans", "flextable", "glmmTMB", "htmltools", "kableExtra", "knitr", "lubridate", "parallel", "pdftools", "readxl", "reshape2", "rmarkdown", "rprojroot", "scales", "shiny", "snow", "stringr")
pkgs[!pkgs %in% rownames(installed.packages())]
```

## Session Info

<details>
<summary>
<b>Click to Expand/Hide Session Info</b>
</summary>

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.4.1 (2024-06-14 ucrt)
    ##  os       Windows 11 x64 (build 22631)
    ##  system   x86_64, mingw32
    ##  ui       RTerm
    ##  language (EN)
    ##  collate  English_United States.utf8
    ##  ctype    English_United States.utf8
    ##  tz       America/Los_Angeles
    ##  date     2024-09-03
    ##  pandoc   3.1.11 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  ! package           * version    date (UTC) lib source
    ##    argparser         * 0.7.2      2024-04-04 [1] CRAN (R 4.4.1)
    ##    askpass             1.2.0      2023-09-03 [1] CRAN (R 4.4.1)
    ##    bookdown          * 0.40       2024-07-02 [1] CRAN (R 4.4.1)
    ##    boot                1.3-30     2024-02-26 [2] CRAN (R 4.4.1)
    ##    cellranger          1.1.0      2016-07-27 [1] CRAN (R 4.4.1)
    ##    cli                 3.6.3      2024-06-21 [1] CRAN (R 4.4.1)
    ##    coda                0.19-4.1   2024-01-31 [1] CRAN (R 4.4.1)
    ##    colorspace          2.1-1      2024-07-26 [1] CRAN (R 4.4.1)
    ##    crayon              1.5.3      2024-06-20 [1] CRAN (R 4.4.1)
    ##    crul                1.5.0      2024-07-19 [1] CRAN (R 4.4.1)
    ##    curl                5.2.1      2024-03-01 [1] CRAN (R 4.4.1)
    ##    data.table          1.15.4     2024-03-30 [1] CRAN (R 4.4.1)
    ##    DHARMa            * 0.4.6      2022-09-08 [1] CRAN (R 4.4.1)
    ##    digest              0.6.36     2024-06-23 [1] CRAN (R 4.4.1)
    ##    dplyr             * 1.1.4      2023-11-17 [1] CRAN (R 4.4.1)
    ##    emmeans           * 1.10.3     2024-07-01 [1] CRAN (R 4.4.1)
    ##    estimability        1.5.1      2024-05-12 [1] CRAN (R 4.4.1)
    ##    evaluate            0.24.0     2024-06-10 [1] CRAN (R 4.4.1)
    ##    fansi               1.0.6      2023-12-08 [1] CRAN (R 4.4.1)
    ##    fastmap             1.2.0      2024-05-15 [1] CRAN (R 4.4.1)
    ##    flextable         * 0.9.6      2024-05-05 [1] CRAN (R 4.4.1)
    ##    fontBitstreamVera   0.1.1      2017-02-01 [1] CRAN (R 4.4.0)
    ##    fontLiberation      0.1.0      2016-10-15 [1] CRAN (R 4.4.0)
    ##    fontquiver          0.2.1      2017-02-01 [1] CRAN (R 4.4.1)
    ##    gdtools             0.3.7      2024-03-05 [1] CRAN (R 4.4.1)
    ##    generics            0.1.3      2022-07-05 [1] CRAN (R 4.4.1)
    ##    gfonts              0.2.0      2023-01-08 [1] CRAN (R 4.4.1)
    ##    glmmTMB           * 1.1.7      2023-04-05 [1] CRAN (R 4.4.1)
    ##    glue                1.7.0      2024-01-09 [1] CRAN (R 4.4.1)
    ##    htmltools         * 0.5.8.1    2024-04-04 [1] CRAN (R 4.4.1)
    ##    httpcode            0.3.0      2020-04-10 [1] CRAN (R 4.4.1)
    ##    httpuv              1.6.15     2024-03-26 [1] CRAN (R 4.4.1)
    ##    jsonlite            1.8.8      2023-12-04 [1] CRAN (R 4.4.1)
    ##    kableExtra        * 1.4.0      2024-01-24 [1] CRAN (R 4.4.1)
    ##    knitr             * 1.48       2024-07-07 [1] CRAN (R 4.4.1)
    ##    later               1.3.2      2023-12-06 [1] CRAN (R 4.4.1)
    ##    lattice             0.22-6     2024-03-20 [2] CRAN (R 4.4.1)
    ##    lifecycle           1.0.4      2023-11-07 [1] CRAN (R 4.4.1)
    ##    lme4                1.1-35.5   2024-07-03 [1] CRAN (R 4.4.1)
    ##    lubridate         * 1.9.3      2023-09-27 [1] CRAN (R 4.4.1)
    ##    magrittr            2.0.3      2022-03-30 [1] CRAN (R 4.4.1)
    ##    MASS                7.3-60.2   2024-04-26 [2] CRAN (R 4.4.1)
    ##    Matrix              1.7-0      2024-04-26 [2] CRAN (R 4.4.1)
    ##    mime                0.12       2021-09-28 [1] CRAN (R 4.4.0)
    ##    minqa               1.2.7      2024-05-20 [1] CRAN (R 4.4.1)
    ##    munsell             0.5.1      2024-04-01 [1] CRAN (R 4.4.1)
    ##    mvtnorm             1.2-6      2024-08-17 [1] CRAN (R 4.4.1)
    ##    nlme                3.1-164    2023-11-27 [2] CRAN (R 4.4.1)
    ##    nloptr              2.1.1      2024-06-25 [1] CRAN (R 4.4.1)
    ##    numDeriv            2016.8-1.1 2019-06-06 [1] CRAN (R 4.4.0)
    ##    officer             0.6.6      2024-05-05 [1] CRAN (R 4.4.1)
    ##    openssl             2.2.0      2024-05-16 [1] CRAN (R 4.4.1)
    ##    pdftools          * 3.4.0      2023-09-25 [1] CRAN (R 4.4.1)
    ##    pillar              1.9.0      2023-03-22 [1] CRAN (R 4.4.1)
    ##    pkgconfig           2.0.3      2019-09-22 [1] CRAN (R 4.4.1)
    ##    plyr                1.8.9      2023-10-02 [1] CRAN (R 4.4.1)
    ##    promises            1.3.0      2024-04-05 [1] CRAN (R 4.4.1)
    ##    qpdf                1.3.3      2024-03-25 [1] CRAN (R 4.4.1)
    ##    R6                  2.5.1      2021-08-19 [1] CRAN (R 4.4.1)
    ##    ragg                1.3.2      2024-05-15 [1] CRAN (R 4.4.1)
    ##    Rcpp                1.0.13     2024-07-17 [1] CRAN (R 4.4.1)
    ##    readxl            * 1.4.3      2023-07-06 [1] CRAN (R 4.4.1)
    ##    renv                1.0.7      2024-04-11 [1] CRAN (R 4.4.1)
    ##    reshape2          * 1.4.4      2020-04-09 [1] CRAN (R 4.4.1)
    ##    rlang               1.1.4      2024-06-04 [1] CRAN (R 4.4.1)
    ##    rmarkdown         * 2.27       2024-05-17 [1] CRAN (R 4.4.1)
    ##    rprojroot         * 2.0.4      2023-11-05 [1] CRAN (R 4.4.1)
    ##    rstudioapi          0.16.0     2024-03-24 [1] CRAN (R 4.4.1)
    ##    scales            * 1.3.0      2023-11-28 [1] CRAN (R 4.4.1)
    ##    sessioninfo         1.2.2      2021-12-06 [1] CRAN (R 4.4.1)
    ##    shiny             * 1.9.1      2024-08-01 [1] CRAN (R 4.4.1)
    ##    snow              * 0.4-4      2021-10-27 [1] CRAN (R 4.4.0)
    ##    stringi             1.8.4      2024-05-06 [1] CRAN (R 4.4.0)
    ##    stringr           * 1.5.1      2023-11-14 [1] CRAN (R 4.4.1)
    ##    svglite             2.1.3      2023-12-08 [1] CRAN (R 4.4.1)
    ##    systemfonts         1.1.0      2024-05-15 [1] CRAN (R 4.4.1)
    ##    textshaping         0.4.0      2024-05-24 [1] CRAN (R 4.4.1)
    ##    tibble              3.2.1      2023-03-20 [1] CRAN (R 4.4.1)
    ##    tidyselect          1.2.1      2024-03-11 [1] CRAN (R 4.4.1)
    ##    timechange          0.3.0      2024-01-18 [1] CRAN (R 4.4.1)
    ##  D TMB                 1.9.14     2024-07-03 [1] CRAN (R 4.4.1)
    ##    utf8                1.2.4      2023-10-22 [1] CRAN (R 4.4.1)
    ##    uuid                1.2-1      2024-07-29 [1] CRAN (R 4.4.1)
    ##    vctrs               0.6.5      2023-12-01 [1] CRAN (R 4.4.1)
    ##    viridisLite         0.4.2      2023-05-02 [1] CRAN (R 4.4.1)
    ##    xfun                0.46       2024-07-18 [1] CRAN (R 4.4.1)
    ##    xml2                1.3.6      2023-12-04 [1] CRAN (R 4.4.1)
    ##    xtable              1.8-4      2019-04-21 [1] CRAN (R 4.4.1)
    ##    yaml                2.3.10     2024-07-26 [1] CRAN (R 4.4.1)
    ##    zip                 2.3.1      2024-01-27 [1] CRAN (R 4.4.1)
    ## 
    ##  [1] C:/Users/bstaton/AppData/Local/R/win-library/4.4
    ##  [2] C:/Program Files/R/R-4.4.1/library
    ## 
    ##  D ── DLL MD5 mismatch, broken installation.
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────

</details>
