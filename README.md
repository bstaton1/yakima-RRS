
> This repository stores the code used to perform the GLM analyses
> presented in the manuscript *Evaluating the long-term net reproductive
> benefits and costs of supplementation in upper Yakima River Chinook
> salmon* by I.J. Koch, B.A. Staton, H.M. Nuetzel, T.R. Seamons, A.P.
> Matala, K.I. Warheit, M.V. Johnston, C.R. Strom, S.R. Narum, and W.J.
> Bosch.

[![ArticleDOI](https://img.shields.io/badge/Article%20DOI%20PLACEHOLDER-blue)]()

[![GitHub Repo Archive
DOI](https://img.shields.io/badge/GitHub%20Repo%20Archive%20DOI%20PLACEHOLDER-blue)]()

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
> per GLM analysis. This takes approximately **X** hours on a current
> modern laptop computer (RAM: 64GB; Processor: 20 threads, 5.40 GHz Max
> Turbo, 24 MB Cache). Similar results to those we present can be
> obtained with fewer bootstrap iterations – change the value of the
> `nboot` variable in the `run-all.R` script to, e.g., 100 to obtain
> results faster.*

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
for each analysis interactively to gain insights about the steps of the
analysis.

## Dependencies

The table below shows all R packages (and their versions at time of
publication) that are specifically called (typically with `pkg::fn()`)
or loaded (using `library(pkg)`).

| Package                                                       |    Version | Purpose                                                                 |
|:--------------------------------------------------------------|-----------:|:------------------------------------------------------------------------|
| [`argparser`](https://CRAN.R-project.org/package=argparser)   |      0.7.1 | Facilitates passing arguments to R scripts via command line             |
| [`bookdown`](https://CRAN.R-project.org/package=bookdown)     |       0.33 | Dynamic report generation (long-form)                                   |
| [`DHARMa`](https://CRAN.R-project.org/package=DHARMa)         |      0.4.6 | Generation of quantile-standardized residuals from GLMs                 |
| [`dplyr`](https://CRAN.R-project.org/package=dplyr)           |      1.1.2 | Easier data summaries/data frame manipulations                          |
| [`emmeans`](https://CRAN.R-project.org/package=emmeans)       |      1.8.6 | Multiple comparisons (only in `08-attr-v-origin`)                       |
| [`flextable`](https://CRAN.R-project.org/package=flextable)   |      0.9.5 | Creation/customization of tables rendered with Rmarkdown                |
| [`glmmTMB`](https://CRAN.R-project.org/package=glmmTMB)       |      1.1.7 | Fitting/predicting from GLMs                                            |
| [`htmltools`](https://CRAN.R-project.org/package=htmltools)   |    0.5.8.1 | Easier construction of HTML tags                                        |
| [`kableExtra`](https://CRAN.R-project.org/package=kableExtra) | 1.3.4.9000 | Customization of tables rendered with Rmarkdown                         |
| [`knitr`](https://CRAN.R-project.org/package=knitr)           |       1.42 | Dynamic report generation                                               |
| [`lubridate`](https://CRAN.R-project.org/package=lubridate)   |      1.9.2 | Easier manipulation of dates                                            |
| [`parallel`](https://CRAN.R-project.org/package=parallel)     |      4.3.0 | Supports parallel processing for bootstrap                              |
| [`readxl`](https://CRAN.R-project.org/package=readxl)         |      1.4.2 | Loading data from `.xlsx` files                                         |
| [`reshape2`](https://CRAN.R-project.org/package=reshape2)     |      1.4.4 | Converting data frames from wide to long (and vice versa)               |
| [`rmarkdown`](https://CRAN.R-project.org/package=rmarkdown)   |       2.21 | Dynamic report generation                                               |
| [`rprojroot`](https://CRAN.R-project.org/package=rprojroot)   |      2.0.3 | Automated file/directory path detection                                 |
| [`scales`](https://CRAN.R-project.org/package=scales)         |      1.2.1 | Supports transparent colors in figures                                  |
| [`shiny`](https://CRAN.R-project.org/package=shiny)           |      1.7.4 | Interactive web interfaces (used only for the `shiny::icon()` function) |
| [`snow`](https://CRAN.R-project.org/package=snow)             |      0.4.4 | Supports parallel processing for bootstrap                              |
| [`stringr`](https://CRAN.R-project.org/package=stringr)       |      1.5.0 | Easier manipulation of character strings                                |

Click below to display the exact computer set up used for all GLM
analyses in the publication.

<details>
<summary>
Click to Expand/Hide
</summary>

    ## ─ Session info ───────────────────────────────────────────────────────────────
    ##  setting  value
    ##  version  R version 4.3.0 (2023-04-21 ucrt)
    ##  os       Windows 11 x64 (build 22000)
    ##  system   x86_64, mingw32
    ##  ui       RTerm
    ##  language (EN)
    ##  collate  English_United States.utf8
    ##  ctype    English_United States.utf8
    ##  tz       America/Los_Angeles
    ##  date     2024-08-05
    ##  pandoc   2.19.2 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)
    ## 
    ## ─ Packages ───────────────────────────────────────────────────────────────────
    ##  package           * version    date (UTC) lib source
    ##  argparser         * 0.7.1      2021-03-08 [1] CRAN (R 4.3.0)
    ##  askpass             1.1        2019-01-13 [1] CRAN (R 4.3.0)
    ##  bookdown          * 0.33       2023-03-06 [1] CRAN (R 4.3.0)
    ##  boot                1.3-28.1   2022-11-22 [2] CRAN (R 4.3.0)
    ##  cellranger          1.1.0      2016-07-27 [1] CRAN (R 4.3.0)
    ##  cli                 3.6.1      2023-03-23 [1] CRAN (R 4.3.0)
    ##  coda                0.19-4     2020-09-30 [1] CRAN (R 4.3.0)
    ##  codetools           0.2-19     2023-02-01 [2] CRAN (R 4.3.0)
    ##  colorspace          2.1-0      2023-01-23 [1] CRAN (R 4.3.0)
    ##  crayon              1.5.2      2022-09-29 [1] CRAN (R 4.3.0)
    ##  crul                1.4.0      2023-05-17 [1] CRAN (R 4.3.0)
    ##  curl                5.0.0      2023-01-12 [1] CRAN (R 4.3.0)
    ##  data.table          1.14.8     2023-02-17 [1] CRAN (R 4.3.0)
    ##  DHARMa            * 0.4.6      2022-09-08 [1] CRAN (R 4.3.0)
    ##  digest              0.6.31     2022-12-11 [1] CRAN (R 4.3.0)
    ##  dplyr             * 1.1.2      2023-04-20 [1] CRAN (R 4.3.0)
    ##  ellipsis            0.3.2      2021-04-29 [1] CRAN (R 4.3.0)
    ##  emmeans           * 1.8.6      2023-05-11 [1] CRAN (R 4.3.0)
    ##  estimability        1.4.1      2022-08-05 [1] CRAN (R 4.3.0)
    ##  evaluate            0.20       2023-01-17 [1] CRAN (R 4.3.0)
    ##  fansi               1.0.4      2023-01-22 [1] CRAN (R 4.3.0)
    ##  fastmap             1.1.1      2023-02-24 [1] CRAN (R 4.3.0)
    ##  flextable         * 0.9.5      2024-03-06 [1] CRAN (R 4.3.3)
    ##  fontBitstreamVera   0.1.1      2017-02-01 [1] CRAN (R 4.3.0)
    ##  fontLiberation      0.1.0      2016-10-15 [1] CRAN (R 4.3.0)
    ##  fontquiver          0.2.1      2017-02-01 [1] CRAN (R 4.3.0)
    ##  gdtools             0.3.7      2024-03-05 [1] CRAN (R 4.3.3)
    ##  generics            0.1.3      2022-07-05 [1] CRAN (R 4.3.0)
    ##  gfonts              0.2.0      2023-01-08 [1] CRAN (R 4.3.0)
    ##  glmmTMB           * 1.1.7      2023-04-05 [1] CRAN (R 4.3.0)
    ##  glue                1.6.2      2022-02-24 [1] CRAN (R 4.3.0)
    ##  htmltools         * 0.5.8.1    2024-04-04 [1] CRAN (R 4.3.3)
    ##  httpcode            0.3.0      2020-04-10 [1] CRAN (R 4.3.0)
    ##  httpuv              1.6.9      2023-02-14 [1] CRAN (R 4.3.0)
    ##  httr                1.4.5      2023-02-24 [1] CRAN (R 4.3.0)
    ##  jsonlite            1.8.4      2022-12-06 [1] CRAN (R 4.3.0)
    ##  kableExtra        * 1.3.4.9000 2023-05-15 [1] Github (kupietz/kableExtra@3bf9b21)
    ##  knitr             * 1.42       2023-01-25 [1] CRAN (R 4.3.0)
    ##  later               1.3.0      2021-08-18 [1] CRAN (R 4.3.0)
    ##  lattice             0.21-8     2023-04-05 [2] CRAN (R 4.3.0)
    ##  lifecycle           1.0.3      2022-10-07 [1] CRAN (R 4.3.0)
    ##  lme4                1.1-33     2023-04-25 [1] CRAN (R 4.3.0)
    ##  lubridate         * 1.9.2      2023-02-10 [1] CRAN (R 4.3.0)
    ##  magrittr            2.0.3      2022-03-30 [1] CRAN (R 4.3.0)
    ##  MASS                7.3-58.4   2023-03-07 [2] CRAN (R 4.3.0)
    ##  Matrix              1.5-4      2023-04-04 [2] CRAN (R 4.3.0)
    ##  mime                0.12       2021-09-28 [1] CRAN (R 4.3.0)
    ##  minqa               1.2.5      2022-10-19 [1] CRAN (R 4.3.0)
    ##  multcomp            1.4-23     2023-03-09 [1] CRAN (R 4.3.0)
    ##  munsell             0.5.0      2018-06-12 [1] CRAN (R 4.3.0)
    ##  mvtnorm             1.1-3      2021-10-08 [1] CRAN (R 4.3.0)
    ##  nlme                3.1-162    2023-01-31 [2] CRAN (R 4.3.0)
    ##  nloptr              2.0.3      2022-05-26 [1] CRAN (R 4.3.0)
    ##  numDeriv            2016.8-1.1 2019-06-06 [1] CRAN (R 4.3.0)
    ##  officer             0.6.5      2024-02-24 [1] CRAN (R 4.3.3)
    ##  openssl             2.0.6      2023-03-09 [1] CRAN (R 4.3.0)
    ##  pillar              1.9.0      2023-03-22 [1] CRAN (R 4.3.0)
    ##  pkgconfig           2.0.3      2019-09-22 [1] CRAN (R 4.3.0)
    ##  plyr                1.8.8      2022-11-11 [1] CRAN (R 4.3.0)
    ##  promises            1.2.0.1    2021-02-11 [1] CRAN (R 4.3.0)
    ##  R6                  2.5.1      2021-08-19 [1] CRAN (R 4.3.0)
    ##  ragg                1.2.5      2023-01-12 [1] CRAN (R 4.3.0)
    ##  Rcpp                1.0.10     2023-01-22 [1] CRAN (R 4.3.0)
    ##  readxl            * 1.4.2      2023-02-09 [1] CRAN (R 4.3.0)
    ##  reshape2          * 1.4.4      2020-04-09 [1] CRAN (R 4.3.0)
    ##  rlang               1.1.0      2023-03-14 [1] CRAN (R 4.3.0)
    ##  rmarkdown         * 2.21       2023-03-26 [1] CRAN (R 4.3.0)
    ##  rprojroot         * 2.0.3      2022-04-02 [1] CRAN (R 4.3.0)
    ##  rstudioapi          0.14       2022-08-22 [1] CRAN (R 4.3.0)
    ##  rvest               1.0.3      2022-08-19 [1] CRAN (R 4.3.0)
    ##  sandwich            3.0-2      2022-06-15 [1] CRAN (R 4.3.0)
    ##  scales            * 1.2.1      2022-08-20 [1] CRAN (R 4.3.0)
    ##  sessioninfo         1.2.2      2021-12-06 [1] CRAN (R 4.3.0)
    ##  shiny             * 1.7.4      2022-12-15 [1] CRAN (R 4.3.0)
    ##  snow              * 0.4-4      2021-10-27 [1] CRAN (R 4.3.0)
    ##  stringi             1.7.12     2023-01-11 [1] CRAN (R 4.3.0)
    ##  stringr           * 1.5.0      2022-12-02 [1] CRAN (R 4.3.0)
    ##  survival            3.5-5      2023-03-12 [2] CRAN (R 4.3.0)
    ##  svglite             2.1.1      2023-01-10 [1] CRAN (R 4.3.0)
    ##  systemfonts         1.0.4      2022-02-11 [1] CRAN (R 4.3.0)
    ##  textshaping         0.3.6      2021-10-13 [1] CRAN (R 4.3.0)
    ##  TH.data             1.1-2      2023-04-17 [1] CRAN (R 4.3.0)
    ##  tibble              3.2.1      2023-03-20 [1] CRAN (R 4.3.0)
    ##  tidyselect          1.2.0      2022-10-10 [1] CRAN (R 4.3.0)
    ##  timechange          0.2.0      2023-01-11 [1] CRAN (R 4.3.0)
    ##  TMB                 1.9.4      2023-04-18 [1] CRAN (R 4.3.0)
    ##  utf8                1.2.3      2023-01-31 [1] CRAN (R 4.3.0)
    ##  uuid                1.1-0      2022-04-19 [1] CRAN (R 4.3.0)
    ##  vctrs               0.6.2      2023-04-19 [1] CRAN (R 4.3.0)
    ##  viridisLite         0.4.1      2022-08-22 [1] CRAN (R 4.3.0)
    ##  webshot             0.5.4      2022-09-26 [1] CRAN (R 4.3.0)
    ##  xfun                0.39       2023-04-20 [1] CRAN (R 4.3.0)
    ##  xml2                1.3.3      2021-11-30 [1] CRAN (R 4.3.0)
    ##  xtable              1.8-4      2019-04-21 [1] CRAN (R 4.3.0)
    ##  yaml                2.3.7      2023-01-23 [1] CRAN (R 4.3.0)
    ##  zip                 2.3.0      2023-04-17 [1] CRAN (R 4.3.0)
    ##  zoo                 1.8-12     2023-04-13 [1] CRAN (R 4.3.0)
    ## 
    ##  [1] C:/Users/bstaton/AppData/Local/R/win-library/4.3
    ##  [2] C:/Program Files/R/R-4.3.0/library
    ## 
    ## ──────────────────────────────────────────────────────────────────────────────

</details>
