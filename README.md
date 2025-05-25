# tabela <badge-and-version>

_A fast, polite scraper for downloadable files & tables_  

[![R-CMD-check](https://github.com/deepbas/tabela/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/deepbas/tabela/actions/workflows/R-CMD-check.yaml)  
[![Codecov](https://codecov.io/gh/deepbas/tabela/branch/main/graph/badge.svg)](https://codecov.io/gh/deepbas/tabela)  
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)  

## Installation

```r
# install from CRAN (when published)
install.packages("tabela")

# or the development version:
remotes::install_github("deepbas/tabela")
```

## Quick example


```r
library(tabela)
res <- scrape_page_data(
  "https://en.wikipedia.org/wiki/2012_Summer_Olympics",
  exts = c("csv"),
  list_tables = TRUE
)
knitr::kable(res$tables[[2]] |> head())
```