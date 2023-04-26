<img src="vignettes/seqLogo-logo.png" align="right" alt="" width="120" />

# _seqLogo_ - Plotting the position weight matrix of a DNA/RNA sequence motif as a sequence logo

## Software status

<!-- badges: start -->
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![How long since the package was first in a released Bioconductor version](https://bioconductor.org/shields/years-in-bioc/seqLogo.svg)](https://bioconductor.org/packages/seqLogo) 
[![Bioconductor-devel Downloads](https://bioconductor.org/shields/downloads/devel/seqLogo.svg)](https://bioconductor.org/packages/stats/bioc/seqLogo/)
[![Support site activity in last 6 months: agged questions/avg. answers per question/avg. comments per question/accepted answers, or 0 if no tagged posts](https://bioconductor.org/shields/posts/seqLogo.svg)](https://support.bioconductor.org/t/seqLogo/)
[![GitHub issues](https://img.shields.io/github/issues/ivanek/seqLogo)](https://github.com/ivanek/seqLogo/issues)
<!-- badges: end -->

&nbsp;

|                     | Bioc ([release](https://bioconductor.org/packages/release/bioc/html/seqLogo.html)) | Bioc ([devel](https://bioconductor.org/packages/devel/bioc/html/seqLogo.html)) |
|:--------------------|----------------------------------------------------------------------------:|--------------------------------------------------------------------------------:|
| OS                  | [![Platforms](https://bioconductor.org/shields/availability/release/seqLogo.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/seqLogo/) | [![Platforms](https://bioconductor.org/shields/availability/devel/seqLogo.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/seqLogo/) |
| Bioc Last Update    | [![Bioconductor-release Last Commit](https://bioconductor.org/shields/lastcommit/release/bioc/seqLogo.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/seqLogo/) | [![Bioconductor-devel Last Commit](https://bioconductor.org/shields/lastcommit/devel/bioc/seqLogo.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/seqLogo/) |
| Bioc Status         | [![Bioconductor-release Build Status](https://bioconductor.org/shields/build/release/bioc/seqLogo.svg)](https://bioconductor.org/checkResults/release/bioc-LATEST/seqLogo) | [![Bioconductor-devel Build Status](https://bioconductor.org/shields/build/devel/bioc/seqLogo.svg)](https://bioconductor.org/checkResults/devel/bioc-LATEST/seqLogo) |
| GitHub Last Commit  | [![GitHub last commit (Bioconductor-release)](https://img.shields.io/github/last-commit/ivanek/seqLogo/RELEASE_3_17)](https://github.com/ivanek/seqLogo/tree/RELEASE_3_17) | [![GitHub last commit (Bioconductor-devel)](https://img.shields.io/github/last-commit/ivanek/seqLogo/devel)](https://github.com/ivanek/seqLogo/tree/devel/) |
| GitHub Actions      | [![R build status](https://github.com/ivanek/seqLogo/workflows/R-CMD-check-bioc/badge.svg?branch=RELEASE_3_17)](https://github.com/ivanek/seqLogo/actions) | [![R build status](https://github.com/ivanek/seqLogo/workflows/R-CMD-check-bioc/badge.svg?branch=devel)](https://github.com/ivanek/seqLogo/actions) |
| Coverage            | [![Codecov.io (Bioconductor-release)](https://codecov.io/github/ivanek/seqLogo/coverage.svg?branch=RELEASE_3_17)](https://codecov.io/gh/ivanek/seqLogo/branch/RELEASE_3_17) | [![Codecov.io (Bioconductor-devel)](https://codecov.io/github/ivanek/seqLogo/coverage.svg?branch=devel)](https://codecov.io/github/ivanek/seqLogo) |

## Authors

- Oliver Bembom
- Robert Ivanek

## Overview

seqLogo takes the position weight matrix of a DNA sequence motif and plots 
the corresponding sequence logo as introduced by Schneider and Stephens (1990).

## Installation

#### Release version

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("seqLogo", version = "release")
```

#### Developmental version

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install("seqLogo", version = "devel")
```

#### Github

```
if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
remotes::install_github("ivanek/seqLogo")
```

## Usage

For detailed instructions check the package vignette 
([release](https://bioconductor.org/packages/release/bioc/vignettes/seqLogo/inst/doc/seqLogo.pdf) 
or 
[developmental](https://bioconductor.org/packages/devel/bioc/vignettes/seqLogo/inst/doc/seqLogo.html) 
version). Or the GitHub.io [page](https://ivanek.github.io/seqLogo/).

## Citation 

```
citation("seqLogo")
```

- Oliver Bembom and Robert Ivanek (2020). seqLogo: Sequence logos for DNA sequence alignments.
