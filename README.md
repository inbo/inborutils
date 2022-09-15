# inborutils package

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![`runiverse-name`](https://inbo.r-universe.dev/badges/:name)
![`runiverse-package`](https://inbo.r-universe.dev/badges/inborutils)
<!-- badges: end -->

This `inborutils` package provides a collection of useful R utilities and snippets that we consider recyclable for multiple projects. The functions are either out of scope or just not mature enough to include as extensions to existing packages. 

Some of these functions will be targeted to INBO usage itself, but maybe some of them are useful in a broader context and we would be happy to support the wider scientific community with our snippets. 

## Installation

Install `inborutils` from `inbo.r-universe`:

```r
install.packages("inborutils", repos = c(inbo = "https://inbo.r-universe.dev", 
                                         CRAN = "https://cloud.r-project.org"))
```

Or you can install `inborutils` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("inbo/inborutils")
```

When successful, load it as usual:

```r
library(inborutils)
```

## Usage

We try to do the best we can to keep the documentation up to date and keep an overview of the functionalities on the package documentation website: https://inbo.github.io/inborutils/. 
