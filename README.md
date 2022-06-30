![Build Status](https://travis-ci.org/sdcTools/recordSwapping.svg?branch=master)
<!--[![Coverage Status](https://coveralls.io/repos/github/sdcTools/recordSwapping/badge.svg?branch=master)](https://coveralls.io/github/sdcTools/recordSwapping?branch=master)-->
<!--[![CRAN](http://www.r-pkg.org/badges/version/recordSwapping)](https://CRAN.R-project.org/package=recordSwapping)-->
<!--[![Downloads](http://cranlogs.r-pkg.org/badges/recordSwapping)](https://CRAN.R-project.org/package=recordSwapping)-->
<!--[![Mentioned in Awesome Official Statistics ](https://awesome.re/mentioned-badge.svg)](http://www.awesomeofficialstatistics.org)-->

# recordSwapping


**recordSwapping** is an R-package for record swapping.


*src/recordSwap* contains the pure `C++` (11) code `recordSwap.cpp` and `recordSwap.h`.
In this package this code is called by an `Rcpp` wrapper which again is called from the top level `R`-function `recordSwap()`.
The `C++` code is not directly embedded using `Rcpp` so that it can be more easily implemented into other projects which do not depend on `R` libraries.
Using an additional top level `R`-functions as well as an `Rcpp` wrapper makes it more conventient to call the pure `C++` code at the bottom as there is no mapping to every stl container from `R`.

## Versions

Information on the different versions can be found [here](https://github.com/sdcTools/recordSwapping/blob/master/NEWS.md)

## Installation

The package can be installed through
```r
devtools::install_github(
  repo = "sdcTools/recordSwapping", 
  build_vignettes = TRUE
)
```

## Application

The procedure can be applied as follows

```r
library(recordSwapping)

# create some dummy data (~ 50k households)
dat <- createDat(50000)

# define parameters through column names or column indices
hierarchy <- c("nuts1","nuts2") # c(1,2)
risk_variables <- c("ageGroup") # c(7)
hid <- "hid" # c(5)

swaprate <- .05 # swapping rate of households

# households are set to risky and are mandatorily swapped
# if at least one individual has counts (over risk_variables and hierarchies)
# smaller equal k_anonymity
# set k_anonymity <- 0 to deactivate this feature
k_anonymity <- 3

# similarity profile: hsize
similar <- "hsize"

# call recodSwap()
dat_swapped <- recordSwap(
  data = dat, hid = hid,
  hierarchy = hierarchy,
  similar = similar,
  risk_variables = risk_variables,
  k_anonymity = k_anonymity,
  swaprate = swaprate
)
dat_swapped
```

```r
# multiple similarity profiles
# first similarity profile: hsize, htype, hincome
# second similarity profile: hsize
similar <- list(
  c("hsize","htype","hincome"),
  c("hsize")
)
similar

dat_swapped <- recordSwap(
  data = dat, 
  hid = hid,
  hierarchy = hierarchy,
  similar = similar,
  risk_variables = risk_variables,
  k_anonymity = k_anonymity,
  swaprate = swaprate
)
```

```r
# return ID of swapped household
dat_swapped <- recordSwap(
  data = dat, hid = hid,
  hierarchy = hierarchy,
  similar = similar,
  risk_variables = risk_variables,
  k_anonymity = k_anonymity,
  swaprate = swaprate,
  return_swapped_id = TRUE
)
                          
dat_swapped[, .(hid, hid_swapped, nuts1, nuts2, nuts3)]
```

More detailed information can be found through the help-pages (`?recordSwap`) or when calling the vignette

```r
vignette("recordSwapping")
```
