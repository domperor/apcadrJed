
# apcadrJed

<!-- badges: start -->
<!-- badges: end -->

This package calculate APC (adenoma per colonoscopy) and ADR (adenoma detection rate) from JED data. It has a single function called "apcadr". You can find test dataset in "tests/testthat/sample_CF_data.csv".


## Installation

You can install the development version of apcadrJed like so:

``` r
remotes::install_github("domperor/apcadrJed")
library(apcadrJed)
```

## Example

This is a basic example:

``` r
library(apcadrJed)
data<-(c("腺腫|腺腫|憩室", "憩室|痔核", "特記所見なし", "家族性大腸腺腫症"))
apcadr(data)

# returns "APC:0.66667, ADR:0.33333"
```

