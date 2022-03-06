
<!-- README.md is generated from README.Rmd. Please edit that file -->

# morse

<!-- badges: start -->
<!-- badges: end -->

morse provides functions to encode and decode with [Morse
code](https://en.wikipedia.org/wiki/Morse_code).

## Installation

You can install the development version of morse like so:

``` r
# install.packages("devtools")
devtools::install_github("kunhtkun/morse")
```

## Usage

To encode a string into Morse code words, use the function `morse`:

``` r
library(morse)
input <- "hello!"
morse(input)
#> [1] "...././.-../.-../---/-.-.--"
```

To decode, use `unmorse`:

``` r
library(morse)
input <- "hello!"
res <- morse(input)
unmorse(res)
#> [1] "hello!"
```
