
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HYPR IS CURRENTLY BEING REWRITTEN - MORE CODE CAN BE FOUND IN THE HYPR-OLD REPO

# hypR

<!-- badges: start -->
<!-- badges: end -->

The underlying philosophy of hypR is to pass data into a function that
will transform it into a hypergraph object that can then be passed into
other functions to perform manipulations and calculations. This means
that once the hypergraph object has been created, no further processing
of the data is needed to carry out any operation. This hopefully allows
for users to easily implement hypergraph based techniques into their
workflows with minimal technical skill required.

However, for those users with a greater technical skillset, they are
able to work directly with the R6 hypergraph object allowing for
operations and manipulations that may be prohibited by the other
functions. Such users are directed to the Hypergraph Objects section for
a more in depth explanation.

## Installation

The development version of hypR is available from
[GitHub](https://github.com) with:

``` r
#install.packages("devtools")
devtools::install_github("hwarden162/hypR")
```

## Hypergraph Objects
