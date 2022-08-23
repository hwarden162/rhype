rhype: A Flexible Package For Working With Hypergraphs In R
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/hwarden162/rhype/branch/main/graph/badge.svg)](https://app.codecov.io/gh/hwarden162/rhype?branch=main)
<!-- badges: end -->

# Introduction <img src="man/figures/rhype_hex.png" align="right" width=150px/>

`rhype` is a package for working with hypergraphs in R. It works under
the general idea that data can be transformed into a hypergraph object
and then there are many functions to manipulate and analyse this
hypergraph object.

# Installation

`rhype` is available from CRAN using

``` r
install.packages("rhype")
```

or the development version is available from GitHub using

``` r
remotes::install_github("hwarden162/rhype")
```

# Getting Started

To get started with `rhype`, see these blog posts:

-   [Working With Hypergraphs In
    R](https://www.hwarden.com/project/rhype/working-with-hypergraphs-in-r-using-rhype/)
-   [Estimating Heterogeneity of scRNA-seq Data With
    rhype](https://www.hwarden.com/project/estimating-heterogeneity-of-scrna-seq-data-with-rhype/)

Within `rhype`, there comes a new environment that saves all the
information about a hypergraph. There are multiple functions for
creating hypergraphs from your own data, but as well as this there is a
function for generating example hypergraphs to test pipelines and run
proof of concepts.

``` r
library(rhype)

hype <- example_hype()
hype
#> Hypergraph Object: 
#>     Number of vertices:  4 
#>     Number of hyperedges:  2 
#>     Oriented:  FALSE    Directed:  FALSE 
#>     Real Coefficients:  FALSE    Weighted:  FALSE
```

This is the basic output that a hypergraph object will print to the
screen. To get more information, you can use the `hype_info` function,
although this may be quite large for large hypergraphs.

``` r
hype_info(hype)
#> ====================HYPERGRAPH INFORMATION====================
#> 
#> --------------------VERTEX INFORMATION--------------------
#> 
#> This hypergraph has  4  vertices
#> 
#> These vertices are called:
#>  v1, v2, v3, v4 
#> 
#> --------------------HYPEREDGE INFORMATION--------------------
#> 
#> The hyperedges are called:
#>  h1, h2 
#> 
#> The hyperedges have the structure:
#> $h1
#> [1] 1 2 3
#> 
#> $h2
#> [1] 2 3 4
#> 
#> ---------------WEIGHTING INFORMATION--------------------
#> 
#> This hypergraph is not weighted
#> 
#> --------------------Orientation Information--------------------
#> 
#> This hypergraph is not oriented
#> 
#> This hypergraph is not directed
#> 
#> --------------------REAL COEFFICIENTS INFORMATION--------------------
#> 
#> This hypergraph does not have real coefficients associating vertices to hyperedges
#> 
#> There is no incidence matrix associating vertices to hyperedges with non-binary coefficients
```

Hypergraphs can be abstracted in many ways and can quickly become
complicated, `rhype` is designed to make these abstractions as simple to
work with as possible and design a simple interface to working with with
many common hypergraph applications. An example of a very complex
hypergraph:

``` r
hype1 <- example_hype(
  oriented = TRUE,
  directed = TRUE,
  vertex_weighted = TRUE,
  edge_weighted = TRUE,
  real_coef = TRUE
)

hype_info(hype1)
#> ====================HYPERGRAPH INFORMATION====================
#> 
#> --------------------VERTEX INFORMATION--------------------
#> 
#> This hypergraph has  4  vertices
#> 
#> These vertices are called:
#>  v1, v2, v3, v4 
#> 
#> --------------------HYPEREDGE INFORMATION--------------------
#> 
#> The hyperedges are called:
#>  h1, h2 
#> 
#> The hyperedges have the structure:
#> $h1
#> $h1$from
#> [1] 1 2
#> 
#> $h1$to
#> [1] 3 4
#> 
#> 
#> $h2
#> $h2$from
#> [1] 2 3 4
#> 
#> $h2$to
#> [1] 1 2
#> 
#> 
#> ---------------WEIGHTING INFORMATION--------------------
#> 
#> This hypergraph is weighted
#> 
#> The hyperedges have weights:
#> h1 h2 
#>  1  2 
#> 
#> The vertices have weights:
#> v1 v2 v3 v4 
#>  1  2  3  4 
#> 
#> --------------------Orientation Information--------------------
#> 
#> This hypergraph is oriented
#> 
#> This hypergraph is directed
#> 
#> --------------------REAL COEFFICIENTS INFORMATION--------------------
#> 
#> This hypergraph has real coefficients associating vertices to hyperedges
#> 
#> The incidence matrix associating vertices to hyperedges is given by:
#> $from
#>    h1 h2
#> v1  1  0
#> v2  2  2
#> v3  0  3
#> v4  0  4
#> 
#> $to
#>    h1 h2
#> v1  0  1
#> v2  0  2
#> v3  3  0
#> v4  4  0
```

`rhype` lowers the in level of technical knowledge needed for working
with hypergraphs, providing a simple interface for exploring higher
order interactions.

# Advanced Functionality

Hypergraph objects are R6 objects that have inherent properties
describing their structure. These properties are private and need to be
changed via public getter and setter functions, although at the moment
these have no validation on them as this is taken care of within other
functions.

The inherent properties of a hypergraph are:

`numv`: The number of vertices in the hypergraph

`elist`: The hyperedge list, a list of the vertices contained in each
hyperedge this structure is slightly more complicated for oriented
hypergraphs)

`vnames`: A vector of names of the vertices

`vweights`: A vector of weights of the vertices

`enames`: A vector of names of the hyperedges

`eweights`: A vector of weights of the hyperedges

`weighted`: Whether the hypergraph is weighted (a hypergraph is weighted
if it has either or both vertex or hyperedge weights)

`oriented`: Whether the hypergraph is oriented

`directed`: Whether the hypergraph is directed (all directed hypergraphs
are oriented)

`real_coef`: Whether the hypergraph has real coefficients associating
vertices to hyperedges

`inc_mat`: A matrix of the real coefficients associating vertices to
hyperedges, only present for hypergraphs with real coefficients (has a
slightly different structure for oriented hypergraphs)

It is encouraged to use these functions if `rhype` does not currently
cater to your analytical needs, however, the way in which hypergraphs
are represented may be changed and therefore so might these functions.
This means that **there is no guarantee these functions will be present
or work in the same way in the future**. If you are doing something that
is not currently supported in `rhype` please wrap it in a function and
make a pull request to the [GitHub
repo](https://github.com/hwarden162/rhype), then the code will always be
maintained and tested to work in future versions.

These properties of the hypergraph interact with one another and so
therefore making changes can break the integrity of the hypergraph
object, causing further functions to fail. If you are changing the
object directly and something is not working the `validate_hypergraph()`
function is supplied as a first point of call for troubleshooting.

``` r
hype <- example_hype(
  oriented = FALSE,
  directed = FALSE,
  edge_weighted = TRUE,
  vertex_weighted = TRUE
)

hype$set_directed(TRUE)
hype$set_numv(50)

validate_hypergraph(hype)
#>   There are 3 serious problems with this hypergraph:
#>   ✖ The number of vertices is not equal to the length of the vector containing the vertex names. 
#>  ✖ The number of vertices is not equal to the length of the vector containing the vertex weights and the hypergraph is weighted. 
#>  ✖ The hypergraph is directed but not oriented.
#>  There are 1 items that need your attention with this hypergraph:
#>   ℹ The number of vertices recorded and the number of vertices contained in the hyperedge list is different. This is expected if and only if you have an isolated vertex in your hypergraph. 
#>  These tests are not exhaustive, just an indication of where things might be going wrong.
```

As well as outputting to the screen this can also be used as a part of a
validation function by setting it to return a boolean

``` r
validate_hypergraph(hype, return = TRUE, verbose = FALSE)
#> [1] FALSE
```
