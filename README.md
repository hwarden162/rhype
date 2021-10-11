
-   [HYPR IS CURRENTLY BEING REWRITTEN - MORE CODE CAN BE FOUND IN THE
    HYPR-OLD
    REPO](#hypr-is-currently-being-rewritten---more-code-can-be-found-in-the-hypr-old-repo)
-   [hypR](#hypr)
    -   [Installation](#installation)
    -   [Getting Started](#getting-started)
        -   [Weighting](#weighting)
        -   [Orientation](#orientation)
        -   [Real Coefficients](#real-coefficients)
        -   [Combining Extra Features](#combining-extra-features)
    -   [Creating Hypergraphs](#creating-hypergraphs)
    -   [Hypergraph Objects](#hypergraph-objects)
        -   [Number of Vertices - `numv`](#number-of-vertices---numv)
        -   [Hyperedge List - `elist`](#hyperedge-list---elist)
        -   [Vertex Names - `vnames`](#vertex-names---vnames)
        -   [Vertex Weights - `vweights`](#vertex-weights---vweights)
        -   [Hyperedge Names - `enames`](#hyperedge-names---enames)
        -   [Hyperedge Weights -
            `eweights`](#hyperedge-weights---eweights)
        -   [Weighted - `weighted`](#weighted---weighted)
        -   [Oriented - `oriented`](#oriented---oriented)
        -   [Directed - `directed`](#directed---directed)
        -   [Real Coefficients -
            `real_coef`](#real-coefficients---real_coef)
        -   [Incidence Matrix - `inc_mat`](#incidence-matrix---inc_mat)

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

## Getting Started

The first thing to do is to create a hypergraph object. Hypergraph
objects can be created from your own data, but the `example_hype()`
function can be used to create many different types of hypergraphs that
are useful for exploring hypR.

``` r
library(hypR)

hype1 <- example_hype()
hype1
#> Hypergraph Object: 
#>     Number of vertices:  4 
#>     Number of hyperedges:  2 
#>     Oriented:  FALSE    Directed:  FALSE 
#>     Real Coefficients:  FALSE    Weighted:  FALSE
```

Just typing the name of a hypergraph will give you a short summary of
the of its structure. To quickly get a more in depth overview of a
hypergraph use the `hype_info()` function

``` r
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

This can generate rather long sections of text and so for bigger
hypergraphs whole sections of output can be omitted by passing different
values

``` r
hype_info(hype1, weighted = FALSE, oriented = FALSE, real_coef = FALSE)
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
```

However, if too many of these settings are being set to false it is
recommended that the user just call out each section of the structure
individually to investigate.

`hype1` is an example of a very basic hypergraph. `hypR` allows for
three more ways to augment a hypergraph: weighting, orientation and real
coefficients.

### Weighting

Weights can be applied to both vertices and hyperedges. `hype2` is an
example of a weighted hypergraph

``` r
hype2 <- example_hype(vertex_weighted = TRUE, edge_weighted = TRUE)

hype_info(hype2, oriented = FALSE, real_coef = FALSE)
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
#> This hypergraph is weighted
#> 
#> The hyperedges have weights:
#> h1 h2 
#>  1  2 
#> 
#> The vertices have weights:
#> v1 v2 v3 v4 
#>  1  2  3  4
```

Hyperedge weightings will change the way information is flows through
the hypergraph and so will have an effect on almost every calculation
done in `hypR`. Vertex weightings are currently not used in any `hypR`
functions except as a store for hyperedge weightings in dual
hypergraphs. However, **this is likely to change**, all efforts will be
made such that changes should not affect previously written code but
this may not be possible/feasible.

### Orientation

Orientations can be applied to hyperedges. This creates two groups
within the hyperedge such that information can only travel across a
hyperedge from a vertex in one group to a vertex in the other group.
`hype3` is an example of an oriented hypergraph

``` r
hype3 <- example_hype(oriented = TRUE)
hype_info(hype3, weighted = FALSE, real_coef = FALSE)
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
#> $h1[[1]]
#> [1] 1 2
#> 
#> $h1[[2]]
#> [1] 3 4
#> 
#> 
#> $h2
#> $h2[[1]]
#> [1] 2 3 4
#> 
#> $h2[[2]]
#> [1] 1 2
#> 
#> 
#> --------------------Orientation Information--------------------
#> 
#> This hypergraph is oriented
#> 
#> This hypergraph is not directed
```

In the hyperedge information section, it can be seen that each hyperedge
now has two groups, representing the two different ends of the
hyperedge. These hyperedges can then be directed such that information
can only flow across a hyperedge from one specific end to the other

``` r
hype4 <- example_hype(oriented = TRUE, directed = TRUE)
hype_info(hype4, weighted = FALSE, real_coef = FALSE)
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
#> --------------------Orientation Information--------------------
#> 
#> This hypergraph is oriented
#> 
#> This hypergraph is directed
```

Now looking at the hyperedge information, it can be seen in the naming
of the groups that each hyperedge has a dedicated set of vertices that
information can flow from and a dedicated set of vertices that
information can flow to. Vertices can be a part of both ends of an
oriented/directed hyperedge, sometimes these are referred to as
catalytic with resepect to the hyperedge.

A very important rule to remember is that **all directed hypergraphs
must be oriented** but not all oriented hypergraphs are directed.

### Real Coefficients

Real coefficients can be seen as an extension of weightings. A real
coefficient can be used to connect a vertex to a hyperedge with a
certain weight. This is useful as sometimes you may want two vertices to
both be a member of a hyperedge, but for one of those vertices to be
twice as important within that hyperedge as the other. Therefore, the
vertex can be connected specifically to that hyperedge with a
coefficient that is twice as big as the other’s.

``` r
hype5 <- example_hype(real_coef = TRUE)
hype_info(hype5, weighted = FALSE, oriented = FALSE)
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
#> --------------------REAL COEFFICIENTS INFORMATION--------------------
#> 
#> This hypergraph has real coefficients associating vertices to hyperedges
#> 
#> The incidence matrix associating vertices to hyperedges is given by:
#>    h1 h2
#> v1  1  0
#> v2  2  2
#> v3  3  3
#> v4  0  4
```

At the bottom of the printout for `hype5` an incidence matrix can be
seen, this can essentially be viewed as a table that shows how strongly
connected each vertex (row) is connected to each hyperedge (column).

### Combining Extra Features

Hypergraph weighting, orientation and real coefficients can all be
combined together the model very complicated systems

``` r
hype6 <- example_hype(
  oriented = TRUE, 
  directed = TRUE,
  vertex_weighted = TRUE,
  edge_weighted = TRUE,
  real_coef = TRUE
  )
hype_info(hype6)
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

All of these feature will interact together, with orientations
restricting the movement of information across hyperedges, weightings
changing how information moves globally through hyperedges and vertices
and real coefficients changing how information moves locally from
specific vertices to specific hyperedges.

All efforts have been made to make hypR as flexible as possible.
However, although the modelling is extremely flexible not all functions
are able to take all forms of hypergaphs, this will be stated in its
documentation or as an error message if an execution is attempted. The
reason for this is either because the computation is complex and has not
been finished yet, or because the maths governing this calculation has
not yet been finished.

If a function does not work for a particular use that you would like,
please raise it on the [GitHub Repo](https://github.com/hwarden162/hypR)
to move that job higher up the list of features to add.

## Creating Hypergraphs

There are currently two methods for creating hypergraphs in hypR, using
a hyperedge list or using an incidence matrix.

A hyperedge list is a list where each entry represents a hypergraph

## Hypergraph Objects

A hypergraph object is essentially a collection of eleven properties.
Together, these properties fully describe many different forms of
hypergraphs. These properties are listed and explained more in depth
below:

### Number of Vertices - `numv`

This is simply a postive integer value representing the number of
vertices the hypergraph has.

### Hyperedge List - `elist`

This property is a list where every entry represents a hyperedge.

For an unoriented hypergraph, a hyperedge is just a vector of the
vertices contained within the hyperedge. Each vertex is represented as a
string.

For an oriented hypergraph, each hyperedge is itself a list of two
vectors. Each of these vectors contains strings representing the
vertices contained in one end of the hyperedge.

For a directed hypergraph, each hyperedge is also a list of two vectors.
In the directed case, the first vector represents the vertices contained
in the tail of the hyperedge and the second the vertices contained in
the head. These two entries are also named `from` and `to`.

### Vertex Names - `vnames`

This a vector of strings representing the name of each vertex. On a
technical point, this is used as a reference for levels when factoring
multiple groups of vertices.

### Vertex Weights - `vweights`

This is a vector of weights associated with each vertex. This has little
practical use in this hypR implementation except to keep track of
hyperedge weights when transforming a hypergraph into its dual.

### Hyperedge Names - `enames`

This a vector of strings representing the name of each hyperedge On a
technical point, this is used as a reference for levels when factoring
multiple groups of hyperedges.

### Hyperedge Weights - `eweights`

This is a vector of weights associated with each hyperedge.

### Weighted - `weighted`

This is a logical value indicating whether there are weights associated
with the vertices or hyperedges.

### Oriented - `oriented`

This is a logical value indicating whether the hyperedges are oriented.

### Directed - `directed`

This is a logical value indicating whether the hyperedges are directed.

### Real Coefficients - `real_coef`

This is a logical vector indicating whether there are real coefficients
associating the vertices to the hyperedges.

### Incidence Matrix - `inc_mat`

This is only used for hypergraphs with real coefficients.

For unoriented hypergraphs this is a matrix with a row for every vertex
and a column for every hyperedge, where each entry represents the value
with which the vertex and hyperedge are associated.

For oriented hypergraphs, `inc_mat` is a list of two incidence matrices,
with one representing incidence to one side of the hyepredge and the
other representing incidence to the other.

For directed hypergraphs, `inc_mat` is a list of two incidence matrices,
with the first representing incidence to the tail of the hyperedge and
the second representing incidence to the head of the hyperedge.
