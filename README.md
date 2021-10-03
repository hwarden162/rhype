
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
