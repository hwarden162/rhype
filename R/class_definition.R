Hypergraph <- R6::R6Class(
  "Hypergraph",
  public = list(
    # Getters and Setters ---
    get_numv = function() {
      private$numv
    },
    set_numv = function(new_numv) {
      private$numv <- new_numv
    },
    get_elist = function() {
      private$elist
    },
    set_elist = function(new_elist) {
      private$elist <- new_elist
    },
    get_vnames = function() {
      private$vnames
    },
    set_vnames = function(new_vnames) {
      private$vnames <- new_vnames
    },
    get_vweights = function() {
      private$vweights
    },
    set_vweights = function(new_vweights) {
      private$vweights <- new_vweights
    },
    get_enames = function() {
      private$enames
    },
    set_enames = function(new_enames) {
      private$enames <- new_enames
    },
    get_eweights = function() {
      private$eweights
    },
    set_eweights = function(new_eweights) {
      private$eweights <- new_eweights
    },
    get_weighted = function() {
      private$weighted
    },
    set_weighted = function(new_weighted) {
      private$weighted <- new_weighted
    },
    get_oriented = function() {
      private$oriented
    },
    set_oriented = function(new_oriented) {
      private$oriented <- new_oriented
    },
    get_directed = function() {
      private$directed
    },
    set_directed = function(new_directed) {
      private$directed <- new_directed
    },
    get_real_coef = function() {
      private$real_coef
    },
    set_real_coef = function(new_real_coef) {
      private$real_coef <- new_real_coef
    },
    get_inc_mat = function() {
      private$inc_mat
    },
    set_inc_mat = function(new_inc_mat) {
      private$inc_mat <- new_inc_mat
    },
    # Constructor ---
    initialize = function(numv,
                          elist,
                          vnames = NULL,
                          vweights = NULL,
                          enames = NULL,
                          eweights = NULL,
                          weighted = FALSE,
                          oriented = FALSE,
                          directed = FALSE,
                          real_coef = FALSE,
                          inc_mat = NULL) {
      self$set_numv(numv)
      self$set_elist(elist)
      self$set_vnames(vnames)
      self$set_vweights(vweights)
      self$set_enames(enames)
      self$set_eweights(eweights)
      self$set_weighted(weighted)
      self$set_oriented(oriented)
      self$set_directed(directed)
      self$set_real_coef(real_coef)
      self$set_inc_mat(inc_mat)
    },
    # Overwrite Print ---
    print = function() {
      cat("Hypergraph Object:", "\n")
      cat("    Number of vertices: ", self$get_numv(), "\n")
      cat("    Number of hyperedges: ", length(self$get_elist()), "\n")
      cat("    Oriented: ", self$get_oriented())
      cat("    Directed: ", self$get_directed(), "\n")
      cat("    Real Coefficients: ", self$get_real_coef())
      cat("    Weighted: ", self$get_weighted(), "\n")
    }
  ),
  private = list(
    # Private Member Variables ---
    numv = NA,
    elist = NA,
    vnames = NULL,
    vweights = NULL,
    enames = NULL,
    eweights = NULL,
    weighted = FALSE,
    oriented = FALSE,
    directed = FALSE,
    real_coef = FALSE,
    inc_mat = NULL
  )
)

#' Print More Detail About a Hypergraph
#'
#' Get a more detailed printout of what is contained within a hypergraph object
#' to understand more about its structure as a whole without having to
#' repeatedly call other functions.
#'
#' This gives a more detailed look at the whole hypegraph object. It is
#' intended solely to  aid the user when using rhype and generally should not be
#' included in final scripts. If a user wants to include this in their final
#' script it is instead heavily encouraged that they use other rhype functions to
#' generate their own bespoke messages.
#'
#' @param hype A hypergraph object
#' @param numv A logical variable indicating whether information about the
#'     number of vertices should be printed
#' @param elist A logical variable indicating whether information about the
#'     hyperedge list should be printed
#' @param vnames A logical variable indicating whether information about the
#'     vertex names should be printed
#' @param vweights A logical variable indicating whether information about the
#'     vertex weights should be printed
#' @param enames A logical variable indicating whether information about the
#'     hyperedge names should be printed
#' @param eweights A logical variable indicating whether information about the
#'     hyperedge weights should be printed
#' @param weighted A logical variable indicating whether information about the
#'     hypergraph weighting should be printed
#' @param oriented A logical variable indicating whether information about the
#'     hypergraph orientation should be printed
#' @param directed A logical variable indicating whether information about the
#'     hypergraph direction should be printed
#' @param real_coef A logical variable indicating whether information about the
#'     hypergraph real coefficients should be printed
#' @param inc_mat A logical variable indicating whether information about the
#'     hypergraph incidence matrix should be printed
#'
#' @export
#'
#' @examples
#' hype1 <- example_hype()
#' hype_info(hype1)
#'
#' hype2 <- example_hype(vertex_weighted = TRUE, edge_weighted = TRUE)
#' hype_info(hype2)
#'
#' hype3 <- example_hype(oriented = TRUE, directed = TRUE, real_coef = TRUE)
#' hype_info(hype3)
hype_info <- function(hype,
                      numv = TRUE,
                      elist = TRUE,
                      vnames = TRUE,
                      vweights = TRUE,
                      enames = TRUE,
                      eweights = TRUE,
                      weighted = TRUE,
                      oriented = TRUE,
                      directed = TRUE,
                      real_coef = TRUE,
                      inc_mat = TRUE) {
  cat("====================HYPERGRAPH INFORMATION====================\n\n")

  # Check whether information on the vertices should be printed
  if (numv | vnames) {
    cat("--------------------VERTEX INFORMATION--------------------\n\n")
    # Print information on the number of vertices
    if (numv) {
      cat("This hypergraph has ", hype$get_numv(), " vertices\n\n")
    }
    # Print information on the vertex names
    if (vnames) {
      cat("These vertices are called:\n", paste0(hype$get_vnames(), collapse = ", "), "\n\n")
    }
  }

  # Check whether information on the hyperedges should be printed
  if (elist | enames) {
    cat("--------------------HYPEREDGE INFORMATION--------------------\n\n")
    # Print information on the hyperedges
    if (enames) {
      cat("The hyperedges are called:\n", paste0(hype$get_enames(), collapse = ", "), "\n\n")
    }
    # Print information on the hyperedge list
    if (elist) {
      cat("The hyperedges have the structure:\n")
      print(hype$get_elist())
    }
  }

  # Check whether information on hypergraph waitings should be printed
  if (weighted) {
    cat("---------------WEIGHTING INFORMATION--------------------\n\n")
    # Print hypergraph weightings
    if (hype$get_weighted()) {
      # Print whether the hypergraph is weighted
      cat("This hypergraph is weighted\n\n")
      # Print the hyperedge weights
      if (eweights) {
        cat("The hyperedges have weights:\n")
        print(hype$get_eweights())
        cat("\n")
      }
      # Print the vertex weights
      if (vweights) {
        cat("The vertices have weights:\n")
        print(hype$get_vweights())
        cat("\n")
      }
    } else {
      cat("This hypergraph is not weighted\n\n")
    }
  }

  # Check whether information on orientation is to be printed
  if (oriented) {
    cat("--------------------Orientation Information--------------------\n\n")

    # Print whether the hypergraph is oriented
    if (hype$get_oriented()) {
      cat("This hypergraph is oriented\n\n")
    } else {
      cat("This hypergraph is not oriented\n\n")
    }

    # Print whethe rthe hypergraph is directed
    if (hype$get_directed()) {
      cat("This hypergraph is directed\n\n")
    } else {
      cat("This hypergraph is not directed\n\n")
    }
  }

  # Check whether information on real coefficients should be printed
  if (real_coef) {
    cat("--------------------REAL COEFFICIENTS INFORMATION--------------------\n\n")
    # Print whether the hypergraph has real coefficients
    if (hype$get_real_coef()) {
      cat("This hypergraph has real coefficients associating vertices to hyperedges\n\n")
      # Check whether the incidence matrix should be printed and print it
      if (inc_mat) {
        cat("The incidence matrix associating vertices to hyperedges is given by:\n")
        print(hype$get_inc_mat())
      }
    } else {
      cat("This hypergraph does not have real coefficients associating vertices to hyperedges\n\n")
      cat("There is no incidence matrix associating vertices to hyperedges with non-binary coefficients\n\n")
    }
  }
}
