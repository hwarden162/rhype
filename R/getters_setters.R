hyperedge_list <- function(hype) {
  hype$get_elist()
}

vertex_names <- function(hype) {
  hype$get_vnames()
}

vertex_weights <- function(hype) {
  hype$get_vweights()
}

hyperedge_names <- function(hype) {
  hype$get_enames()
}

hyperedge_weights <- function(hype) {
  hype$get_eweights()
}

is_weighted <- function(hype) {
  hype$get_weighted()
}

is_oritented <- function(hype) {
  hype$get_oriented()
}

is_directed <- function(hype) {
  hype$get_directed()
}

has_real_coef <- function(hype) {
  hype$get_real_coef()
}




