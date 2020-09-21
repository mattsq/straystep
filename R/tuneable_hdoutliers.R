#' Nearest Neighbour Search Methods
#'
#' @param values A character string of possible values. See `values_knn_search_type`
#'  in examples below.
#'
#' @details
#' This parameter is used in `straystep::step_hdoutliers`.
#' @examples
#' values_knn_search_type
#' knn_search_type()
#' @importFrom dials new_qual_param
#' @export
knn_search_type <- function(values = values_knn_search_type) {
  dials::new_qual_param(
    type     = c("character"),
    values   = values,
    default  = "brute",
    label    = c(knnsearchtype = "Nearest Neigbour Search Method"),
    finalize = NULL
  )
}

#' @export
tunable.step_hdoutliers <- function(x, ...) {
  tibble::tibble(
    name = c("outlier_cutoff_threshold",
             "k_neighbours",
             "knnsearchtype",
             "candidate_proportion",
             "threshold_sample_size"),
    call_info = list(
      list(pkg = "dials", fun = "threshold"),
      list(pkg = "dials", fun = "neighbors"),
      list(pkg = "straystep", fun = "knn_search_type"),
      list(pkg = "dials", fun = "threshold"),
      list(pkg = "dials", fun = "sample_size")
      ),
    source = "recipe",
    component = "step_hdoutliers",
    component_id = x$id
  )
}
#' @rdname knn_search_type
#' @export
values_knn_search_type <- c("brute", "kd_tree", "cover_tree")
