#' XXXX
#'
#' `XXX` creates a *specification* of a recipe step
#'  that will ....
#'
#' @param ... One or more selector functions to choose which
#'  variables will be used to compute the components. See
#'  [selections()] for more details. For the `tidy`
#'  method, these are not currently used.
#' @param role For model terms created by this step, what analysis
#'  role should they be assigned?. By default, the function assumes
#'  that the new principal component columns created by the original
#'  variables will be used as predictors in a model.
#' @param options A list of options to the default method for
#'  [XXXX].
#' @param prefix A character string that will be the prefix to the
#'  resulting new variables.
#' @param ref_dist placeholder
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected), `value` (the
#'  loading), and `component`.
#' @keywords datagen
#' @concept preprocessing
#' @concept dwt
#' @concept projection_methods
#' @importFrom recipes add_step step terms_select ellipse_check check_name
#' @importFrom recipes rand_id bake prep
#' @importFrom tibble as_tibble tibble
#' @export
#' @details
#' XXXX
#'
#' @references Add reference
#'
#' @examples
#' XXX

step_hdoutliers <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  ref_dist = NULL,
  outlier_cutoff_threshold = .01,
  k_neighbours = 10,
  knnsearchtype = "brute",
  normalize_methood = "unitze",
  candidate_proportion = .5,
  threshold_sample_size = 50,
  options = list(),
  skip = FALSE,
  id = recipes::rand_id("hdout")
) {
  ## The variable selectors are not immediately evaluated by using
  ##  the `quos()` function in `rlang`. `ellipse_check()` captures
  ##  the values and also checks to make sure that they are not empty.
  terms <- recipes::ellipse_check(...)

  recipes::add_step(
    recipe,
    step_hdoutliers_new(
      terms = terms,
      trained = trained,
      ref_dist = ref_dist,
      outlier_cutoff_threshold = outlier_cutoff_threshold,
      k_neighbours = k_neighbours,
      knnsearchtype = knnsearchtype,
      normalize_methood = normalize_methood,
      candidate_proportion = candidate_proportion,
      threshold_sample_size = threshold_sample_size,
      options = options,
      skip = skip,
      id = id
    )
  )
}

step_hdoutliers_new <-
  function(terms,
           trained,
           ref_dist,
           outlier_cutoff_threshold,
           k_neighbours,
           knnsearchtype,
           normalize_methood,
           candidate_proportion,
           threshold_sample_size,
           options,
           skip,
           id) {
    recipes::step(
      subclass = "hdoutliers",
      terms = terms,
      trained = trained,
      ref_dist = ref_dist,
      outlier_cutoff_threshold = outlier_cutoff_threshold,
      k_neighbours = k_neighbours,
      knnsearchtype = knnsearchtype,
      normalize_methood = normalize_methood,
      candidate_proportion = candidate_proportion,
      threshold_sample_size = threshold_sample_size,
      options = options,
      skip = skip,
      id = id
    )
  }


