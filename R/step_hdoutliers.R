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
#' @importFrom recipes rand_id bake prep printer
#' @importFrom tibble as_tibble tibble
#' @importFrom stray find_HDoutliers
#' @export
#' @details
#' Some text here
#'
#' @references Add reference
#'
#' @examples
#' x <- 1

step_hdoutliers <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  outlier_bounds = NULL,
  outlier_cutoff_threshold = .01,
  k_neighbours = 10,
  knnsearchtype = "brute",
  normalize_method = "unitize",
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
      outlier_bounds = outlier_bounds,
      outlier_cutoff_threshold = outlier_cutoff_threshold,
      k_neighbours = k_neighbours,
      knnsearchtype = knnsearchtype,
      normalize_method = normalize_method,
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
           outlier_bounds,
           outlier_cutoff_threshold,
           k_neighbours,
           knnsearchtype,
           normalize_method,
           candidate_proportion,
           threshold_sample_size,
           options,
           skip,
           id) {
    recipes::step(
      subclass = "hdoutliers",
      terms = terms,
      trained = trained,
      outlier_bounds = outlier_bounds,
      outlier_cutoff_threshold = outlier_cutoff_threshold,
      k_neighbours = k_neighbours,
      knnsearchtype = knnsearchtype,
      normalize_method = normalize_method,
      candidate_proportion = candidate_proportion,
      threshold_sample_size = threshold_sample_size,
      options = options,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_hdoutliers <- function(x, training, info = NULL, ...) {
  col_names <- recipes::terms_select(terms = x$terms, info = info)
  ref_dist <- training[,col_names]

  args <- list(
    data = ref_dist,
    alpha = x$outlier_cutoff_threshold,
    k = x$k_neighbours,
    knnsearchtype = x$knnsearchtype,
    normalize = x$normalize_method,
    p = x$candidate_proportion,
    tn = x$threshold_sample_size
  )

  outliers_call <- rlang::call2("find_HDoutliers", !!!args, .ns = "stray")
  outliers <- eval(outliers_call)
  scores <- outliers$out_scores

  args <- list(
    outlier_score = scores,
    alpha = x$outlier_cutoff_threshold,
    outtail = "min",
    p = x$candidate_proportion,
    tn = x$threshold_sample_size
  )

  lower_bound_call <- rlang::call2(return_outlier_bound, !!!args)
  lower_bound <- eval(lower_bound_call)
  args$outtail <- "max"
  upper_bound_call <- rlang::call2(return_outlier_bound, !!!args)
  upper_bound <- eval(upper_bound_call)

  outlier_bounds <- tibble::tibble(upper_bound = upper_bound, lower_bound = lower_bound)

  step_hdoutliers_new(
    terms = x$terms,
    trained = TRUE,
    outlier_bounds = outlier_bounds,
    outlier_cutoff_threshold = x$outlier_cutoff_threshold,
    k_neighbours = x$k_neighbours,
    knnsearchtype = x$knnsearchtype,
    normalize_method = x$normalize_method,
    candidate_proportion = x$candidate_proportion,
    threshold_sample_size = x$threshold_sample_size,
    options = x$options,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_dwt <- function(object, new_data, ...) {
  col_names <- recipes::terms_select(terms = object$terms)
  new_data_used <- new_data[,col_names]

  args <- list(
    data = new_data_used,
    alpha = object$outlier_cutoff_threshold,
    k = object$k_neighbours,
    knnsearchtype = object$knnsearchtype,
    normalize = object$normalize_method,
    p = object$candidate_proportion,
    tn = object$threshold_sample_size
  )

  outliers_call <- rlang::call2("find_HDoutliers", !!!args, .ns = "stray")
  outliers_raw <- eval(outliers_call)
  new_outlier_scores <- outliers_raw$out_scores
  excl_indexes <- c(
    which(new_outlier_scores < object$outlier_bounds$lower_bound),
    which(new_outlier_scores > object$outlier_bounds$upper_bound)
  )
  new_data  <- new_data[-excl_indexes,]

  return(tibble::as_tibble(new_data))
}

#' @export
print.step_hdoutliers <- function (x, width = max(20, options()$width - 31), ...)
{
  cat("HDOutliers Transformation for ", sep = "")
  recipes::printer(names(x$models), x$terms, x$trained, width = width)
  invisible(x)
}
