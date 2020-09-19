# this is a bit of a hack - took the code from stray that returns outlier indexes,
# but use it instead to calcuate the outlier bound score
# the theory here being that we use that bound from the training data
# to apply to test data, to reduce target leakage

return_outlier_bound <- function(outlier_score, alpha, outtail = c("max", "min"), p, tn) {

  n <- length(outlier_score)
  if(outtail == "min")
  {
    outlier_score <- - outlier_score
  }

  ord <- order(outlier_score)
  gaps <- c(0, diff(outlier_score[ord]))
  n4 <- max(min(tn, floor(n / 4)), 2)
  J <- 2:n4
  start <- max(floor(n * (1-p)), 1) + 1
  ghat <- numeric(n)
  for (i in start:n) ghat[i] <- sum((J / (n4 - 1)) * gaps[i - J + 1 ]) # check i - j +1
  logAlpha <- log(1 / alpha)
  bound <- Inf

  for (i in start:n) {
    if (gaps[i] > logAlpha * ghat[i]) {
      bound <- outlier_score[ord][i - 1]
      break
    }
  }
  return(bound)
}
