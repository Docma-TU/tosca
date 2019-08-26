#' Precision and Recall
#'
#' Estimates Precision and Recall for sampling in different intersections
#' 
#' @param w Numeric vector: Each entry represents one intersection.
#' Proportion of texts in this intersection.
#' @param p Numeric vector: Each entry represents one intersection.
#' Proportion of relevant texts in this intersection.
#' @param subset Logical vector: Each entry represents one intersection.
#' Controls if the intersection belongs to the subcorpus of interest or not.
#' @param n Integer vector: Number of Texts labeled in the corresponding intersection.
#' @return Estimator for precision, recall, and their variances respectively.
#' @examples
#' w <- c(0.5, 0.1, 0.2, 0.2)
#' p <- c(0.01, 0.8, 0.75, 0.95)
#' subset <- c(FALSE, TRUE, FALSE, TRUE)
#' n <- c(40, 20, 15, 33)
#' precision(w, p, subset)
#' vprecision(w, p, subset, n)
#' recall(w, p, subset)
#' vrecall(w, p, subset, n)
#' 
#' @rdname precisionRecall
#' @export 

precision <- function(w, p, subset){
  stopifnot(
    is.numeric(w), round(sum(w),5)==1, all(w>=0),
    is.numeric(p), all(p>=0), all(p<=1),
    is.logical(subset),
    length(w) == length(p), length(p) == length(subset)
  ) 
  w <- w[subset] / sum(w[subset])
  sum(w*p[subset])
}

#' @rdname precisionRecall
#' @export

vprecision <- function(w, p, subset, n){
  stopifnot(
    is.numeric(w), round(sum(w),5)==1, all(w>=0),
    is.numeric(p), all(p>=0), all(p<=1),
    is.logical(subset),
    all(n == as.integer(n)), all(n>=0),
    length(w) == length(p), length(p) == length(subset), length(subset) == length(n)
  ) 
  sum((w^2*p*(1-p)/n)[subset])
}

#' @rdname precisionRecall
#' @export

recall <- function(w, p, subset){
  stopifnot(
    is.numeric(w), round(sum(w),5)==1, all(w>=0),
    is.numeric(p), all(p>=0), all(p<=1),
    is.logical(subset),
    length(w) == length(p), length(p) == length(subset)
  ) 
  sum((w*p)[subset]) / sum(w*p)
}
#1 / (1 + (sum((w*p)[!subset]) / sum((w*p)[subset])))}

#' @rdname precisionRecall
#' @export

vrecall <- function(w, p, subset, n){
  stopifnot(
    is.numeric(w), round(sum(w),5)==1, all(w>=0),
    is.numeric(p), all(p>=0), all(p<=1),
    is.logical(subset),
    all(n == as.integer(n)), all(n>=0),
    length(w) == length(p), length(p) == length(subset), length(subset) == length(n)
  ) 
  (sum((w*p)[!subset]) / sum(w*p)^2)^2 * sum((w^2*p*(1-p)/n)[ subset]) +
    (sum((w*p)[ subset]) / sum(w*p)^2)^2 * sum((w^2*p*(1-p)/n)[!subset])
}
