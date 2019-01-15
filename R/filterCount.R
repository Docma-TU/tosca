#' Subcorpus With Count Filter
#'
#' Generates a subcorpus by restricting it to texts containing a specific number of words
#'
#' @param object A \code{\link{textmeta}} object
#' @param text Not necassary if \code{object} is specified, else should be
#' \code{object$text}: list of article texts
#' @param count An integer marking how many words must at least be found in the text.
#' @param out Type of output: \code{text} filtered corpus,
#' \code{bin} logical vector for all texts,
#' \code{count} the counts.
#' @return \code{\link{textmeta}} object if \code{object} is specified,
#' else only the filtered \code{text}. If a \code{\link{textmeta}} object is
#' returned its meta data are filtered to those texts which appear in the corpus.
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#'
#' filterCount(text=texts, count=10L)
#'
#' filterCount(text=texts, count=10L, out="bin")
#'
#' filterCount(text=texts, count=10L, out="count")
#' @export filterCount
filterCount <- function(object, text, count = 1L, out = c("text", "bin", "count")){

  returnTextmeta <- FALSE
  if (!missing(object)){
    stopifnot(is.textmeta(object))
    text <- object$text
    returnTextmeta <- TRUE
  }

  stopifnot(is.textmeta(textmeta(text = text)), as.integer(count) == count,
    all(out %in% c("text", "bin", "count")))

  counts <- stringr::str_count(unlist(lapply(lapply(text, unlist),
    function(x) paste(x, collapse = " "))), pattern = "\\b[a-z,A-Z](.*?)\\b")

  subid <- counts >= count
  subid[is.na(subid)] <- FALSE
  if(out[1] == "text"){
    if(returnTextmeta){
      object$text <- text[subid]
      object$meta <- object$meta[object$meta$id %in% names(object$text), ]
      return(object)
    }
    return(text[subid])
  }
  if(out[1] == "bin") return(subid)
  if(out[1] == "count") return(counts)
}
