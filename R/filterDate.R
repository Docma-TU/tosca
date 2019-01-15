#' Subcorpus With Date Filter
#'
#' Generates a subcorpus by restricting it to a specific time window
#'
#' @param object \code{\link{textmeta}} object
#' @param text Not necessary if \code{object} is specified, else should be
#' \code{object$text}
#' @param meta Not necessary if \code{object} is specified, else should be
#' \code{object$meta}
#' @param s.date Start date of subcorpus as date object
#' @param e.date End date of subcorpus as date object
#' @return \code{\link{textmeta}} object if \code{object} is specified,
#' else only the filtered \code{text}. If a \code{\link{textmeta}} object is
#' returned its meta data are filtered to those texts which appear in the corpus.
#' @keywords manip
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#'
#' corpus <- textmeta(meta=data.frame(id=c("A", "B", "C", "D"),
#' title=c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
#' date=c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
#' additionalVariable=1:4, stringsAsFactors=FALSE), text=texts)
#'
#' subcorpus <- filterDate(object=corpus, s.date = "1951-05-06")
#' subcorpus$meta
#' subcorpus$text
#' @export filterDate
filterDate <- function(object, text = object$text, meta = object$meta,
  s.date = min(meta$date, na.rm = TRUE),
  e.date = max(meta$date, na.rm = TRUE)){

  returnTextmeta <- TRUE
  if(missing(object)){
    object <- textmeta(meta = meta, text = text)
    returnTextmeta <- FALSE
  }
  stopifnot(is.textmeta(object), length(s.date) == 1, length(e.date) == 1)
  object$meta <- object$meta[match(names(object$text), object$meta$id),]
  ind <- object$meta$date >= s.date & object$meta$date <= e.date
  ind[is.na(ind)] <- FALSE
  object$meta <- object$meta[ind, ]
  object$text <- object$text[match(object$meta$id, names(object$text))]
  if(returnTextmeta) return(object)
  return(object$text)
}
