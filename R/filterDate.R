#' Subcorpus With Date Filter
#'
#' Generates a subcorpus by restricting it to a specific time window.
#'
#' @param ... Not used.
#' @param object \code{\link{textmeta}} object
#' @param text Not necessary if \code{object} is specified, else should be
#' \code{object$text}
#' @param meta Not necessary if \code{object} is specified, else should be
#' \code{object$meta}
#' @param s.date Start date of subcorpus as date object
#' @param e.date End date of subcorpus as date object
#' @param filtermeta Logical: Should the meta component be filtered, too?
#' @return \code{\link{textmeta}} object if \code{object} is specified,
#' else only the filtered \code{text}. If a \code{\link{textmeta}} object is
#' returned its meta data are filtered to those texts which appear in the corpus
#' by default (\code{filtermeta}).
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
filterDate <- function(...) UseMethod("filterDate")

#' @rdname filterDate
#' @export
filterDate.default <- function(text, meta,
  s.date = min(meta$date, na.rm = TRUE),
  e.date = max(meta$date, na.rm = TRUE), ...){
  
  stopifnot(is.textmeta(textmeta(meta = meta, text = text)),
    length(s.date) == 1, length(e.date) == 1)
  
  meta <- meta[match(names(text), meta$id),]
  ind <- meta$date >= s.date & meta$date <= e.date
  ind[is.na(ind)] <- FALSE
  meta <- meta[ind, ]
  
  text <- text[match(meta$id, names(text))]
  
  invisible(text)
}

#' @rdname filterDate
#' @export
filterDate.textmeta <- function(object,
  s.date = min(object$meta$date, na.rm = TRUE),
  e.date = max(object$meta$date, na.rm = TRUE),
  filtermeta = TRUE, ...){

  stopifnot(is.textmeta(object), is.logical(filtermeta), length(filtermeta) == 1)
  
  text <- NextMethod("filterDate", object = object$text, text = object$text,
    meta = object$meta, s.date = s.date, e.date = e.date)
  
  object$text <- text
  if(filtermeta){
    object$meta <- object$meta[object$meta$id %in% names(object$text),]
  }
  return(object)
}
