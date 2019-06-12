#' Subcorpus With ID Filter
#'
#' Generates a subcorpus by restricting it to specific ids.
#'
#' @param ... Not used.
#' @param object A \code{\link{textmeta}} object
#' @param text Not necassary if \code{object} is specified, else should be
#' \code{object$text}: list of article texts
#' @param id Character: IDs the corpus should be filtered to.
#' @param filtermeta Logical: Should the meta component be filtered, too?
#' @return \code{\link{textmeta}} object if \code{object} is specified,
#' else only the filtered \code{text}. If a \code{\link{textmeta}} object is
#' returned its meta data are filtered to those texts which appear in the corpus
#' by default (\code{filtermeta}).
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#' 
#' meta <- data.frame(id = c("C", "B"), date = NA, title = c("Fisher", "Fish"),
#' stringsAsFactors = FALSE)
#' tm <- textmeta(text = texts, meta = meta)
#' 
#' filterID(texts, c("A", "B"))
#' filterID(texts, "C")
#' filterID(tm, "C")
#' filterID(tm, "B")
#' filterID(tm, c("B", "A"), FALSE)
#' @export filterID
filterID <- function(...) UseMethod("filterID")

#' @rdname filterID
#' @export
filterID.default <- function(text, id, ...){
  stopifnot(is.textmeta(textmeta(text = text)), is.character(id))
  text <- text[names(text) %in% id]
  invisible(text)
}

#' @rdname filterID
#' @export
filterID.textmeta <- function(object, id, filtermeta = TRUE, ...){
  stopifnot(is.textmeta(object), is.character(id), is.logical(filtermeta),
    length(filtermeta) == 1)
  object$text <- object$text[names(object$text) %in% id]
  if(filtermeta){
    object$meta <- object$meta[object$meta$id %in% names(object$text),]
  }
  return(object)
}
