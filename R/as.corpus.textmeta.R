#' Transform textmeta to corpus
#'
#' Transfers data from a \code{\link{textmeta}} object to a
#' \code{\link[quanteda]{corpus}} object -  the way text data is stored in the
#' package \code{\link[quanteda]{quanteda}}.
#'
#' @param object \code{\link{textmeta}} object
#' @param docnames Character: string with the column of \code{object$meta}
#' which should be kept as \code{\link[quanteda]{docnames}}.
#' @param docvars Character: vector with columns of \code{object$meta} which
#' should be kept as \code{\link[quanteda]{docvars}}.
# @param metadoc Character: vector with columns of \code{object$meta} which should be kept as \code{\link[quanteda]{metadoc}}.
#' @param ... Additional parameters like \code{meta} or \code{compress} for \code{\link[quanteda]{corpus}}.
#' @return \code{\link[quanteda]{corpus}} object
#' @keywords manip
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#'  Teach a Man To Fish, and You Feed Him for a Lifetime",
#'  B="So Long, and Thanks for All the Fish",
#'  C="A very able manipulative mathematician, Fisher enjoys a real mastery
#'  in evaluating complicated multiple integrals.")
#'
#' obj <- textmeta(meta=data.frame(id=c("A", "B", "C", "D"),
#'  title=c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
#'  date=c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
#'  additionalVariable=1:4, stringsAsFactors=FALSE), text=texts)
#'
#' corp <- as.corpus.textmeta(obj)
#' quanteda::docvars(corp)
#' #quanteda::textstat_summary(corp)
#' @export as.corpus.textmeta

as.corpus.textmeta <- function(object, docnames = "id",
  docvars = setdiff(colnames(object$meta), "id"),
  #metadoc = character(),
  ...){

  # stop if parameters set wrong
  stopifnot(is.textmeta(object), is.character(docnames), length(docnames) == 1,
    is.character(docvars), #is.character(metadoc),
    all(union(docnames, docvars) %in% colnames(object$meta)))
  
  id <- object$meta[,docnames]
  texts <- sapply(object$text, paste, collapse = "\n\n")[match(id, names(object$text))]
  names(texts) = id
  vars <- as.data.frame(object$meta[,docvars], stringsAsFactors = FALSE)
  colnames(vars) <- docvars
  #meta <- as.data.frame(object$meta[,metadoc], stringsAsFactors = FALSE)
  #colnames(meta) <- metadoc

  corp <- quanteda::corpus(x = texts, docnames = id, docvars = vars, ...)
  #suppressWarnings(
  #  quanteda::docvars(corp) <- 
  #    cbind(quanteda::docvars(corp), quanteda::metadoc(corp))
  #)

  return(corp)
}
