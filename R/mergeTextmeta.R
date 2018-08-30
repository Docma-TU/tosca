#' Merge Textmeta Objects
#'
#' Merges a list of textmeta objects to a single object. It is possible to control whether all
#' columns or the intersect should be considered.
#'
#'
#' @param x A list of \code{\link{textmeta}} objects
#' @param all Logical: Should the result contain
#' \code{\link{union}} (\code{TRUE}) or \code{\link{intersection}} (\code{FALSE}) of columns of
#' all objects? If \code{TRUE}, the columns which at least appear in one of
#' the meta components are filled with \code{NA}s in the merged meta component.
#' @return \code{\link{textmeta}} object
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
#' corpus2 <- textmeta(meta=data.frame(id=c("E", "F"),
#' title=c("title1", "title2"),
#' date=c("2018-01-01", "2018-01-01"),
#' additionalVariable2=1:2, stringsAsFactors=FALSE), text=list(E="text1", F="text2"))
#'
#' merged <- mergeTextmeta(x=list(corpus, corpus2), all = TRUE)
#' str(merged$meta)
#'
#' merged <- mergeTextmeta(x=list(corpus, corpus2), all = FALSE)
#' str(merged$meta)
#' @export mergeTextmeta
#'

mergeTextmeta <- function(x, all = TRUE){
  stopifnot(is.list(x), all(sapply(x, is.textmeta)),
    is.logical(all), length(all) == 1)
  if (any(duplicated(unlist(lapply(x, function(y) names(y$text))))))
    message(paste0("NOTE: There are duplicates in the names of texts",
      ", could result in problems with unambiguity."))
  if (all) cols <- Reduce(union, lapply(x, function(y) colnames(y$meta)))
  else cols <- Reduce(intersect, lapply(x, function(y) colnames(y$meta)))
  meta = NULL
  if (length(cols) > 0){
    ind <- which(sapply(x, function(y) is.data.frame(y$meta)))
    if (length(ind) > 1){
      meta <- (x[[ind[1]]])$meta
      for (i in 2:length(ind))
        meta <- merge(meta, (x[[ind[i]]])$meta, all = TRUE, sort = FALSE)
    }
    else meta <- (x[[ind]])$meta # only one data.frame
  }
  object <- textmeta(meta = meta[, cols], text = Reduce(c, lapply(x, function(y) y$text)),
    metamult = Reduce(c, lapply(x, function(y) y$metamult)))
  return(object)
}

# mergeTextmeta <- function(t1, t2, all = TRUE){
#   stopifnot(is.textmeta(t1), is.textmeta(t2), is.logical(all), length(all) == 1)
#   if (all) cols <- union(colnames(t1$meta), colnames(t2$meta))
#   else cols <- intersect(colnames(t1$meta), colnames(t2$meta))
#   if (length(cols) > 0){
#     if (is.null(t1$meta)){
#       if (is.null(t2$meta)) meta = NULL
#       else meta = t2$meta[, cols]
#     }
#     else{
#       if (is.null(t2$meta)) meta = t1$meta[, cols]
#       else meta = merge(t1$meta, t2$meta, all = TRUE, sort = FALSE)[, cols]
#     }
#   }
#   else meta = NULL
#   object <- textmeta(meta = meta, text = c(t1$text, t2$text),
#     metamult = c(t1$metamult, t2$metamult))
#   return(object)
# }
