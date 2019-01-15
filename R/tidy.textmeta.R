#' Transform textmeta to an object with tidy text data
#'
#' Transfers data from a text component of a \code{\link{textmeta}} object to a
#' tidy data.frame.
#'
#' @param object A \code{\link{textmeta}} object
#' @return An object with tidy text data
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
#' tidy.textmeta(obj)
#' 
#' obj <- cleanTexts(obj)
#' tidy.textmeta(obj)
#' @export tidy.textmeta

tidy.textmeta <- function(object){
  stopifnot(is.textmeta(object))
  emptyText <- lengths(object$text) == 0
  if (any(emptyText)){
    message("Deleting ", sum(emptyText),  " empty texts...")
    object$text <- object$text[!emptyText]
    object$meta <- object$meta[object$meta %in% names(object$text)]
  }
  if (all(lengths(object$text) == 1)){
    dat <- data.frame(id = names(object$text),
      text = unlist(object$text), stringsAsFactors = FALSE)
  }else{
    dat <- data.frame(id = rep(names(object$text), times = lengths(object$text)),
      tokenid = unlist(lapply(object$text, seq_along)),
      token = unlist(object$text), stringsAsFactors = FALSE)
  }
  row.names(dat) <- 1:nrow(dat)
  object$text <- dat
  class(object) <- "textmeta_tidy"
  return(object)
}

#' @rdname tidy.textmeta
#' @param x an R Object.
#' @export
is.textmeta_tidy <- function(x){
  isMeta <- function(x){
    return(
      all(
        is.data.frame(x), all(c("id", "date", "title") %in% colnames(x)),
        is.character(x$id), lubridate::is.Date(x$date), is.character(x$title)))
  }
  isTidyText <- function(x){
    return(
      all(
        is.data.frame(x), ncol(x) == 2, all(c("id", "text") %in% colnames(x)),
        is.character(x$id), is.character(x$text)))
  }
  isTidyToken <- function(x){
    return(
      all(
        is.data.frame(x), ncol(x) == 3, all(c("id", "tokenid", "token") %in% colnames(x)),
        is.character(x$id), is.character(x$token), is.numeric(x$tokenid)))
  }
  return(
    all(
      class(x) == "textmeta_tidy",
      is.null(x$meta) || isMeta(x$meta),
      is.null(x$text) || isTidyText(x$text) || isTidyToken(x$text),
      is.null(x$metamult) || is.list(x$metamult)))
}

#' @rdname tidy.textmeta
#' @param ... further arguments passed to or from other methods.
#' @export
print.textmeta_tidy <- function(x, ...){
  stopifnot(is.textmeta_tidy(x))
  cat("Object of class \"textmeta_tidy\":\n")
  # x$text:
  cat(paste(" number of observations in text:", length(unique(x$text$id)), "\n"))
  # x$meta:
  if (!is.null(nrow(x$meta)) && !is.null(ncol(x$meta))){
    cat(paste0(" meta: ", nrow(x$meta), " observations of ", ncol(x$meta),
      " variables\n"))
  }
  if ("metamult" %in% names(x) && length(x$metamult) > 0){
    cat(paste0(" metamult: ", sum(lengths(x$metamult[[1]])),
      " observations of ", length(x$metamult), " variables\n"))
  }
  # date-range:
  if ("date" %in% colnames(x$meta)){
    na.date <- is.na(x$meta$date)
    if (any(!na.date)){
      cat(paste0(" range of date: ", paste(range(x$meta$date, na.rm = TRUE),
        collapse = " till ")), "\n")
    }
    if (any(na.date)){
      cat(paste0(" NAs in date: ", sum(na.date), " (",
        round(mean(na.date), 2), ")\n"))
    }
  }
  
  invisible(x)
}
