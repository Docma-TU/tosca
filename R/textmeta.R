#' "textmeta"-Objects
#'
#' Creates, Tests, Summarises and Plots Textmeta-Objects
#'
#' @param meta Data.frame (or matrix) of the meta-data
#' @param text List (or character vector) of the text-data
#' @param metamult List of the metamult-data
#' @param dateFormat Charachter string with the date format in meta
#' for \code{\link{as.Date}}
#' @return A \code{textmeta} object.
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
#' print(corpus)
#' summary(corpus)
#' str(corpus)
#'
#' @import graphics
#' @import grDevices
#' @import stats
#' @import utils
#' @import tm
#' @import lda
# #' @import lubridate
#' @export textmeta
textmeta <- function(meta = NULL, text = NULL, metamult = NULL, dateFormat = "%Y-%m-%d"){
  if(!is.null(meta)){
    if(!is.data.frame(meta)) meta <- as.data.frame(meta, stringsAsFactors = FALSE)
    meta$date <- as.Date(meta$date, format = dateFormat)
  }
  if(!is.null(text) && !is.list(text)) text <- as.list(text)
  stopifnot(is.null(meta) || is.data.frame(meta),
            is.null(text) || is.list(text),
            is.null(metamult) || is.list(metamult))
  res <- list(meta = meta, text = text, metamult = metamult)
  class(res) <- "textmeta"
  if (!is.textmeta(res)) stop("One of the components does not meet the requirements of a textmeta object.")

  return(res)
}

#' @rdname textmeta
#' @param x an R Object.
#' @export
is.textmeta <- function(x){
  isMeta <- function(x){
    return(
      all(
        is.data.frame(x), all(c("id", "date", "title") %in% colnames(x)),
        is.character(x$id), lubridate::is.Date(x$date), is.character(x$title)))
  }
  return(
    all(
      class(x) == "textmeta",
      is.null(x$meta) || isMeta(x$meta),
      is.null(x$text) || is.list(x$text) || is.character(x$text),
      is.null(x$metamult) || is.list(x$metamult)))
}

#' @rdname textmeta
#' @export
print.textmeta <- function(x, ...){
  stopifnot(is.textmeta(x))
  cat("Object of class \"textmeta\":\n")
  # x$text:
  cat(paste(" number of observations in text:", length(x$text), "\n"))
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

#' @rdname textmeta
#' @export
#' @param object textmeta object
#' @param listnames Character vector with names of textmeta lists (meta, text, metamult). Summaries are generated for those lists only. Default gives summaries for all lists.
#' @param metavariables Character vector with variable-names from the meta dataset. Summaries are generated for those variables only.
summary.textmeta <- function(object, listnames = names(object),
                             metavariables = character(), ...){
  stopifnot(is.textmeta(object), is.character(listnames),
            all(listnames %in% names(object)),
            ifelse("text" %in% listnames && length(object$text) > 0,
                   is.list(object$text) || is.character(object$text), TRUE),
            ifelse("meta" %in% listnames && !is.null(nrow(object$meta)),
                   is.data.frame(object$meta), TRUE),
            ifelse("metamult" %in% listnames && !is.null(nrow(object$metamult)),
                   is.list(object$metamult), TRUE))
  nextprint <- paste(paste0(rep("-", 70), collapse = ""), "\n\n")
  # object$text:
  if ("text" %in% listnames && length(object$text) > 0){
    n.text <- length(object$text)
    na.text <- sum(is.na(object$text))
    # print:
    cat(paste("number of observations in text:", n.text, "\n"))
    cat("\nNAs in text:\n")
    print(data.frame(NA.abs = na.text,
                     NA.rel = round(na.text/n.text, 2)), row.names = FALSE)
  }
  # object$meta:
  if ("meta" %in% listnames && !is.null(nrow(object$meta))){
    cols <- colnames(object$meta)
    n.meta <- nrow(object$meta)
    na.meta <- sapply(object$meta, function(x) sum(is.na(x)))
    # print:
    cat(paste0(nextprint, "meta: ", nrow(object$meta), " observations of ",
               ncol(object$meta), " variables\n"))
    cat("\nNAs in meta:\n")
    print(cbind(abs = na.meta, rel = round(na.meta/n.meta, 2)))
    # print tables (not NA-table) of metavariables
    for (i in metavariables){
      if (i %in% cols){
        tab <- table(object$meta[, i])
        cat(paste0(nextprint, i, ":\n"))
        print(cbind(abs = tab, rel = round(tab/n.meta, 2)))
      }
    }
    # print date-range:
    if ("date" %in% cols){
      cat(nextprint)
      if (na.meta["date"] < n.meta){
        cat(paste0("range of date: ",
                   paste(range(object$meta$date, na.rm = TRUE),
                         collapse = " till "), "\n"))
      }
      if (na.meta["date"] > 0){
        cat(paste0("NAs in date: ", na.meta["date"], " (",
                   round(na.meta["date"]/n.meta, 2), ")\n"))
      }
    }

  }
  if ("metamult" %in% listnames && length(object$metamult) > 0){
    n.metamult <- sapply(object$metamult, function(x) sum(lengths(x)))
    na.metamult <- sapply(object$metamult, function(x) sum(is.na(x)))
    cat(paste0(nextprint, "metamult:\n"))
    print(cbind(n = n.metamult, NA.abs = na.metamult,
                NA.rel = ifelse(n.metamult > 0, round(na.metamult/n.metamult, 2), 0)))
  }
  invisible(object)
}

#' @rdname textmeta
#' @param ... further arguments in plot. Not implemented for print and summary.
#' @export
plot.textmeta <- function(x, ...){
  plotScot(object = x, ...)
}
