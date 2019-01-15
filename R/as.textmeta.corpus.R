#' Transform corpus to textmeta
#'
#' Transfers data from a \code{\link[quanteda]{corpus}} object -  the way text
#' data is stored in the package \code{\link[quanteda]{quanteda}} - to a
#' \code{\link{textmeta}} object.
#'
#' @param corpus Object of class \code{\link[quanteda]{corpus}},
#' package \code{\link[quanteda]{quanteda}}.
#' @param cols Character: vector with columns which should be kept.
#' @param dateFormat Character: string with the date format in the
#' date column for \code{\link{as.Date}}.
#' @param idCol Character: string with column name of the IDs in corpus
#' - named "id" in the resulting data.frame.
#' @param dateCol Character: string with column name of the Dates in corpus
#' - named "date" in the resulting data.frame.
#' @param titleCol Character: string with column name of the Titles in corpus
#' - named "title" in the resulting data.frame.
#' @param textCol Character: string with column name of the Texts in corpus
#' - results in a named list ("id") of the Texts.
#' @param duplicateAction Logical:
#' Should \code{\link{deleteAndRenameDuplicates}} be applied to the
#' created \code{\link{textmeta}} object?
#' @param addMetadata Logical: Should the metadata flag of corpus
#' be added to the meta flag of the \code{\link{textmeta}} object? If there are
#' conflicts regarding the naming of columns, the metadata columns would be
#' overwritten by the document specific columns.
#' @return \code{\link{textmeta}} object
#' @keywords manip
#' @examples
#' texts <- c("Give a Man a Fish, and You Feed Him for a Day.
#'  Teach a Man To Fish, and You Feed Him for a Lifetime",
#'  "So Long, and Thanks for All the Fish",
#'  "A very able manipulative mathematician, Fisher enjoys a real mastery
#'  in evaluating complicated multiple integrals.")
#'
#' corp <- quanteda::corpus(x = texts)
#' obj <- as.textmeta.corpus(corp, addMetadata = FALSE)
#' 
#' quanteda::docvars(corp, "title") <- c("Fishing", "Don't panic!", "Sir Ronald")
#' quanteda::docvars(corp, "date") <- c("1885-01-02", "1979-03-04", "1951-05-06")
#' quanteda::docvars(corp, "id") <- c("A", "B", "C")
#' quanteda::docvars(corp, "additionalVariable") <- 1:3
#' 
#' obj <- as.textmeta.corpus(corp)
#' @export as.textmeta.corpus

as.textmeta.corpus <- function(corpus, cols, dateFormat = "%Y-%m-%d", idCol = "id",
  dateCol = "date", titleCol = "title", textCol = "texts", duplicateAction = TRUE,
  addMetadata = TRUE){

  # stop if parameters set wrong
  stopifnot(quanteda::is.corpus(corpus),
    is.logical(duplicateAction), is.character(dateFormat),
    is.character(idCol), is.character(dateCol), is.character(titleCol),
    is.character(textCol), length(dateFormat) == 1, length(idCol) == 1,
    length(dateCol) == 1, length(titleCol) == 1, length(textCol) == 1,
    length(duplicateAction) == 1)

  if(missing(cols)) cols <- colnames(corpus$documents)
  cols <- setdiff(cols, c(idCol, dateCol, titleCol, textCol))

  if(!(idCol %in% colnames(corpus$documents))){
    message(paste0("NOTE: No ID-column \"", idCol, "\", set to ascending numbers"))
    meta <- data.frame(id = paste0("ID-", 1:nrow(corpus$documents)),
      stringsAsFactors = FALSE)
  }else{
    meta <- data.frame(id = as.character(corpus$documents[, idCol]),
      stringsAsFactors = FALSE)
  }
  tmpid <- meta$id

  if(addMetadata){
    tmp <- unlist(corpus$metadata)
    meta[, names(corpus$metadata)] <- matrix(rep(tmp, each = length(tmpid)), nrow = length(tmpid))
  }

  if(length(cols) > 0){
    meta[, cols] <- corpus$documents[, cols]
    for(i in 1:ncol(meta)){
      if(is.factor(meta[, i])){
        meta[, i] = as.character(meta[, i])
      }
    }
  }
  meta$id <- tmpid

  if(!(dateCol %in% colnames(corpus$documents))){
    message(paste0("NOTE: No date-column \"", dateCol, "\", set to NA"))
    meta$date <- as.Date(NA)
  }else{
    meta$date <- as.Date(corpus$documents[, dateCol], format = dateFormat)
  }

  if(!(titleCol %in% colnames(corpus$documents))){
    message(paste0("NOTE: No title-column \"", titleCol, "\", set to NA"))
    meta$title <- NA_character_
  }else{
    meta$title <- as.character(corpus$documents[, titleCol])
  }

  if(!(textCol %in% colnames(corpus$documents))){
    message(paste0("NOTE: No text-column \"", textCol, "\", set to NA"))
    text <- as.list(rep(NA_character_, nrow(meta)))
  }else{
    text <- as.list(as.character(corpus$documents[, textCol]))
  }
  names(text) <- meta$id

  # create textmeta
  res <- list("meta" = meta, "text" = text, "metamult" = NULL)
  class(res) <- "textmeta"

  # remove duplicates
  if(duplicateAction) res <- deleteAndRenameDuplicates(res, paragraph = FALSE)

  # print summary to console
  summary(res)
}
