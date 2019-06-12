#' "meta" Component of "textmeta"-Objects
#'
#' Helper to create the requested data.frame to create a "textmeta" object.
#'
#' @param x data.frame to convert
#' @param cols \code{character} vector with columns which should be kept
#' @param idCol \code{character} string with column name of the IDs
#' @param dateCol \code{character} string with column name of the Dates
#' @param titleCol \code{character} string with column name of the Titles
#' @param dateFormat \code{character} string with the date format in x for \code{\link{as.Date}}.
#' If not supplied, dates are not transformed.
#' @return A data.frame with columns "id", "date", "title" and user-specified others.
#' @keywords manip
#' @examples
#' meta <- data.frame(id = 1:3, additionalVariable = matrix(5, ncol = 4, nrow = 3))
#' (as.meta(meta))
#'
#' @export as.meta
as.meta <- function(x, cols = colnames(x), idCol = "id", dateCol = "date", titleCol = "title", dateFormat){
  
  name <- deparse(substitute(x))
  
  stopifnot(is.character(idCol), is.character(dateCol), is.character(titleCol),
    length(idCol) == 1, length(dateCol) == 1, length(titleCol) == 1,
    is.data.frame(x))
  
  cols <- union(cols, c(idCol, dateCol, titleCol))
  x <- x[, colnames(x) %in% cols]
  
  if(!(idCol %in% colnames(x))){
    message(paste0("NOTE: No ID-column \"", idCol, "\" in ", name, ", set to ascending numbers"))
    x[, idCol] <- paste0("ID-", 1:nrow(x))
  }
  if(!(dateCol %in% colnames(x))){
    message(paste0("NOTE: No date-column \"", dateCol, "\" in ", name, ", set to NA"))
    x[, dateCol] <- NA
  }
  if(!(titleCol %in% colnames(x))){
    message(paste0("NOTE: No title-column \"", titleCol, "\" in ", name, ", set to NA"))
    x[, titleCol] <- NA
  }
  
  if(!missing(dateFormat)) x[, dateCol] <- as.Date(x[, dateCol], format = dateFormat)
  x[, idCol] <- as.character(x[, idCol])
  x[, titleCol] <- as.character(x[, titleCol])
  colnames(x)[colnames(x) == dateCol] <- "date"
  colnames(x)[colnames(x) == idCol] <- "id"
  colnames(x)[colnames(x) == titleCol] <- "title"
  
  # firstThreeCols = c("id", "date", "title")
  # x = x[,match(c(firstThreeCols, setdiff(colnames(x), firstThreeCols)), colnames(x))]
  x <- cbind(x[,c("id", "date", "title")], x[,!(colnames(x) %in% c("id", "date", "title"))])

  invisible(x)
}
