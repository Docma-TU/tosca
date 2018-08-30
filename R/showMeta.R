#' Export Readable Meta-Data of Articles.
#'
#' Exports requested meta-data of articles for given id's.
#'
#'
#' @param meta A data.frame of meta-data as a result of a read-function.
#' @param id Character vector or matrix including article ids.
#' @param cols Character vector including the requested columns of meta.
#' @param file Character Filename for the export.
#' @return A list of the requested meta data. If file is set, writes a csv including the meta-data of the
#' requested meta data.
#' @keywords manip
#' @examples
#' meta <- data.frame(id=c("A", "B", "C", "D"),
#' title=c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
#' date=c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
#' additionalVariable=1:4, stringsAsFactors=FALSE)
#'
#' extractedMeta <- showMeta(meta=meta, cols = c("title", "date"))
#'
#' @export showMeta
#'
showMeta <- function(meta, id = meta$id, cols = colnames(meta), file){
  stopifnot(is.data.frame(meta), all(id %in% meta$id), is.character(cols),
    all(cols %in% colnames(meta)))
  more_files <- TRUE
  if(is.vector(id)){
    id <- as.matrix(id)
    more_files <- FALSE
    nameArg <- ""
  }
  else{
    if(is.null(colnames(id))) nameArg <- 1:ncol(id)
    else nameArg <- colnames(id)
  }
  if(is.null(colnames(id))) nameArg <- 1:ncol(id)
  else nameArg <- colnames(id)
  outlist <- list()
  for(i in 1:ncol(id)){
    out <-  meta[meta$id %in% id[, i], cols]
    if(!missing(file)) write.csv(out, file = paste0(file, nameArg[i], ".csv"))
    outlist <- c(outlist, list(out))
  }
  if(more_files){
    names(outlist) <- nameArg
  }
  else outlist <- outlist[[1]]
  invisible(outlist)
}
