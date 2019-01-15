#' Exports Readable Text Lists
#'
#' Exports the article id, text, title and date.
#'
#'
#' @param object \code{\link{textmeta}} object
#' @param id Character vector or matrix including article ids
#' @param file Character Filename for the export. If not specified the functions output ist only invisible.
#' @param fileEncoding character string: declares file encoding. For more information see \code{\link[utils]{write.csv}}
#' @return A list of the requested articles. If file is set, writes a csv including the meta-data of the
#' requested articles.
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
#' exportedTexts <- showTexts(object=corpus, id = c("A","C"))
#' @export showTexts
showTexts <- function(object, id = names(object$text), file, fileEncoding = "UTF-8"){
  stopifnot(is.textmeta(object), all(id %in% object$meta$id),
    all(id %in% names(object$text)))
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
  outlist <- list()
  for(i in 1:ncol(id)){
    mtch1 <- match(id[,i],names(object$text))
    mtch2 <- match(id[,i],object$meta$id)
    out <- lapply(object$text[mtch1],paste,collapse=" ")
    out <- unlist(out)
    out2 <- cbind(object$meta$id[mtch2],as.character(object$meta$date[mtch2]),object$meta$title[mtch2],out)
    out2 <- data.frame(out2, stringsAsFactors = FALSE, row.names = 1:length(out))
    colnames(out2) <- c("id","date","title","text")
    if(!missing(file)) write.csv(out2, file = paste0(file, nameArg[i], ".csv"), fileEncoding = fileEncoding)
    outlist <- c(outlist, list(out2))
  }
  if(more_files){
    names(outlist) <- nameArg
  }
  else outlist <- outlist[[1]]
  invisible(outlist)
}
