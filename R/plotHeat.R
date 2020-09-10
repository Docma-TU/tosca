#' Plotting Topics over Time relative to Corpus
#'
#' Creates a pdf showing a heat map. For each topic, the heat map shows the deviation of
#' its current share from its mean share. Shares can be calculated on corpus level or on subcorpus level concerning LDA vocabulary.
#' Shares can be calculated in absolute deviation from the mean or relative to the mean of the topic to account for different topic strengths.
#'
#' @param object \code{\link{textmeta}} object with strictly tokenized \code{text}
#' component (calculation of proportion on document lengths) or
#' \code{\link{textmeta}} object which contains only the \code{meta} component
#' (calculation of proportion on count of words out of the LDA vocabulary in each
#' document)
#' @param select Numeric vector containing the numbers of the topics to be plotted. Defaults to all topics.
#' @param ldaresult LDA result object.
#' @param ldaID Character vector containing IDs of the texts.
#' @param norm Logical: Should the values be normalized by the mean topic share to account for differently sized topics (default: \code{FALSE})?
#' @param file Character vector containing the path and name for the pdf output file.
#' @param tnames Character vector with labels for the topics.
#' @param unit Character:  To which unit should dates be floored (default: \code{"year"})?
#' Other possible units are \code{"bimonth"}, \code{"quarter"}, \code{"season"},
#' \code{"halfyear"}, \code{"year"}, for more units see \code{\link[lubridate]{round_date}}
#' @param date_breaks How many labels should be shown on the x axis (default: \code{1})?
#' If \code{data_breaks} is \code{5} every fifth label is drawn.
#' @param margins See \code{\link{heatmap}}
#' @param ... Additional graphical parameters passed to \code{\link{heatmap}},
#' for example \code{distfun} or \code{hclustfun}.
#' details The function is useful to search for peaks in the coverage of topics.
#' @return A pdf.
#' Invisible: A dataframe.
#' @examples
#' \dontrun{
#' data(politics)
#' poliClean <- cleanTexts(politics)
#' words10 <- makeWordlist(text=poliClean$text)
#' words10 <- words10$words[words10$wordtable > 10]
#' poliLDA <- LDAprep(text=poliClean$text, vocab=words10)
#' LDAresult <- LDAgen(documents=poliLDA, K=10, vocab=words10)
#' plotHeat(object=poliClean, ldaresult=LDAresult, ldaID=names(poliLDA))
#' }

#' @export plotHeat

plotHeat <- function(object, ldaresult, ldaID,
  select = 1:nrow(ldaresult$document_sums), tnames,
  norm = FALSE, file, unit = "year", date_breaks = 1, margins = c(5,0), ...){

  stopifnot(is.textmeta(object), is.character(ldaID),
    all(as.integer(select) == select), min(select) > 0,
    max(select) <= nrow(ldaresult$document_sums))

  if(missing(tnames)) tnames <- paste0("T", select, ".",
    lda::top.topic.words(ldaresult$topics, num.words = 1, by.score = TRUE)[select])
  if(!missing(file)) pdf(file, width = 15, height = 8)

  #create data frame. rows: documents, columns: topics
  tmp <- data.frame(t(ldaresult$document_sums))

  #get dates for all documents to be visualized and
  #round to years, respectively unit
  tmpdate <- lubridate::floor_date(object$meta$date[
    match(ldaID, object$meta$id)], unit = unit)
  #sum document-levels values to months, respectively unit
  tmp <- aggregate(tmp, by = list(date = tmpdate), FUN = sum)

  ### Prepare normalization data ###
  #get dates of every document in the corpus and
  #count words for every document
  if(is.null(object$text)){
    normdates <- tmpdate
    normsums <- aggregate(lengths(ldaresult$assignments),
      by = list(date = normdates), FUN = sum)
  }
  else{
    normdates <- lubridate::floor_date(object$meta$date[
      match(names(object$text), object$meta$id)], unit = unit)
    normsums <- aggregate(lengths(object$text),
      by = list(date = normdates), FUN = sum)
  }
  #tidy up
  rm(normdates)

  ### Normalize data ###
  normsums <- normsums[match(tmp$date, normsums$date),]
  tmp[,2:length(tmp)] <- apply(tmp[,2:length(tmp)],2,function(y) y/normsums$x)
  #cell values are now shares in document x of topic y

  #filter for topics to be plotted
  tmp <- tmp[, c(1,select+1)]

  #get mean for each topic over entire time: column means
  tmeans <- apply(tmp[2:length(tmp)], 2, mean)
  #calculate absolute distance to mean. normalize distance with mean if specified
  for(i in 1:nrow(tmp)){
    tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] - tmeans
    if(norm == TRUE){
      tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] / tmeans
    }
  }

  breaks <- character(length(tmp$date))
  ind <- c(seq(1, length(breaks), by = date_breaks), length(breaks))
  breaks[ind] <- as.character(tmp$date[ind])

  heatmap(t(as.matrix(tmp[-1])), Colv = NA, labRow = tnames, labCol = breaks,
    col = colorRampPalette(c("#0571b0", "#ffffff","#ca0020"))(50),
    scale = "none", margins = margins, main = ifelse(norm == T,
      "Normalized Deviation of Topic Shares from Mean Topic Share",
      "Absolute Deviation of Topic Shares from Mean Topic Share"), ...)
  if(!missing(file)) dev.off()
  names(tmp)[2:(length(select)+1)] <- tnames
  invisible(tmp)
}
