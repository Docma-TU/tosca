#' Plotting Counts of Topics over Time (Relative to Corpus)
#'
#' Creates a plot of the counts/proportion of specified topics of a result of
#' \code{\link{LDAgen}}. There is an option to plot all curves in one plot
#' or to create one plot for every curve (see \code{pages}).
#' In addition the plots can be written to a pdf by setting \code{file}.
#'
#' @param object \code{\link{textmeta}} object with strictly tokenized
#' \code{text} component (character vectors) - such as a result of
#' \code{\link{cleanTexts}}
#' @param ldaresult The result of a function call \code{\link{LDAgen}}
#' @param ldaID Character vector of IDs of the documents in
#' \code{ldaresult}
#' @param select Integer: Which topics of
#' \code{ldaresult} should be plotted (default: all topics)?
#' @param tnames Character vector of same length as \code{select}
#' - labels for the topics (default are the first returned words of
#' \code{\link{top.topic.words}} from the \code{lda} package for each topic)
#' @param rel Logical: Should counts (\code{FALSE}) or
#' proportion (\code{TRUE}) be plotted (default: \code{FALSE})?
#' @param mark Logical: Should years be marked by
#' vertical lines (default: \code{TRUE})?
#' @param unit Character: To which unit should dates be floored
#' (default: \code{"month"})? Other possible units are \code{"bimonth"}, \code{"quarter"}, \code{"season"},
#' \code{"halfyear"}, \code{"year"}, for more units see \code{\link[lubridate]{round_date}}
#' @param curves Character: Should \code{"exact"},
#' \code{"smooth"} curve or \code{"both"} be plotted (default: \code{"exact"})?
#' @param smooth Numeric: Smoothing parameter
#' which is handed over to \code{\link{lowess}} as \code{f} (default: \code{0.05})
#' @param both.lwd Graphical parameter for smoothed values
#' if \code{curves = "both"}
#' @param both.lty Graphical parameter for smoothed values
#' if \code{curves = "both"}
#' @param main Character: Graphical parameter
#' @param xlab Character: Graphical parameter
#' @param ylab Character: Graphical parameter
#' @param ylim Graphical parameter
#' @param col Graphical parameter, could be a vector. If \code{curves = "both"}
#' the function will for every topicgroup plot at first the exact and then the
#' smoothed curve - this is important for your col order.
#' @param legend Character: Value(s) to specify the legend coordinates (default: \code{"topright"},
#' \code{"onlyLast:topright"} for \code{pages = TRUE} respectively).
#' If "none" no legend is plotted.
#' @param pages Logical: Should all curves be
#' plotted in a single plot (default: \code{FALSE})? In addtion you could set
#' \code{legend = "onlyLast:<argument>"} with \code{<argument>} as a
#' \code{character} \code{legend} argument
#' for only plotting a legend on the last plot of set.
#' @param natozero Logical: Should NAs be coerced
#' to zeros (default: \code{TRUE})? Only has effect if \code{rel = TRUE}.
#' @param file Character: File path if a pdf should be created
#' @param ... Additional graphical parameters
#' @return A plot.
#' Invisible: A dataframe with columns \code{date} and \code{tnames} with the
#' counts/proportion of the selected topics.
#' @examples
#' \dontrun{
#' data(politics)
#' poliClean <- cleanTexts(politics)
#' words10 <- makeWordlist(text=poliClean$text)
#' words10 <- words10$words[words10$wordtable > 10]
#' poliLDA <- LDAprep(text=poliClean$text, vocab=words10)
#' LDAresult <- LDAgen(documents=poliLDA, K=10, vocab=words10)
#'
#' # plot all topics
#' plotTopic(object=poliClean, ldaresult=LDAresult, ldaID=names(poliLDA))
#'
#' # plot special topics
#' plotTopic(object=poliClean, ldaresult=LDAresult, ldaID=names(poliLDA), select=c(1,4))
#' }

#' @export plotTopic

plotTopic <- function(object, ldaresult, ldaID,
  select = 1:nrow(ldaresult$document_sums), tnames, rel = FALSE, mark = TRUE,
  unit = "month", curves = c("exact", "smooth", "both"), smooth = 0.05,
  main, xlab, ylim, ylab, both.lwd, both.lty, col,
  legend = ifelse(pages, "onlyLast:topright", "topright"),
  pages = FALSE, natozero = TRUE, file, ...){

  if(missing(tnames)) tnames <- paste0("T", select, ".",
    lda::top.topic.words(ldaresult$topics, num.words = 1, by.score = TRUE)[select])
  # set x-label if missing
  if(missing(xlab)) xlab <- "date"
  # set y-label if missing
  if(missing(ylab)) ylab <- paste(ifelse(rel, "proportion", "counts"), "per", unit)
  # set "both" - graphical parameters if missing
  if(missing(both.lwd)) both.lwd <- 1
  if(missing(both.lty)) both.lty <- 1

  if(!missing(file)) pdf(file, width = 15, height = 8)
  if(pages){
    mainP <- paste("Count of topic", tnames, "over time")
    if(rel) mainP <- paste("Proportion of topic", tnames,"over time")
    if(curves[1] == "both") colP <- c("grey", "black")
    else colP <- "black"
    for (i in seq_along(select))
      Recall(object = object, ldaresult = ldaresult, ldaID = ldaID,
        select = select[i], tnames = tnames[i], rel = rel, mark = mark,
        unit = unit, curves = curves, smooth = smooth,
        main = ifelse(missing(main), mainP[i], main), col = colP,
        legend = legend, both.lwd = both.lwd, both.lty = both.lty,
        xlab = xlab, ylab = ylab, ylim = ylim , pages = FALSE, ...)
  }

  stopifnot(is.textmeta(object), is.list(ldaresult),
    is.matrix(ldaresult$document_sums), is.character(ldaID),
    all(as.integer(select) == select), length(tnames) == length(select),
    is.character(tnames), is.logical(rel), is.logical(mark), length(rel) == 1,
    length(mark) == 1,
    is.character(unit), length(unit) == 1, is.numeric(smooth),
    length(smooth) == 1, all(curves %in% c("exact", "smooth", "both")),
    is.character(xlab), is.character(ylab), is.numeric(both.lwd),
    is.numeric(both.lty), length(xlab) == 1, length(ylab) == 1,
    length(both.lty) == 1, length(both.lwd) == 1)

  indMeta <- match(ldaID, object$meta$id)
  indText <- match(ldaID, names(object$text))

  # generate x-values date (non-unique at this point)
  dates <- lubridate::floor_date(object$meta$date[indMeta], unit)
  # generate markers on every beginning year
  if (mark) markYears <- seq(from = lubridate::floor_date(
    min(dates, na.rm = TRUE), unit = "year"), to = lubridate::ceiling_date(
      max(dates, na.rm = TRUE), unit = "year"), by = "year")
  else markYears <- NA

  # columns: selected topics, rows: documents
  docTopic <- data.frame(t(ldaresult$document_sums)[, select])
  # sum words to unit
  docTopic <- aggregate(docTopic, by = list(date = dates), FUN = sum)

  if (rel){
    # sum words to unit for normalization
    normsums <- aggregate(
      lengths(ldaresult$assignments),
      by = list(date = lubridate::floor_date(object$meta$date[indMeta], unit)),
      FUN = sum)
    normsums <- normsums[match(docTopic$date, normsums$date),]
    docTopic[, 2:length(docTopic)] <- docTopic[, 2:length(docTopic)] / normsums$x
    if(missing(main)) main <- "Proportion of topics over time"
  }
  else if(missing(main)) main <- "Count of topics over time"
  if(missing(ylim)) ylim <- c(0, max(docTopic[, 2:length(docTopic)]))
  names(docTopic) <- c("date", tnames)

  # identify levels to add as zeros
  levs <-
    unique(lubridate::floor_date(seq(from = min(docTopic$date),
      to = max(docTopic$date), by = "day"), unit = unit))
  zerosToAdd <- !(levs %in% docTopic$date)
  if(any(zerosToAdd)){
    matrixAdd <- matrix(0, nrow = sum(zerosToAdd), ncol = ncol(docTopic)-1)
    #if(rel) matrixAdd <- cbind(matrixAdd,
    #  matrix(NA, nrow = sum(zerosToAdd), ncol = ncol(docTopic)-1))
    zerosToAdd <- data.frame(levs[zerosToAdd], matrixAdd)
    names(zerosToAdd) <- names(docTopic)
    docTopic <- rbind(docTopic, zerosToAdd)
  }
  # order docTopic
  docTopic <- docTopic[order(docTopic$date),]
  if(natozero) docTopic[is.na(docTopic)] <- 0
  row.names(docTopic) <- 1:nrow(docTopic)

  plot(docTopic$date, docTopic[, 2], type = "n",
    main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
  abline(v = markYears, lty = 3)
  switch(curves[1],
    exact = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(8, "Dark2")
      col <- rep(col, length.out = length(tnames))
      for (i in 1:(ncol(docTopic)-1))
        lines(docTopic$date, docTopic[, i+1], col = col[i], ...)
    },
    smooth = {
      # set colors if missing
      if(missing(col)) col <- RColorBrewer::brewer.pal(8, "Dark2")
      col <- rep(col, length.out = length(tnames))
      for (i in 1:(ncol(docTopic)-1))
        lines(lowess(docTopic$date, docTopic[, i+1], f = smooth), col = col[i], ...)
    },
    both = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(12, "Paired")
      col <- rep(col, length.out = 2*length(tnames))
      # plot both curves
      for (i in 1:(ncol(docTopic)-1)){
        lines(docTopic$date, docTopic[, i+1], col = col[2*i-1], ...)
        lines(lowess(docTopic$date, docTopic[, i+1], f = smooth), col = col[2*i],
          lwd = both.lwd, lty = both.lty)
      }
      # reduce col-vector for legend
      col <- col[seq_along(col) %% 2 == 0]
    })
  # plot legend
  if(all(legend != "none", !grepl("onlyLast:", legend)) ||
      (grepl("onlyLast:", legend) && pages))
    legend(gsub("onlyLast:", "", x = legend), legend = tnames, col = col, pch = 20)
  if(!missing(file)) dev.off()

  invisible(docTopic)
}
