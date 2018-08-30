#' Plots Counts of Documents or Words over Time (relative to Corpus)
#'
#' Creates a plot of the counts/proportion of documents/words in the subcorpus,
#' which could be specified by \code{id}.
#'
#' @param object \code{\link{textmeta}} object with strictly tokenized
#' \code{text} component vectors if \code{type = "words"}
#' @param id Character: Vector (default: \code{object$meta$id}) which IDs
#' specify the subcorpus
#' @param type Character: Should counts/proportion
#' of documents \code{"docs"} (default) or words \code{"words"} be plotted?
#' @param rel Logical: Should counts
#' (default: \code{FALSE}) or proportion (\code{TRUE}) be plotted?
#' @param mark Logical: Should years be marked by
#' vertical lines (default: \code{TRUE})?
#' @param unit Character: To which unit should
#' dates be floored (default: \code{"month"}). Other possible units are \code{"bimonth"}, \code{"quarter"}, \code{"season"},
#' \code{"halfyear"}, \code{"year"}, for more units see \code{\link[lubridate]{round_date}}.
#' @param curves Character: Should \code{"exact"},
#' \code{"smooth"} curve or \code{"both"} be plotted (default: \code{"exact"})?
#' @param smooth Numeric: Smoothing parameter
#' which is handed over to \code{\link{lowess}} as \code{f} (default: \code{0.05}).
#' @param both.lwd Graphical parameter for smoothed values if \code{curves = "both"}
#' @param both.col Graphical parameter for smoothed values if \code{curves = "both"}
#' @param both.lty Graphical parameter for smoothed values if \code{curves = "both"}
#' @param main Character: Graphical parameter
#' @param xlab Character: Graphical parameter
#' @param ylab Character: Graphical parameter
#' @param ylim Graphical parameter (default if \code{rel = TRUE}: \code{c(0, 1)})
#' @param natozero Logical: Should NAs be coerced
#' to zeros (default: \code{TRUE})? Only has an effect if \code{rel = TRUE}.
#' @param file Character: File path if a pdf should be created.
#' @param ... additional graphical parameters
#' @details \code{object} needs a textmeta object with strictly tokenized text component
#' (character vectors) if you use \code{type = "words"}.
#' If you use \code{type = "docs"} you can use a tokenized or a non-tokenized text component.
#' @return A plot
#' Invisible: A dataframe with columns \code{date} and \code{counts},
#' respectively \code{proportion}
#' @examples
#' \donttest{
#' data(politics)
#' poliClean <- cleanTexts(politics)
#'
#' # complete corpus
#' plotScot(object=poliClean)
#'
#' # subcorpus
#' subID <- filterWord(poliClean, search=c("bush", "obama"), out="bin")
#' plotScot(object=poliClean, id=names(subID)[subID], curves="both", smooth=0.3)
#' }

#' @export plotScot

plotScot <- function(object, id = object$meta$id, type = c("docs", "words"),
  rel = FALSE, mark = TRUE, unit = "month", curves = c("exact", "smooth", "both"),
  smooth = 0.05, main, xlab, ylab, ylim, both.lwd, both.col, both.lty,
  natozero = TRUE, file, ...){

  stopifnot(is.textmeta(object), is.character(id), is.logical(rel),
    is.logical(mark), length(rel) == 1, length(mark) == 1, is.character(unit),
    length(unit) == 1, all(type %in% c("docs", "words")),
    all(curves %in% c("exact", "smooth", "both")), is.numeric(smooth),
    length(smooth) == 1)

  if(!missing(file)) pdf(file, width = 15, height = 8)
  # set x-label if missing
  if (missing(xlab)) xlab <- "date"
  # match id with id which appears in object$text
  if (type[1] == "words"){
    id <- names(object$text)[names(object$text) %in% id]
    insert <- "words"
  }
  else insert <- "texts"
  # generate x-values date (non-unique at this point)
  dates <- lubridate::floor_date(
    object$meta$date[match(id, object$meta$id)], unit)
  # generate markers on every beginning year
  if (mark) markYears <- seq(from = lubridate::floor_date(
    min(dates, na.rm = TRUE), unit = "year"), to = lubridate::ceiling_date(
      max(dates, na.rm = TRUE), unit = "year"), by = "year")
  else markYears <- NA
  # compute counts (of words/documents)
  if (type[1] == "words"){
    docLengths <- lengths(object$text[match(id, names(object$text))])
    counts <- sapply(split(docLengths, dates), sum)
  }
  else counts <- table(dates)

  if (rel){
    if (missing(ylab)) ylab <- paste("proportion per", unit)
    # compute normalisation
    if (type[1] == "words"){
      allDates <- lubridate::floor_date(
        object$meta$date[match(names(object$text), object$meta$id)], unit)
      allCounts <- sapply(split(lengths(object$text), allDates), sum)
    }
    else{
      allDates <- lubridate::floor_date(object$meta$date, unit)
      allCounts <- table(allDates)
    }
    # compute proportions
    proportion <- counts[match(names(allCounts), names(counts))] / allCounts
    # some preparation for plotting
    dateNames <- as.Date(names(allCounts))
    proportion <- as.vector(proportion)
    proportion[is.na(proportion)] <- 0
    # set main and ylim if missing
    if (missing(main)) main <- paste("Proportion of", insert, "over time")
    if (missing(ylim)) ylim <- c(0, 1)
    tab = data.frame(date = dateNames, proportion = proportion)
  }
  else{
    if (missing(ylab)) ylab <- paste("counts per", unit)
    # some preparation for plotting
    dateNames <- as.Date(names(counts))
    counts <- as.vector(counts)
    # set main if missing
    if (missing(main)) main <- paste("Count of", insert, "over time")
    if (missing(ylim)) ylim <- c(0, max(counts))
    tab = data.frame(date = dateNames, counts = counts)
  }
  levs <-
    unique(lubridate::floor_date(
      seq(from = min(tab$date), to = max(tab$date), by = "day"), unit = unit))
  zerosToAdd <- !(levs %in% tab$date)
  if(any(zerosToAdd)){
    # add NA for proportion or zero for counts
    zerosToAdd <- data.frame(levs[zerosToAdd], ifelse(rel, NA, 0))
    names(zerosToAdd) <- names(tab)
    tab <- rbind(tab, zerosToAdd)
  }
  tab <- tab[order(tab$date),]
  if(natozero) tab[is.na(tab)] <- 0
  row.names(tab) <- 1:nrow(tab)
  if (curves[1] %in% c("exact", "both")){
    plot(tab, type = "l", main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
    if (curves[1] == "both"){
      if (missing(both.lwd)) both.lwd <- 1
      if (missing(both.col)) both.col <- "red"
      if (missing(both.lty)) both.lty <- 1
      lines(lowess(tab, f = smooth), type = "l",
        lwd = both.lwd, col = both.col, lty = both.lty)
    }
  }
  else{
    tab2 <- data.frame(date = tab$date, values = lowess(tab, f = smooth)$y)
    colnames(tab2) <- colnames(tab)
    plot(tab2, type = "l", main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
  }
  abline(v = markYears, lty = 3)
  if(!missing(file)) dev.off()
  invisible(tab)
}
