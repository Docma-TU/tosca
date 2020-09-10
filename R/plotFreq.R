#' Plotting Counts of specified Wordgroups over Time (relative to Corpus)
#'
#' Creates a plot of the counts/proportion of given wordgroups (\code{wordlist})
#' in the subcorpus. The counts/proportion can be calculated on document or word
#' level - with an 'and' or 'or' link - and additionally can be normalised by
#' a subcorporus, which could be specified by \code{id}.
#'
#' @param object \code{\link{textmeta}} object with strictly tokenized
#' \code{text} component (\code{character} vectors) - like a result of
#' \code{\link{cleanTexts}}
#' @param id \code{character} vector (default: \code{object$meta$id}) which IDs
#' specify the subcorpus
#' @param type \code{character} (default: \code{"docs"}) should counts/proportion
#' of documents, where every  \code{"docs"} or words \code{"words"} be plotted
#' @param wordlist list of \code{character} vectors. Every list element is an 'or'
#' link, every \code{character} string in a vector is linked by the argument
#' \code{link}. If \code{wordlist} is only a \code{character} vector it will be
#' coerced to a list of the same length as the vector (see \code{\link{as.list}}),
#' so that the argument \code{link} has no effect. Each \code{character} vector
#' as a list element represents one curve in the outcoming plot
#' @param link \code{character} (default: \code{"and"}) should the (inner)
#' \code{character} vectors of each list element be linked by an \code{"and"}
#' or an \code{"or"}
#' @param wnames \code{character} vector of same length as \code{wordlist}
#' - labels for every group of 'and' linked words
#' @param ignore.case \code{logical} (default: \code{FALSE}) option
#' from \code{\link{grepl}}.
#' @param rel \code{logical} (default: \code{FALSE}) should counts
#' (\code{FALSE}) or proportion (\code{TRUE}) be plotted
#' @param mark \code{logical} (default: \code{TRUE}) should years be marked by
#' vertical lines
#' @param unit \code{character} (default: \code{"month"}) to which unit should
#' dates be floored. Other possible units are \code{"bimonth"}, \code{"quarter"}, \code{"season"},
#' \code{"halfyear"}, \code{"year"}, for more units see \code{\link[lubridate]{round_date}}
#' @param curves \code{character} (default: \code{"exact"}) should \code{"exact"},
#' \code{"smooth"} curve or \code{"both"} be plotted
#' @param smooth \code{numeric} (default: \code{0.05}) smoothing parameter
#' which is handed over to \code{\link{lowess}} as \code{f}
#' @param both.lwd graphical parameter for smoothed values
#' if \code{curves = "both"}
#' @param both.lty graphical parameter for smoothed values
#' if \code{curves = "both"}
#' @param main \code{character} graphical parameter
#' @param xlab \code{character} graphical parameter
#' @param ylab \code{character} graphical parameter
#' @param ylim (default if \code{rel = TRUE}: \code{c(0, 1)}) graphical parameter
#' @param col graphical parameter, could be a vector. If \code{curves = "both"}
#' the function will for every wordgroup plot at first the exact and then the
#' smoothed curve - this is important for your col order.
#' @param legend \code{character} (default: "topright") value(s) to specify the
#' legend coordinates. If "none" no legend is plotted.
#' @param natozero \code{logical} (default: \code{TRUE}) should NAs be coerced
#' to zeros. Only has effect if \code{rel = TRUE}.
#' @param file \code{character} file path if a pdf should be created
#' @param ... additional graphical parameters
#' @return A plot.
#' Invisible: A dataframe with columns \code{date} and \code{wnames} - and
#' additionally columns \code{wnames_rel} for \code{rel = TRUE} - with the
#' counts (and proportion) of the given wordgroups.
#' @examples
#' \dontrun{
#' data(politics)
#' poliClean <- cleanTexts(politics)
#' plotFreq(poliClean, wordlist=c("obama", "bush"))
#' }
#' @export plotFreq

plotFreq <- function(object, id = names(object$text),
  type = c("docs", "words"), wordlist, link = c("and", "or"), wnames,
  ignore.case = FALSE,
  rel = FALSE, mark = TRUE, unit = "month",
  curves = c("exact", "smooth", "both"), smooth = 0.05,
  both.lwd, both.lty, main, xlab, ylab, ylim, col, legend = "topright",
  natozero = TRUE, file, ...){

  stopifnot(is.textmeta(object), is.character(id), is.logical(rel),
    is.logical(mark), length(rel) == 1, length(mark) == 1, is.character(unit),
    length(unit) == 1, all(type %in% c("docs", "words")),
    all(curves %in% c("exact", "smooth", "both")), is.numeric(smooth),
    length(smooth) == 1, all(link %in% c("and", "or")))

  if(!missing(file)) pdf(file, width = 15, height = 8)
  # match id with id which appears in object$text
  inds <- names(object$text) %in% id
  id <- names(object$text)[inds]
  # coerce list
  if(!is.list(wordlist)) wordlist <- as.list(wordlist)
  # set labels (wordnames) if missing
  if(missing(wnames)) wnames <- sapply(wordlist, paste, collapse = ".")
  # collect word candidates for applying filterWord with out = "count"
  words <- unique(unlist(wordlist))
  # count appearences of candidates on subcorpus
  tmp <- filterWord(text = object$text[inds], search = words, out = "count",
    ignore.case = ignore.case)
  # calculate rowSums for each of the wordlists components
  tab <- do.call(cbind, lapply(wordlist,
    function(x) rowSums(as.matrix(tmp[, colnames(tmp) %in% x]))))
  if (link[1] == "and"){
    # identifying articles where not all words of an wordlist component
    # are represented (per wordlist component)
    tmp <- do.call(cbind, lapply(wordlist,
      function(x) apply(as.matrix(tmp[, colnames(tmp) %in% x]), 1,
        function(y) all(y > 0))))
    # set these counts to zero
    tab[!tmp] <- 0
  }
  # tidy up and set some labels
  rm(tmp)
  rownames(tab) <- id
  colnames(tab) <- wnames

  # generate x-values date (non-unique at this point)
  dates <- lubridate::floor_date(
    object$meta$date[match(id, object$meta$id)], unit)
  # generate markers on every beginning year
  if (mark) markYears <- seq(from = lubridate::floor_date(
    min(dates, na.rm = TRUE), unit = "year"), to = lubridate::ceiling_date(
      max(dates, na.rm = TRUE), unit = "year"), by = "year")
  else markYears <- NA
  # set aggreagation function for type = "words" or type = "docs"
  if (type[1] == "words"){
    aggr <- sum
    # set main label for plotting
    insert <- "words"
  }
  else{
    aggr <- function(x) sum(x > 0)
    # set main label for plotting
    insert <- "texts"
  }
  # compute counts (of words/documents)
  tab <- apply(tab, 2, function(x) sapply(split(x, dates), aggr))
  tab <- data.frame(date = row.names(tab), tab, row.names = 1:nrow(tab))

  if (rel){
    # compute normalisation
    if (type[1] == "words"){
      allDates <-
        lubridate::floor_date(object$meta$date[match(id, object$meta$id)], unit)
      allCounts <- sapply(split(lengths(
        object$text[match(id, names(object$text))]), allDates), sum)
    }
    else{
      allDates <-
        lubridate::floor_date(object$meta$date[object$meta$id %in% id], unit)
      allCounts <- table(allDates)
    }
    # tidy up
    rm(allDates)
    # compute proportions
    proportion <- as.matrix(tab[, !(names(tab) %in% "date")] /
        allCounts[match(tab$date, names(allCounts))])
    # set main and ylim if missing
    if (missing(main)) main <- paste("Proportion of", insert, "over time - link:", link[1])
    if (missing(ylim)) ylim <- c(0, 1)
    # set indices to plot
    toplot <- (ncol(tab)+1):(ncol(tab)+ncol(proportion))
    tab <- data.frame(tab, proportion)
    names(tab)[toplot] <- paste0(wnames, "_rel")
    # tidy up
    rm(proportion, allCounts)
  }
  else{
    # set main and ylim if missing
    if (missing(main))
      main <- paste("Count of wordlist-filtered", insert, "over time - link:", link[1])
    if (missing(ylim)) ylim <- c(0, max(tab[, !(names(tab) %in% "date")]))
    # set indices to plot
    toplot <- 2:ncol(tab)
  }
  # set type to date
  tab$date <- as.Date(tab$date)
  # identify levels to add as zeros
  levs <-
    unique(lubridate::floor_date(
      seq(from = min(tab$date), to = max(tab$date), by = "day"), unit = unit))
  zerosToAdd <- !(levs %in% tab$date)
  if(any(zerosToAdd)){
    matrixAdd <- matrix(0, nrow = sum(zerosToAdd), ncol = length(wordlist))
    if(rel) matrixAdd <- cbind(matrixAdd,
      matrix(NA, nrow = sum(zerosToAdd), ncol = length(wordlist)))
    zerosToAdd <- data.frame(levs[zerosToAdd], matrixAdd)
    names(zerosToAdd) <- names(tab)
    tab <- rbind(tab, zerosToAdd)
  }
  # order tab
  tab <- tab[order(tab$date),]
  if(natozero) tab[is.na(tab)] <- 0
  row.names(tab) <- 1:nrow(tab)
  # set y values to plot
  toplot <- as.matrix(tab[, toplot])

  # set x-label if missing
  if (missing(xlab)) xlab <- "date"
  # set y-label if missing
  if (missing(ylab)) ylab <- paste(ifelse(rel, "proportion", "counts"), "per", unit)
  plot(tab$date, toplot[, 1], type = "n",
    main = main, xlab = xlab, ylab = ylab, ylim = ylim, ...)
  abline(v = markYears, lty = 3)
  switch(curves[1],
    exact = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(8, "Dark2")
      col <- rep(col, length.out = length(wordlist))
      for (i in 1:ncol(toplot))
        lines(tab$date, toplot[, i], col = col[i], ...)
    },
    smooth = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(8, "Dark2")
      col <- rep(col, length.out = length(wordlist))
      for (i in 1:ncol(toplot))
        lines(lowess(tab$date, toplot[, i], f = smooth), col = col[i], ...)
    },
    both = {
      # set colors if missing
      if (missing(col)) col <- RColorBrewer::brewer.pal(12, "Paired")
      col <- rep(col, length.out = 2*length(wordlist))
      # set "both" - graphical parameters if missing
      if (missing(both.lwd)) both.lwd <- 1
      if (missing(both.lty)) both.lty <- 1
      # plot both curves
      for (i in 1:ncol(toplot)){
        lines(tab$date, toplot[, i], col = col[2*i-1], ...)
        lines(lowess(tab$date, toplot[, i], f = smooth), col = col[2*i],
          lwd = both.lwd, lty = both.lty)
      }
      # reduce col-vector for legend
      col <- col[seq_along(col) %% 2 == 0]
    })
  # plot legend
  if (legend != "none") legend(legend, legend = wnames, col = col, pch = 20)
  if(!missing(file)) dev.off()
  # return data.frame as invisible
  invisible(tab)
}
