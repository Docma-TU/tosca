#' Plotting topics over time as stacked areas below plotted lines.
#'
#' Creates a stacked area plot of all or selected topics.
#'
#'
#' @param ldaresult LDA result object
#' @param ldaID Character vector including IDs of the texts
#' @param select Selects all topics if parameter is null. Otherwise vector of integers or topic label. Only topics belonging to that numbers, and labels respectively would be plotted.
#' @param tnames Character vector of topic labels. It must have same length than number of topics in the model.
#' @param threshold Numeric: Treshold between 0 and 1. Topics would only be used if at least one time unit exist with a topic proportion above the treshold
#' @param meta The meta data for the texts or a date-string.
#' @param unit Time unit for x-axis. Possible units are \code{"bimonth"}, \code{"quarter"}, \code{"season"},
#' \code{"halfyear"}, \code{"year"}, for more units see \code{\link[lubridate]{round_date}}
#' @param xunit Time unit for tiks on the x-axis. For possible units see \code{\link[lubridate]{round_date}}
#' @param color Color vector. Color vector would be replicated if the number of plotted topics is bigger than length of the vector.
#' @param sort Logical: Should the topics be sorted by topic proportion?
#' @param legend Position of legend. If \code{NULL} (default), no legend will be plotted
#' @param legendLimit Numeric between 0 (default) and 1. Only Topics with proportions above this limit appear in the legend.
#' @param peak Numeric between 0 (default) and 1. Label peaks above \code{peak}. For each Topic every area which are at least once above \code{peak} will e labeled. An area ends if the topic proportion is under 1 percent.
#' @param file Character: File path if a pdf should be created
#' @details This function is useful to visualize the volume of topics and to show trends over time.
#' @return List of two matrices. \code{rel} contains the topic proportions over time, \code{relcum} contains the cumulated topic proportions
#' @examples
#' \dontrun{
#' data(politics)
#' poliClean <- cleanTexts(politics)
#' words10 <- makeWordlist(text=poliClean$text)
#' words10 <- words10$words[words10$wordtable > 10]
#' poliLDA <- LDAprep(text=poliClean$text, vocab=words10)
#' LDAresult <- LDAgen(documents=poliLDA, K=10, vocab=words10)
#' plotArea(ldaresult=LDAresult, ldaID=names(poliLDA), meta=politics$meta)
#'
#' plotArea(ldaresult=LDAresult, ldaID=names(poliLDA), meta=politics$meta, select=c(1,3,5))
#' }
#' @export plotArea
#'
plotArea <- function(ldaresult, ldaID, select = NULL, tnames = NULL,
                     threshold = NULL, meta, unit = "quarter", xunit = "year", color = NULL,
                     sort = TRUE, legend = NULL, legendLimit = 0, peak = 0, file){
  
  if(!missing(file)) pdf(file, width = 15, height = 8)
  if(is.null(color)) color <- RColorBrewer::brewer.pal(n=12, name="Paired")[c(2*(1:6),2*(1:6)-1)]
  if(is.null(tnames)) tnames <- paste0("T", 1:nrow(ldaresult$document_sums))
  
  IDmatch <- match(ldaID,meta$id)
  if(any(is.na(IDmatch))){stop("missing id's in meta")}
  x <- as.data.frame(t(ldaresult$document_sums))
  textDate <- lubridate::floor_date(meta$date[IDmatch], unit)
  x <- split(x=x, f=textDate)
  x <- sapply(x,colSums)
  x <- t(t(x)/colSums(x))
  rownames(x) <- tnames
  
  ## add zeros
  levs <-
    as.character(unique(lubridate::floor_date(
      seq(from = as.Date(min(colnames(x))), to = as.Date(max(colnames(x))), by = "day"), unit = unit)))
  zerosToAdd <- !(levs %in% colnames(x))
  if(any(zerosToAdd)){
    # add NA for proportion
    tab <- matrix(NA, nrow = nrow(x), ncol = sum(zerosToAdd))
    colnames(tab) <- levs[zerosToAdd]
    x <- cbind(x, tab)
    x <- x[,order(colnames(x))]
  }
  
  ## reduce to used topics
  if(!is.null(select)){
    if(is.numeric(select) | is.integer(select)){x <- x[select,]
    }else{x <- x[match(select, tnames),]
    }
  }
  
  ## reduce via threshold
  if(!is.null(threshold)){x <- x[apply(x,1, function(x)any(x>=threshold, na.rm = TRUE)),]}
  
  
  ## sort topics
  if(sort){topicVolume <- rowSums(x, na.rm = TRUE)
  x <- x[order(topicVolume, decreasing=FALSE),]}
  
  ## cumsum
  y <- apply(x,2, cumsum)
  y <- rbind(y[1,]-y[1,],y)
  
  ## expand color vector if necessary
  if(length(color) < nrow(y)) color <- rep(color, ceiling(nrow(y)/length(color)))[1:nrow(x)]else color <- color[1:nrow(x)]
  
  ## plotting
  tmplas <- par("las")
  par(las = 2)
  plot(NULL, xlim=range(as.Date(colnames(x))), ylim=c(0,max(y, na.rm = TRUE)), ylab="", xlab="", xaxt="n")
  for(i in 1:(nrow(y)-1)){
    polygon(x=c(as.Date(colnames(y)), rev(as.Date(colnames(y)))), c(y[i,], rev(y[i+1,])), col=color[i])
  }
  xvals <- seq(lubridate::floor_date(min(as.Date(colnames(y))), unit=xunit), max(as.Date(colnames(y))), by=xunit)
  axis(side = 1, xvals, format(xvals, "%b %y"), cex.axis = .85)
  
  ## label peaks
  if(peak>0){
    xPeak <- x>peak
    xPeaklower <- x>0.01
    xPeak[is.na(xPeak)] = FALSE
    xPeaklower[is.na(xPeaklower)] = FALSE
    for(i in 1:nrow(xPeak)){
      wPeak <- which(xPeak[i,])
      if(length(wPeak)==0)next
      if(length(wPeak)==1){ text(x=as.Date(names(wPeak)),y=(y[i,wPeak] + y[i+1,wPeak])/2, labels=rownames(y)[i+1])
        next}
      wPeakNew <- NULL
      j <- 1
      while(j <length(wPeak)){
        kold <- 0
        for(k in j:length(wPeak)){
          if(any(x[i,(wPeak[j]+1):(wPeak[j+1]-1)]<0.01, na.rm = TRUE)){ wPeakNew <- c(wPeakNew, wPeak[j:k][which.max(x[i,wPeak[j:k]])])
          kold <- k
          j <- k+1
          break}
          if(k==length(wPeak)){wPeakNew <- c(wPeakNew, wPeak[j:k][which.max(x[i,wPeak[j:k]])])
          j <- k+1}
        }
      }
      if(rev(wPeakNew)[1] != rev(wPeak)[1] & k==kold+1) wPeakNew <- c(wPeakNew, rev(wPeak)[1])
      text(x=as.Date(names(wPeakNew)),y=(y[i,wPeakNew] + y[i+1,wPeakNew])/2, labels=rownames(y)[i+1])
    }
  }
  
  ## legend
  if(!is.null(legend)){legend(x=legend, legend= rev(rownames(y)[-1][apply(x,1,function(z)any(z>legendLimit))]), bg="white", pch=15, col=rev(color[apply(x,1,function(z)any(z>legendLimit))]))}
  par(las=tmplas)
  x <- data.frame(t(x))
  y <- data.frame(t(y))[, -1]
  if(!missing(file)) dev.off()
  invisible(list(rel=x,relcum=y))
}

