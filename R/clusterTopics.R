#' Cluster Analysis
#'
#' This function makes a cluster analysis using the Hellinger distance.
#'
#' @param ldaresult The result of a function call \code{\link{LDAgen}} -
#' alternatively the corresponding matrix \code{result$topics}
#' @param file File for the dendogram pdf.
#' @param tnames Character vector as label for the topics.
#' @param method Method statement from \code{\link[stats]{hclust}}
#' @param width Grafical parameter for pdf output. See \code{\link[grDevices]{pdf}}
#' @param height Grafical parameter for pdf output. See \code{\link[grDevices]{pdf}}
#' @param ... Additional parameter for \code{\link[graphics]{plot}}
#' @return A dendogram as pdf and a list containing \item{dist}{A distance matrix}
#' \item{clust}{The result from \code{hclust}}
#' @details This function is useful to analyze
#' topic similarities and while evaluating the right number of topics of LDAs.
#' @examples
#'
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
#' corpus <- cleanTexts(corpus)
#' wordlist <- makeWordlist(corpus$text)
#' ldaPrep <- LDAprep(text=corpus$text, vocab=wordlist$words)
#'
#' LDA <- LDAgen(documents=ldaPrep, K = 3L, vocab=wordlist$words, num.words=3)
#' clusterTopics(ldaresult=LDA)
#'
#' @export clusterTopics
clusterTopics <- function(ldaresult, file, tnames = NULL,
                          method = "average", width = 30, height = 15, ...){
  if(is.matrix(ldaresult)) ldaresult <- list(topics = ldaresult)
  if(is.null(tnames)) tnames <- 1:nrow(ldaresult$topics)
  stopifnot(is.list(ldaresult), is.matrix(ldaresult$topics))
  topics <- ldaresult$topics/rowSums(ldaresult$topics)
  if(!is.null(tnames)) rownames(topics) <- tnames
  topics <- sqrt(topics)
  Dist <- 1/sqrt(2) * dist(topics)
  attr(Dist, "method") <- "hellinger"
  clust <- hclust(d=Dist, method)
  if(!missing(file)){
    pdf(file, width, height)
    plot(clust, ...)
    dev.off()
  }
  else plot(clust, label=tnames, ...)
  invisible(list(dist=Dist, cluster=clust))
}
