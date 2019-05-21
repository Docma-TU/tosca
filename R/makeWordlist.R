#' Counts Words in Text Corpora
#'
#' Creates a wordlist and a frequency table.
#'
#' This function helps, if \code{table(x)} needs too much RAM.
#'
#' @param text List of texts.
#' @param k Integer: How many texts should be processed at once (RAM
#' usage)?
#' @param ... further arguments for the sort function. Often you
#' want to set \code{method = "radix"}.
#' @return \item{words}{An alphabetical list of the words in the corpus}
#' \item{wordtable}{A frequency table of the words in the corpus}
#' @keywords manip
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#'
#' texts <- cleanTexts(text=texts)
#' makeWordlist(text=texts, k = 2L)
#'
#' @export makeWordlist
makeWordlist <- function(text, k = 100000L, ...){
  stopifnot(is.textmeta(textmeta(text = text)),
    is.numeric(k), as.integer(k) == k, length(k) == 1)
  n <- length(text)
  message(paste("number of articles:", n,""))
  message("find out vocabularies...\n done:")
  N <- 0:floor(n/k)
  words <- NULL
  for(i in N){
    message("  ", i*k)
    words <- c(words, unique(unlist(text[(i*k+1):(min(n, i*k+k))])))
  }
  message("  ", n, " next step")
  words <- sort(unique(words), ...)
  message("calculate counts...\n done:")
  wordtable <- rep(0, length(words))
  names(wordtable) <- words
  for(i in N){
    message("  ", i*k)
    tmp <- table(unlist(text[(i*k+1):(min(n, i*k+k))]))
    mtch <- match(names(tmp), names(wordtable))
    wordtable[mtch] <- wordtable[mtch] + tmp
  }
  message("  ", n, " success")
  return(list(words = words, wordtable = wordtable))
}
