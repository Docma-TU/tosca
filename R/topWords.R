#' Top Words per Topic
#'
#' Determines the top words per topic as \code{\link[lda]{top.topic.words}} do.
#' In addition, it is possible to request the values that are taken for 
#' determining the top words per topic. Therefore, the function \code{importance}
#' is used, which also can be called independently.
#'
#' @param topics \code{named matrix}: 
#' The counts of vocabularies (column wise) in topics (row wise).
#' @param numWords \code{integer(1)}: 
#' The number of requested top words per topic.
#' @param byScore \code{logical(1)}: 
#' Should the values that are taken for determining the top words per topic be
#' calculated by the function \code{importance} (\code{TRUE}) or should
#' the absolute counts be considered (\code{FALSE})?
#' @param epsilon \code{numeric(1)}: 
#' Small number to add to logarithmic calculations to overcome the issue of
#' determining \code{log(0)}.
#' @param values \code{logical(1)}: 
#' Should the values that are taken for determining the top words per topic be
#' returned?
#' @return Matrix of top words or, if \code{value} is \code{TRUE} a list of
#' matrices with entries \code{word} and \code{val}.
#' @examples
#' texts <- list(
#'  A = "Give a Man a Fish, and You Feed Him for a Day.
#'       Teach a Man To Fish, and You Feed Him for a Lifetime",
#'  B = "So Long, and Thanks for All the Fish",
#'  C = "A very able manipulative mathematician, Fisher enjoys a real mastery
#'       in evaluating complicated multiple integrals.")
#'
#' corpus <- textmeta(meta = data.frame(id = c("A", "B", "C", "D"),
#'   title = c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
#'   date = c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
#'   additionalVariable = 1:4, stringsAsFactors = FALSE), text = texts)
#'
#' corpus <- cleanTexts(corpus)
#' wordlist <- makeWordlist(corpus$text)
#' ldaPrep <- LDAprep(text = corpus$text, vocab = wordlist$words)
#'
#' LDA <- LDAgen(documents = ldaPrep, K = 3L, vocab = wordlist$words, num.words = 3)
#' topWords(LDA$topics)
#' 
#' importance(LDA$topics)

#' @export topWords
topWords <- function(topics, numWords = 1, byScore = TRUE, epsilon = 1e-5, values = FALSE){
  
  stopifnot(is.matrix(topics), all(topics >= 0), !is.null(colnames(topics)),
    length(numWords) == 1, length(byScore) == 1, length(values) == 1,
    as.integer(numWords) == numWords, is.logical(byScore), is.logical(values))
  
  if (byScore){
    topics <- importance(topics, epsilon = epsilon)
  }
  words <- apply(topics, 1, function(y) colnames(topics)[head(order(y, decreasing = TRUE), numWords)])
  if (values){
    vals <- drop(head(apply(topics, 1, function(y) -sort(-y, partial = seq_len(numWords))), numWords))
    return(list(word = words, val = vals))
  }else{
    return(words)
  }
}

#' @rdname topWords
#' @export
importance <- function(topics, epsilon = 1e-5){
  
  stopifnot(is.matrix(topics), all(topics >= 0), length(epsilon) == 1,
    is.numeric(epsilon), epsilon > 0, epsilon < 2)
  
  rel <- topics/rowSums(topics)
  logs <- log(rel + epsilon)
  rel * (logs - rep(colMeans(logs), each = nrow(topics)))
}
