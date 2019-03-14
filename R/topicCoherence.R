#' Calculating Topic Coherence
#'
#' Implementationof Mimno's topic coherence.
#' 
#' @param ldaresult The result of a function call \code{\link{LDAgen}}
#' @param documents A list prepared by \code{\link{LDAprep}}.
#' @param num.words Integer: Number of topwords used for calculating topic coherence (default: \code{10}).
#' @param by.score Logical: Should the Score from \code{\link[lda]{top.topic.words}} be used (default: \code{TRUE})?
#' @return A vector of topic coherences. the length of the vector corresponds to the number of topics in the model.
#' @references Mimno, David and Wallach, Hannah M. and Talley, Edmund and Leenders, Miriam and McCallum, Andrew. Optimizing semantic coherence in topic models. EMNLP '11 Proceedings of the Conference on Empirical Methods in Natural Language Processing, 2011.
#'
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
#' corpus <- cleanTexts(corpus)
#' wordlist <- makeWordlist(corpus$text)
#' ldaPrep <- LDAprep(text=corpus$text, vocab=wordlist$words)
#'
#' \donttest{result <- LDAgen(documents=ldaPrep, K = 3L, vocab=wordlist$words, num.words=3)}
#' \donttest{topicCoherence(ldaresult=result, documents=ldaPrep, num.words=10, by.score=TRUE)}
#' @export topicCoherence
topicCoherence <- function(ldaresult, documents, num.words=10, by.score=TRUE){
  stopifnot(is.list(ldaresult), is.list(ldaresult$assignments), length(ldaresult$assignments) == length(documents), is.matrix(ldaresult$topics), is.list(documents),
            as.integer(num.words) == num.words, length(num.words) == 1,
            is.logical(by.score), length(by.score) == 1)  
  ttw <- lda::top.topic.words(ldaresult$topics, num.words = num.words, by.score = by.score)
  wordtopic <- mapply(function(x,y)list(rbind(x[1,]+1, y+1)), documents, ldaresult$assignments)
  topicCoherence <- numeric(nrow(ldaresult$topics))
  
  for(i in 1:nrow(ldaresult$topics)){
    wordid <- match(ttw[,i], colnames(ldaresult$topics))
    
    D <- NULL
    for(j in 1:nrow(ttw)) D <- cbind(D, sapply(wordtopic, function(x)wordid[j] %in% x[1,]))
    Dsum <- colSums(D)
    
    grid <- cbind(rep(2:num.words, 1:(num.words-1)), unlist(sapply(1:(num.words-1), function(x)1:x)))
    
    tc <- apply(grid, 1, function(x) (sum(D[,x[1]]*D[,x[2]]) +1) / Dsum[x[2]] )
    topicCoherence[i] <- sum(log(tc))
  }
  topicCoherence
}
