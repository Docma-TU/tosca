#' Preparation of Different LDAs For Clustering
#'
#' Merges different lda-results to one matrix, including only the words which
#' appears in all lda-results.
#'
#'
#' @param x A list of lda results.
#' @details The function is useful for merging lda-results prior to a cluster analysis with \code{\link{clusterTopics}}.
#' @return A matrix including all topics from all lda-results. The number of rows
#' is the number of topics, the number of columns is the number of words which
#' appear in all results.
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
#' corpus <- cleanTexts(corpus)
#' wordlist <- makeWordlist(corpus$text)
#' ldaPrep <- LDAprep(text=corpus$text, vocab=wordlist$words)
#'
#' \donttest{LDA1 <- LDAgen(documents=ldaPrep, K = 3L, vocab=wordlist$words, num.words=3)}
#' \donttest{LDA2 <- LDAgen(documents=ldaPrep, K = 3L, vocab=wordlist$words, num.words=3)}
#' \donttest{mergeLDA(list(LDA1=LDA1, LDA2=LDA2))}
#' @export mergeLDA
mergeLDA <- function(x){
    if(is.null(names(x))) names <- as.character(seq_along(x)) else names <- names(x)
    ntimes <- sapply(x, function(y)nrow(y$topics))
    n <- length(x)
    x <- lapply(x,function(x)x$topics)
    vocab <- table(unlist(sapply(x,colnames)))
    vocab <- names(vocab)[which(vocab==n)]
    mtch <- lapply(x,function(x)match(vocab, colnames(x)))
    res <- NULL
    for(i in 1:length(x))res <- rbind(res,x[[i]][,mtch[[i]]])
    res <- res/rowSums(res)
    rownames(res) <- paste(rep(names, ntimes), unlist(apply(as.matrix(ntimes), 1, seq)), sep="_")
    return(res)
}


