#' Function to fit LDA model
#'
#' This function uses the \code{\link[lda]{lda.collapsed.gibbs.sampler}} from the lda-
#' package and additionally saves topword lists and a R workspace.
#'
#'
#' @param documents A list prepared by \code{\link{LDAprep}}.
#' @param K Number of topics
#' @param vocab Character vector containing the words in the corpus
#' @param num.iterations Number of iterations for the gibbs sampler
#' @param burnin Number of iterations for the burnin
#' @param alpha Hyperparameter for the topic proportions
#' @param eta Hyperparameter for the word distributions
#' @param seed A seed for reproducability.
#' @param folder File for the results. Saves in the temporary directionary by default.
#' @param num.words Number of words in the top topic words list
#' @param LDA logical: Should a new model be fitted or an existing R workspace?
#' @param count logical: Should article counts calculated
#' per top topic words be used for output as csv
#' (default: \code{FALSE})?
#' @return A .csv file containing the topword list and a R workspace containing the
#' result data.
#' @seealso Documentation for the lda package.
#' @references Blei, David M. and Ng, Andrew and Jordan, Michael. Latent
#' Dirichlet allocation. Journal of Machine Learning Research, 2003.
#'
#' Jonathan Chang (2012). lda: Collapsed Gibbs sampling methods for topic
#' models.. R package version 1.3.2. http://CRAN.R-project.org/package=lda
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
#' LDAgen(documents=ldaPrep, K = 3L, vocab=wordlist$words, num.words=3)
#'
#' @export LDAgen
LDAgen <- function(documents, K = 100L, vocab, num.iterations = 200L,
  burnin = 70L, alpha = NULL, eta = NULL, seed = NULL,
  folder = file.path(tempdir(),"lda-result"), num.words = 50L, LDA = TRUE, count = FALSE){
    if(is.null(alpha)) alpha <- 1/K
    if(is.null(eta)) eta <- 1/K
    if(is.null(seed)) seed <- sample(1:10^8,1)
    stopifnot(is.list(documents), as.integer(K) == K, length(K) == 1,
              is.character(vocab), as.integer(num.iterations) == num.iterations,
              length(num.iterations) == 1, as.integer(burnin) == burnin,
              length(burnin) == 1, is.numeric(alpha), length(alpha) == 1,
              is.numeric(eta), length(eta) == 1, is.numeric(seed),
              length(seed) == 1, is.character(folder), length(folder) == 1,
              as.integer(num.words) == num.words, length(num.words) == 1,
              is.logical(LDA), length(LDA) == 1)
    if(LDA){
        set.seed(seed)
        result <- lda.collapsed.gibbs.sampler(documents = documents, K = K,
                                              vocab = vocab,
                                              num.iterations = num.iterations,
                                              burnin = burnin,
                                              alpha = alpha, eta = eta,
                                              compute.log.likelihood = TRUE)
        ldaID <- names(documents)
        save(list = c("result", "ldaID"), file = paste0(folder, "-k", K, "alpha", round(alpha,2), 
                                                        "eta", round(eta,2), "i", num.iterations, 
                                                        "b", burnin, "s", seed, ".RData"))
    }
    else{
        load(paste0(folder, "-k", K, "alpha", round(alpha,2), 
                    "eta", round(eta,2), "i", num.iterations, 
                    "b", burnin, "s", seed, ".RData"))
    }
    ttw <- lda::top.topic.words(result$topics, num.words = num.words, by.score = TRUE)
    if(count){
      counts <- matrix(nrow = nrow(ttw), ncol = ncol(ttw))
      for(i in 1:ncol(ttw)){
        topicNr <- i-1
        wordIDs <- match(ttw[, i], vocab)-1
        countTmp <- numeric(ncol(ttw))
        for(j in 1:length(wordIDs)){
          counts[j, i] <- sum(mapply(function(x, y) topicNr %in% x[y],
            result$assignments, lapply(documents, function(x) x[1,] == wordIDs[j])))
        }
      }
      ttw <- rbind(paste0("T", 1:ncol(ttw)), ttw)
      counts <- rbind(round(t(result$topic_sums / sum(result$topic_sums))*100,2), counts)
      ttw <- cbind(ttw, counts)
      ttw <- ttw[, rep(1:(ncol(ttw)/2), each = 2) + rep(c(0, ncol(ttw)/2), ncol(ttw)/2)]
    }
    else{
      ttw <- rbind(round(t(result$topic_sums / sum(result$topic_sums))*100,2), ttw)
    }
    rownames(ttw) <- c("Topic", 1:num.words)
    write.csv(ttw, file = paste0(folder, "-k", K, "alpha", round(alpha,2), 
                                 "eta", round(eta,2), "i", num.iterations, 
                                 "b", burnin, "s", seed, ".csv"), 
              fileEncoding="UTF-8")
    invisible(result)
}

