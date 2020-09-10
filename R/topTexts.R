#' Get The IDs Of The Most Representive Texts
#'
#' The function extracts the text IDs belonging to the texts with the highest relative or
#' absolute number of words per topic.
#'
#'
#' @param ldaresult LDA result
#' @param ldaID Vector of text IDs
#' @param limit Integer: Number of text IDs per topic.
#' @param rel Logical: Should be the relative frequency be used?
#' @param select Which topics should be returned?
#' @param tnames Names of the selected topics
#' @param minlength Minimal total number of words a text must have to be
#' included
#' @return Matrix of text IDs.
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
#' LDA <- LDAgen(documents=ldaPrep, K = 3L, vocab=wordlist$words, num.words=3)
#' topTexts(ldaresult=LDA, ldaID=c("A","B","C"), limit = 1L, minlength=2)
#' @export topTexts
topTexts <- function(ldaresult, ldaID, limit = 20L, rel = TRUE,
  select = 1:nrow(ldaresult$document_sums), tnames, minlength = 30L){

  if(missing(tnames)) tnames <- paste0("T", select, ".",
    lda::top.topic.words(ldaresult$topics, 1, by.score = TRUE)[select])
  stopifnot(is.character(ldaID), as.integer(limit) == limit, length(limit) == 1,
    is.logical(rel), length(rel) == 1,
    as.integer(minlength) == minlength,
    length(minlength) == 1)

  small <- apply(ldaresult$document_sums, 2, sum) >= minlength
  ldaresult$document_sums <- ldaresult$document_sums[,small]
  ldaID <- ldaID[small]

  if(rel)
    res <- t(t(ldaresult$document_sums) / colSums(ldaresult$document_sums))
  else res <- ldaresult$document_sums
  res <- res[select, ]
  if(limit)
    res <- apply(res, 1, function(x) order(x, decreasing = TRUE)[1:limit])
  else res <- apply(res, 1, function(x) order(x, decreasing = TRUE))
  if(is.vector(res)){ res <- ldaID[res]
                  names(res) <- tnames}
  else{ res <- apply(res, 2, function(x) ldaID[x])
  colnames(res) <- tnames}
  return(res)
}
