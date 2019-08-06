#' Export Topic-Matrices per Text
#'
#' The function extracts the matrix of assignments per topic and word in texts.
#'
#' @param ldaresult LDA result.
#' @param documents A list prepared by \code{\link{LDAprep}}.
#' @param id Character vector including text ids
#' (default is \code{names(documents)}).
#' @param vocab Character vector containing the words in the corpus
#' (default is \code{colnames(ldaresult$topics)}).
#' @param tnames Names of the topics
#' (default is \code{1:K},
#' while \code{K} is the number of topics assumed in \code{ldaresult}).
#' @param file Character File path for the export. If not specified the function
#' does not write. If \code{file = ""} the current file path is chosen.
#' @param prefix Character Prefix for each file (each text). File names are
#' build from prefix and the corresponding text id.
#' @param fileEncoding Character Declares file encoding. For more information
#' see \code{\link[utils]{write.csv}}.
#' @return A list of topic-word-matrices of the requested texts.
#' If file is set, for each text a csv is written.
#' @keywords manip
#' @examples
#' texts <- list(
#'  A = "Give a Man a Fish, and You Feed Him for a Day.
#'       Teach a Man To Fish, and You Feed Him for a Lifetime",
#'  B = "So Long, and Thanks for All the Fish",
#'  C = "A very able manipulative mathematician, Fisher enjoys a real mastery
#'       in evaluating complicated multiple integrals.")
#'
#' corpus <- textmeta(
#'  meta = data.frame(
#'   id = c("A", "B", "C", "D"),
#'   title = c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
#'   date = c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
#'   additionalVariable = 1:4,
#'   stringsAsFactors = FALSE),
#'  text = texts)
#'
#' corpus <- cleanTexts(corpus)
#' wordlist <- makeWordlist(corpus$text)
#' ldaPrep <- LDAprep(text=corpus$text, vocab=wordlist$words)
#'
#' \donttest{LDA <- LDAgen(documents = ldaPrep, K = 3L,
#' vocab = wordlist$words, num.words = 3)}
#' \donttest{res <- showTextTopics(ldaresult = LDA, documents = ldaPrep)}
#' @export showTextTopics

showTextTopics = function(ldaresult, documents, id = names(documents),
  vocab = colnames(ldaresult$topics), tnames,
  file, prefix = "", fileEncoding = "UTF-8"){
  
  # ldaresult und docs gleiche struktur
  # alle woerter werden aktuell beruecksichtigt
  K = nrow(ldaresult$topics)
  if (missing(tnames)) tnames = 1:K
  # tnames.laenge == K
  
  textind = match(id, names(documents))
  res = list()
  k = 1
  for (i in textind){
    tab = table(
      factor(ldaresult$assignments[[i]] + 1, levels = 1:K),
      documents[[i]][1,])
    colnames(tab) = vocab[as.integer(colnames(tab))+1]
    rownames(tab) = tnames
    tab = rbind(colSums(tab), tab)
    tab = cbind(rowSums(tab), tab)
    colnames(tab)[1] = "WORD"
    rownames(tab)[1] = "TOPIC"
    res[[k]] = tab
    k = k+1
  }
  if (!missing(file)){
    if (file == "") file = getwd()
    for (i in seq_along(res)){
      write.csv(res[[i]], file = file.path(file, paste0(prefix, id[i], ".csv")),
        fileEncoding = fileEncoding)
    }
  }
  names(res) = id
  invisible(res)
}
