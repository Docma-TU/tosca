#' Export Topic Assignments per Text
#'
#' The function extracts the matrix (matrices) of assignments to each word of a text.
#' 
#' @param obj \code{\link{textmeta}} object.
#' @param ldaresult LDA result.
#' @param documents A list prepared by \code{\link{LDAprep}}.
#' @param id Character vector containing the considered text ids
#' (default is \code{names(documents)}).
#' @param tnames Character vector containing the names for the topics
#' (default is determined by \code{\link[lda]{top.topic.words}}).
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
#' raw <- corpus
#'  
#' corpus <- cleanTexts(corpus)
#' wordlist <- makeWordlist(corpus$text)
#' ldaPrep <- LDAprep(text=corpus$text, vocab=wordlist$words)
#'
#' \donttest{LDA <- LDAgen(documents = ldaPrep, K = 3L, vocab = wordlist$words, num.words = 3)}
#' \donttest{res <- showTopicsInText(raw, LDA, ldaPrep)}
#' @export showTopicsInText

showTopicsInText = function(obj, ldaresult, documents, id = names(documents),
  tnames, file, prefix = "", fileEncoding = "UTF-8"){
  
  textind = match(id, names(documents))
  if (any(is.na(textind))){
    stop(paste0("ID(s) ", paste(id[is.na(textind)], collapse = ", "), " not found."))
  }
  documents = documents[textind]
  ldaresult$assignments = ldaresult$assignments[textind]
  textind = match(id, names(obj$text))
  if (any(is.na(textind))){
    stop(paste0("ID(s) ", paste(id[is.na(textind)], collapse = ", "), " not found."))
  }
  originaltext = obj$text[textind]
  
  stopifnot(is.textmeta(obj),
    is.list(ldaresult), all(c("assignments", "topics") %in% names(ldaresult)),
    is.list(documents), all(sapply(documents, is.matrix)),
    length(ldaresult$assignments) == length(documents),
    all(lengths(ldaresult$assignments) == sapply(documents, ncol)),
    is.character(id), all(id %in% names(documents)))
  
  if (missing(tnames))
    tnames = paste0("T", seq_len(nrow(ldaresult$topics)), ".",
      lda::top.topic.words(ldaresult$topics, 1, TRUE))
  
  stopifnot(is.character(tnames), length(tnames) == nrow(ldaresult$topics),
    is.character(prefix), length(prefix) == 1,
    is.character(fileEncoding), length(fileEncoding) == 1,
    missing(file) || (is.character(file) && length(file) == 1))

  topics = lapply(ldaresult$assignments, function(x) tnames[x+1])
  words = lapply(documents, function(x) colnames(ldaresult$topics)[x[1,]+1])
  res = list()
  for (i in seq_along(documents)){
      o2 <- unlist(strsplit(obj$text[[i]], split="\\s"))
      o2 <- o2[!(o2=="")]
      o2 <- data.frame(
        TEXT = o2,
        WORD = tm::removeNumbers(tolower(tm::removePunctuation(o2))),
        TOPIC = NA_character_,
        stringsAsFactors = FALSE)
      remainingwords <- data.frame(
        WORD = words[[i]],
        TOPIC = topics[[i]], 
        stringsAsFactors = FALSE)
      for(j in seq_len(nrow(o2))){
        actualmatch <- match(o2$WORD[j], remainingwords$WORD)
        if (is.na(actualmatch)) next
        o2$TOPIC[j] <- remainingwords$TOPIC[actualmatch]
        remainingwords <- remainingwords[-actualmatch,]
      }
      res[[i]] = o2
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
