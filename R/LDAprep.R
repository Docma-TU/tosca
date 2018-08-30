#' Create Lda-ready Dataset
#'
#' This function transforms a text corpus such as the result of
#' \code{\link{cleanTexts}} into the form needed by the \code{\link{lda}}-package.
#'
#'
#' @param text A list of tokenized texts
#' @param vocab A character vector containing all words which should beused for
#' lda
#' @param reduce Logical: Should empty texts be deleted?
#' @return A list in which every entry contains a matrix with two rows: The
#' first row gives the number of the entry of the word in \code{vocab} minus
#' one, the second row is 1 and the number of the
#' occurrence of the word will be shown by the number of columns belonging to
#' this word.
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
#' LDAprep(text=corpus$text, vocab=wordlist$words, reduce = TRUE)
#'
#' @export LDAprep
#' @import data.table

LDAprep <- function(text, vocab,
                    reduce = TRUE){
    stopifnot(is.textmeta(textmeta(text = text)), is.character(vocab),
              is.logical(reduce), length(reduce) == 1)
    text <- lapply(text, unlist)
    . <- NULL # for cran check
    id <- NULL # for cran check

    vtable <- data.table::data.table(word = vocab, id = seq_along(vocab) - 1L, key = "word")
    text <- mapply(
        function(article, words) {
            rbind(vtable[.(words), "id", nomatch = 0L][order(id)]$id, 1L)
        }, article = names(text), words = text, SIMPLIFY = FALSE)
                                        # ltable = rbindlist(mapply(
                                        #   function(article, words) {
                                        #   data.table(word = words, article = article)[, list(article = article, N = .N), by = "word"]
                                        # }, article = names(text), words = text, SIMPLIFY = FALSE))
                                        #
                                        # ltable = merge(ltable, vtable, all.x = FALSE, all.y = FALSE, on = "word")
                                        # rm(vtable)
                                        # res = ltable[, list(x = list(rbind(id, 1))), by = article]
                                        # res = setNames(x$x, x$article)
                                        #})
                                        # x(split(tmp$id, tmp$article)
                                        #
    if(reduce){                         # delete entries where dimension is not computable
        tmp <- lengths(lapply(text, dim)) == 0
        if (length(tmp) > 0) text <- text[!tmp]
        Dim <- sapply(text, dim)
        text <- text[Dim[2,] != 0]
        text <- text[Dim[1,] != 1]
    }
    return(text)
}
