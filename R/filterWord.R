#' Subcorpus With Word Filter
#'
#' Generates a subcorpus by restricting it to texts containing specific filter Words
#'
#' @param object A \code{\link{textmeta}} object
#' @param text Not necessary if \code{object} is specified, else should be
#' \code{object\$text}: list of article texts.
#' @param search List of data frames. Every List element is an 'or'
#' link, every entry in a data frame is linked by an 'and'. The dataframe must have following tree variables: \code{pattern} a character string including the search terms, \code{word}, a logical value displaying if a word (TRUE) or character (search) is wanted and \code{count} an integer marking how many times the word must at least be found in the text. \code{word} can alternatively be a character string containing the keywords \code{pattern} for character search, \code{word} for word-search and \code{left} and \code{right} for truncated search.
#' If \code{wordlist} is only a character Vector the link is 'or', and a character search will be used with \code{count=1}
#' @param ignore.case Logical: Lower and upper case will be ignored.
#' @param out Type of output: \code{text} filtered corpus, \code{bin} logical vector for all texts, \code{count} the number of matches (max one match per character string).
#' @return Filtered list of texts.
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#'
#' # search for pattern "fish"
#' filterWord(text=texts, search="fish", ignore.case=TRUE)
#'
#' # search for word "fish"
#' filterWord(text=texts, search=data.frame(pattern="fish", word="word", count=1),
#' ignore.case=TRUE)
#'
#' # pattern must appear at least two times
#' filterWord(text=texts, search=data.frame(pattern="fish", word="pattern", count=2),
#' ignore.case=TRUE)
#'
#' # search for "fish" AND "day"
#' filterWord(text=texts, search=data.frame(pattern=c("fish", "day"), word="word", count=1),
#' ignore.case=TRUE)
#'
#' # search for "Thanks" OR "integrals"
#' filterWord(text=texts, search=list(data.frame(pattern="Thanks", word="word", count=1),
#' data.frame(pattern="integrals", word="word", count=1)))
#'
#'
#' @export filterWord
filterWord <- function(object, text, search, ignore.case = FALSE,
                          out = c("text", "bin", "count")){

    returnTextmeta <- FALSE
    if (!missing(object)){
      stopifnot(is.textmeta(object))
      text <- object$text
      returnTextmeta <- TRUE
    }

    stopifnot(is.textmeta(textmeta(text = text)),
      is.logical(ignore.case), length(ignore.case) == 1, is.character(out),
      all(out %in% c("text", "bin", "count")))

    text <- lapply(text, unlist)
    if(ignore.case) text <- lapply(text, tolower)
    if(is.character(search)) search <- lapply(search, function(x)
      data.frame(pattern = x, word = FALSE, count = 1, stringsAsFactors = FALSE))
    if(is.data.frame(search)) search <- list(search)

    stopifnot(is.list(search), all(sapply(search, is.data.frame)),
      all(sapply(search, function(x) c("pattern", "word", "count") %in% names(x))),
      all(sapply(search, function(x) all(as.character(x$pattern) == x$pattern))),
      all(sapply(search, function(x) is.logical(x$word) |
          (all(as.character(x$word) == x$word) & all(x$word %in% c("word", "pattern", "left", "right"))))),
      all(sapply(search, function(x) is.numeric(x$count))),
      all(sapply(search, function(x) all(as.integer(x$count) == x$count))))

    subid <- integer(length(text))
    counts_out <- NULL

    for(i in 1:length(search)){
        search[[i]]$pattern <- as.character(search[[i]]$pattern)
        pattern <- search[[i]]$pattern
        if(ignore.case) search[[i]]$pattern <- tolower(search[[i]]$pattern)
        if(is.logical(search[[i]]$word))search[[i]]$word <- c("pattern", "word")[search[[i]]$word+1]
        search[[i]]$pattern[search[[i]]$word == "word"] <- paste0("\\b",search[[i]]$pattern[search[[i]]$word == "word"], "\\b")
        search[[i]]$pattern[search[[i]]$word == "left"] <- paste0(search[[i]]$pattern[search[[i]]$word == "left"],"\\b")
        search[[i]]$pattern[search[[i]]$word == "right"] <- paste0("\\b",search[[i]]$pattern[search[[i]]$word == "right"])
        counts <- NULL

        for(j in 1:nrow(search[[i]])){
            count <- lapply(text, stringr::str_count, pattern= search[[i]]$pattern[j])
            count <- sapply(count, sum)
            counts <- cbind(counts,count)
        }
        colnames(counts) <- pattern
        subcandidate <- apply(counts, 1, function(x)all(x>=search[[i]]$count))

        subid <- subid + subcandidate
        if(out[1] == "count"){#colnames(counts) <- pattern
                                     if(ignore.case)colnames(counts) <- paste0(colnames(counts), "_case")
                                     colnames(counts)[search[[i]]$word == "word"] <- paste0(colnames(counts)[search[[i]]$word == "word"], "_w")
                                     colnames(counts)[search[[i]]$word == "left"] <- paste0(colnames(counts)[search[[i]]$word == "left"], "_l")
                                     colnames(counts)[search[[i]]$word == "right"] <- paste0(colnames(counts)[search[[i]]$word == "right"], "_r")
                                     counts_out <- cbind(counts_out, counts)
                                     }
    }

    subid <- subid > 0
    subid[is.na(subid)] <- FALSE
    if(out[1] == "text"){
      if(returnTextmeta){
        object$text <- text[subid]
        object$meta <- object$meta[object$meta$id %in% names(object$text), ]
        return(object)
      }
      return(text[subid])
    }
    if(out[1] == "bin") return(subid)
    if(out[1] == "count") return(counts_out)
}
