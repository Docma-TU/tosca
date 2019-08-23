#' Topic Assignments per Text
#'
#' The function \code{topicsInText} creates a HTML document with the words of
#' texts colored depending on the topic allocation of each word. Analogously,
#' \code{showTopicsInText} extracts the matrix (matrices) of assignments to each
#' word of a text and \code{showTextTopics} extracts the matrix (matrices) of
#' assignments per topic and word in texts.
#' 
#' @param ldaresult LDA result.
#' @param documents The result of \code{\link{LDAprep}}
#' @param id Character vector containing the considered text ids
#' (default for \code{show} functions is \code{names(documents)}).
#' For \code{topicsInText} it has to be character string of length 1 (default is
#' a random id).
#' @param obj \code{\link{textmeta}} object (optional for \code{topicsInText}).
#' @param tnames Character vector containing the names for the topics
#' (default is determined by \code{\link[lda]{top.topic.words}}. For
#' \code{topicsInText} no labeling is done, if \code{tnames} is missing).
#' @param wordOrder Type of output: \code{"alphabetical"} prints the words of
#' the article in alphabetical order, \code{"topics"} sorts by topic
#' (biggest topic first) and \code{"both"} prints both versions. All other
#' inputs will result to no output (this is useful only in combination with \code{obj}).
#' @param colors Character vector of colors. If the vector is shorter than the
#' number of topics it will be completed by "black" entrys.
#' @param fixColors Logical: If \code{FALSE} the first color will be used for
#' the biggest topic and so on. If \code{fixColors=TRUE} the the color-entry
#' corresponding to the position of the topic is chosen.
#' @param unclearTopicAssignment Logical: If TRUE all words which are assigned
#' to more than one topic will not be colored. Otherwise the words will be
#' colored in order of topic apperance in the \code{ldaresult}.
#' @param htmlreturn Logical: HTML output for tests
#' @return
#' \describe{
#'   \item{\code{topicsInText}}{HTML document}
#'   \item{\code{showTopicsInText}}{A list of matrices specifying the assigned
#'   topics for each token of the requested texts. If file is set, for each
#'   text a csv is written.}
#'   \item{\code{showTextTopics}}{A list of topic-word-matrices of the requested
#'   texts. If file is set, for each text a csv is written.}
#' }
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
#' \donttest{
#' LDA <- LDAgen(documents = ldaPrep, K = 3L, vocab = wordlist$words, num.words = 3)
#' topicsInText(LDA, ldaPrep, obj = raw)
#' res <- showTopicsInText(raw, LDA, ldaPrep)
#' res2 <- showTextTopics(ldaresult = LDA, documents = ldaPrep)
#' }
#' 
#' @export topicsInText

topicsInText <- function(ldaresult, documents, id, obj,
    tnames, wordOrder = c("both", "alphabetical", "topics", ""), colors,
    fixColors = FALSE, unclearTopicAssignment = TRUE, htmlreturn = FALSE){
    
    if(missing(id)){
        id = sample(names(documents), 1)
    }
    
    stopifnot(missing(obj) || is.textmeta(obj),
        is.list(ldaresult), all(c("assignments", "topics") %in% names(ldaresult)),
        is.list(documents), all(sapply(documents, is.matrix)),
        length(ldaresult$assignments) == length(documents),
        all(lengths(ldaresult$assignments) == sapply(documents, ncol)),
        is.character(id), length(id) == 1, id %in% names(documents),
        is.logical(fixColors), length(fixColors) == 1,
        is.logical(unclearTopicAssignment), length(unclearTopicAssignment) == 1,
        is.logical(htmlreturn), length(htmlreturn) == 1,
        is.character(wordOrder))
    
    ## set colors if colors=NULL or 1:12
    if(missing(colors)){
        colors <- RColorBrewer::brewer.pal(n=12, name="Paired")[c(2*(1:6),2*(1:6)-1)]
    }
    if((is.integer(colors)|is.numeric(colors))&colors[1] %in%1:12){
        colors <- RColorBrewer::brewer.pal(n=12, name="Paired")[c(2*(1:6),2*(1:6)-1)][1:colors]
    }
    
    ## read article of interest
    texttopic <- data.frame(
        word = colnames(ldaresult$topics)[documents[[id]][1,]+1],
        topic = ldaresult$assignments[[which(names(documents)==id)]],
        stringsAsFactors = FALSE)
    topictable <- sort(table(texttopic$topic), decreasing=TRUE)
    
    ## delete unclear topic assignments
    if(!unclearTopicAssignment){
        multipleTopics <- split(texttopic$topic, f=texttopic$word)
        multipleTopics <- names(multipleTopics)[sapply(multipleTopics,
            function(x) length(unique(x))>1)]
        texttopic <- texttopic[!(texttopic$word %in% multipleTopics),]
    }
    
    ## set generic tnames if tnames missing and reduce tnames to used ones
    if(missing(tnames)) tnames <- paste("Topic", 1:(max(as.numeric(names(topictable)))+1))
    stopifnot(is.character(tnames), length(tnames) == nrow(ldaresult$topics))
    tnames <- tnames[as.numeric(names(topictable))+1]
    
    ## Additional (black colors, if length(colors) is too small)
    
    if(fixColors){
        colors <- c(colors, rep("#000000", 
            max(0, max(as.integer(names(topictable)))+1 - length(colors))))
        colors <- colors[as.integer(names(topictable))+1]
    }else{
        colors <- c(colors[1:min(length(colors), length(topictable))],
            rep("#000000", max(0, length(topictable) - length(colors))))
    }
    
    ## sort topics by number of words in article
    texttopic$topic <- match(texttopic$topic, names(topictable))
    
    ## print header
    if(missing(obj)){
        htmlOutput <- c("<h2>Document: ", id, "</h2><p>")
    }else{
        meta <- obj$meta
        meta$date <- as.character(meta$date)
        htmlOutput <- c("<h2>Document: ", meta$title[meta$id==id], "</h2><p>",
            paste0(names(meta), ": ", meta[meta$id==id,], "</br>"), collapse="", "</br>")
    }
    
    ## print topwords of topics
    ttw <- apply(
        lda::top.topic.words(ldaresult$topics, num.words = 20, by.score = TRUE),
        2, paste, collapse=" ")[as.numeric(names(topictable))+1]
    htmlOutput <- c(htmlOutput, paste0("<font color=", colors, "> ",
        tnames, ":",ttw ,"</font> </br>"))
    
    ## print words in alphabetical order
    if(wordOrder[1]=="both" | wordOrder[1]=="alphabetical"){
        texttopic2 <- texttopic[order(texttopic$word),]
        htmlOutput <- c(htmlOutput, "</br></br></br>",
            paste0("<font color=", colors[texttopic2$topic], "> ", texttopic2$word, "</font> "))
    }
    
    ## print words in topic order
    if(wordOrder[1]=="both" | wordOrder[1]=="topics"){
        texttopic2 <- texttopic[order(texttopic$topic),]
        htmlOutput <- c(htmlOutput, "</br></br></br>",
            paste0("<font color=", colors[texttopic2$topic], "> ", texttopic2$word, "</font> "))
    }
    
    ## print topics in original text
    if(!missing(obj)){
        originaltext <- obj$text[[id]]
        o2 <- unlist(strsplit(originaltext, split="\\s"))
        o2 <- o2[!(o2=="")]
        o2 <- data.frame(
            otext = o2,
            cleartext = removeNumbers(tolower(removePunctuation(o2))),
            color = "#000000",
            stringsAsFactors = FALSE)
        remainingwords <- texttopic
        for(i in 1:nrow(o2)){
            actualmatch <- match(o2$cleartext[i],remainingwords$word)
            if(is.na(actualmatch))next
            if(remainingwords$topic[actualmatch]>length(colors))next
            o2$color[i] <- colors[remainingwords$topic[actualmatch]]
            remainingwords <- remainingwords[-actualmatch,]
        }
        htmlOutput <- c(htmlOutput, "</br></br></br>",
            paste0("<font color=", o2$color, "> ", o2$otext, "</font> "))
    }
    ## print final html
    if(htmlreturn) return(htmlOutput)
    htmltools::html_print(htmltools::HTML(htmlOutput))
}

#' @rdname topicsInText
#' @param file Character File path for the export. If not specified the function
#' does not write. If \code{file = ""} the current file path is chosen.
#' @param prefix Character Prefix for each file (each text). File names are
#' build from prefix and the corresponding text id.
#' @param fileEncoding Character Declares file encoding. For more information
#' see \code{\link[utils]{write.csv}}.
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
        o2 <- unlist(strsplit(originaltext[[i]], split="\\s"))
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

#' @rdname topicsInText
#' @param vocab Character vector containing the words in the corpus
#' (default is \code{colnames(ldaresult$topics)}).
#' @export showTextTopics

showTextTopics = function(ldaresult, documents, id = names(documents),
    vocab = colnames(ldaresult$topics), tnames,
    file, prefix = "", fileEncoding = "UTF-8"){
    
    textind = match(id, names(documents))
    if (any(is.na(textind))){
        stop(paste0("ID(s) ", paste(id[is.na(textind)], collapse = ", "), " not found."))
    }
    
    stopifnot(is.list(ldaresult), all(c("assignments", "topics") %in% names(ldaresult)),
        is.list(documents), all(sapply(documents[textind], is.matrix)),
        length(ldaresult$assignments) == length(documents),
        all(lengths(ldaresult$assignments) == sapply(documents, ncol)),
        is.character(id), all(id %in% names(documents)),
        is.character(vocab), all(vocab %in% colnames(ldaresult$topics)))
    
    K = nrow(ldaresult$topics)
    if (missing(tnames))
        tnames = paste0("T", seq_len(nrow(ldaresult$topics)), ".",
            lda::top.topic.words(ldaresult$topics, 1, TRUE))
    
    stopifnot(is.character(tnames), length(tnames) == K, is.character(prefix),
        length(prefix) == 1, is.character(fileEncoding), length(fileEncoding) == 1,
        missing(file) || (is.character(file) && length(file) == 1))
    
    res = list()
    k = 1
    for (i in textind){
        tab = table(
            factor(ldaresult$assignments[[i]] + 1, levels = 1:K),
            documents[[i]][1,])
        colnames(tab) = vocab[as.integer(colnames(tab))+1]
        rownames(tab) = tnames
        tab = tab[, colnames(tab) %in% vocab, drop = FALSE]
        if (ncol(tab) == 0){
            tab = as.matrix(rep(0, K))
            colnames(tab) = "none of the words from vocab found in text"
            rownames(tab) = tnames
        }
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
