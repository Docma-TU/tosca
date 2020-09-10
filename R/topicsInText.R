#' Coloring the words of a text corresponding to topic allocation
#'
#' The function creates a HTML document with the words of texts colored depending on the topic allocation of each word.
#'
#' @param text The result of \code{\link{LDAprep}}
#' @param ldaID List of IDs for \code{text}
#' @param id ID of the article of interest
#' @param ldaresult A result object from the \code{standardLDA}
#' @param label Optional label for each topic
#' @param vocab Character: Vector of \code{vocab} corresponding to the \code{text} object
#' @param wordOrder Type of output: \code{"alphabetical"} prints the words of the article in alphabetical order, \code{"topics"} sorts by topic (biggest topic first) and \code{"both"} prints both versions. All other inputs will result to no output (this makes only sense in combination with \code{originaltext}.
#' @param colors Character vector of colors. If the vector is shorter than the number of topics it will be completed by "black" entrys.
#' @param fixColors Logical: If \code{FALSE} the first color will be used for the biggest topic and so on. If \code{fixColors=TRUE} the the color-entry corresponding to the position of the topic is choosen.
#' @param meta Optional input for meta data. It will be printed in the header of the output.
#' @param originaltext Optional a list of texts (the \code{text} list of the \code{textmeta} object) including the desired text. Listnames must be IDs. Necessary for output in original text
#' @param unclearTopicAssignment Logical: If TRUE all words which are assigned to more than one topic will not be colored. Otherwise the words will be colored in order of topic apperance in the \code{ldaresult}.
#' @param htmlreturn Logical: HTML output for tests
#' @return A HTML document
#' @examples
#' \dontrun{
#' data(politics)
#' poliClean <- cleanTexts(politics)
#' words10 <- makeWordlist(text=poliClean$text)
#' words10 <- words10$words[words10$wordtable > 10]
#' poliLDA <- LDAprep(text=poliClean$text, vocab=words10)
#' LDAresult <- LDAgen(documents=poliLDA, K=10, vocab=words10)
#' topicsInText(text=politics$text, ldaID=names(poliLDA), id="ID2756",
#'              ldaresult=LDAresult, vocab=words10)}
#' @export topicsInText


topicsInText <- function(text, ldaID, id, ldaresult, label=NULL, vocab, wordOrder=c("both", "alphabetical", "topics", ""), colors=NULL, fixColors=FALSE, meta=NULL, originaltext=NULL, unclearTopicAssignment=TRUE, htmlreturn=FALSE){
    ## set colors if colors=NULL or 1:12
    if(is.null(colors)) colors <- RColorBrewer::brewer.pal(n=12, name="Paired")[c(2*(1:6),2*(1:6)-1)]
    if((is.integer(colors)|is.numeric(colors))&colors[1] %in%1:12) colors <- RColorBrewer::brewer.pal(n=12, name="Paired")[c(2*(1:6),2*(1:6)-1)][1:colors]

    ## read article of interest
    texttopic <- data.frame(word=vocab[text[[id]][1,]+1], topic=ldaresult$assignments[[which(ldaID==id)]], stringsAsFactors=FALSE)
    topictable <- sort(table(texttopic$topic), decreasing=TRUE)

    ## delete unclear topic assignments
    if(!unclearTopicAssignment){
        multipleTopics <- split(texttopic$topic, f=texttopic$word)
        multipleTopics <- names(multipleTopics)[sapply(multipleTopics,function(x)length(unique(x))>1)]
        texttopic <- texttopic[!(texttopic$word %in% multipleTopics),]
    }

    ## set generic label if label=NULL and reduce label to used ones
    if(is.null(label)) label <- paste("Topic", 1:(max(as.numeric(names(topictable)))+1))
    label <- label[as.numeric(names(topictable))+1]

    ## Additional (black colors, if length(colors) is too small)

    if(fixColors){colors <- c(colors, rep("#000000", max(0, max(as.integer(names(topictable)))+1 - length(colors))))
                  colors <- colors[as.integer(names(topictable))+1]
              }else{colors <- c(colors[1:min(length(colors), length(topictable))], rep("#000000", max(0, length(topictable) - length(colors))))
                }

    ## sort topics by number of words in article
    texttopic$topic <- match(texttopic$topic, names(topictable))

    ## print header
    if(is.null(meta)){htmlOutput <- c("<h2>Document: ", id, "</h2><p>")}else{
        meta$date <- as.character(meta$date)
        htmlOutput <- c("<h2>Document: ", meta$title[meta$id==id], "</h2><p>", paste0(names(meta), ": ", meta[meta$id==id,], "</br>"), collapse="", "</br>")}

    ## print topwords of topics
    ttw <- apply(lda::top.topic.words(ldaresult$topics, num.words = 20, by.score = TRUE), 2, paste, collapse=" ")[as.numeric(names(topictable))+1]
    htmlOutput <- c(htmlOutput, paste0("<font color=", colors, "> ", label, ":",ttw ,"</font> </br>"))

    ## print words in alphabetical order
    if(wordOrder[1]=="both" | wordOrder[1]=="alphabetical"){
        texttopic2 <- texttopic[order(texttopic$word),]
        htmlOutput <- c(htmlOutput, "</br></br></br>", paste0("<font color=", colors[texttopic2$topic], "> ", texttopic2$word, "</font> "))
    }

    ## print words in topic order
    if(wordOrder[1]=="both" | wordOrder[1]=="topics"){
        texttopic2 <- texttopic[order(texttopic$topic),]
        htmlOutput <- c(htmlOutput, "</br></br></br>", paste0("<font color=", colors[texttopic2$topic], "> ", texttopic2$word, "</font> "))
    }

    ## print topics in original text
    if(!is.null(originaltext)){
        originaltext <- originaltext[[id]]
        o2 <- unlist(strsplit(originaltext, split="\\s"))
        o2 <- o2[!(o2=="")]
        o2 <- data.frame(otext=o2, cleartext=removeNumbers(tolower(removePunctuation(o2))), color="#000000", stringsAsFactors=FALSE)
        remainingwords <- texttopic
        for(i in 1:nrow(o2)){
            actualmatch <- match(o2$cleartext[i],remainingwords$word)
            if(is.na(actualmatch))next
            if(remainingwords$topic[actualmatch]>length(colors))next
            o2$color[i] <- colors[remainingwords$topic[actualmatch]]
            remainingwords <- remainingwords[-actualmatch,]
        }
        htmlOutput <- c(htmlOutput, "</br></br></br>", paste0("<font color=", o2$color, "> ", o2$otext, "</font> "))
    }
    ## print final html
    if(htmlreturn) return(htmlOutput)
    htmltools::html_print(htmltools::HTML(htmlOutput))
}



