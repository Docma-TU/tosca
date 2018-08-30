#' Read files from Wikinews
#'
#' Reads the XML-files from the Wikinews export page \url{https://en.wikinews.org/wiki/Special:Export}.
#'
#'
#' @param path Path where the data files are.
#' @param file Character string with names of the HTML files.
# #' @param do.meta Logical: Should the algorithm collect meta data?
# #' @param do.text Logical: Should the algorithm collect text data?
#' @return textmeta-object
#'
#' @export readWikinews

readWikinews <- function(path = getwd(), file = list.files(path = path, pattern = "*.xml$",
full.names = FALSE, recursive = TRUE)){

  temp_time <- Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  text <- NULL
  meta <- NULL
  metamult <- NULL
  for (i in seq_along(file)) {
    cat(paste(file[i]), "\n")
    article <- readLines(con = paste(path,file[i], sep="/"), encoding = "UTF-8")

    lines <- grep(pattern = "</page>", article)
    lines <- cbind(c(1,lines[-length(lines)]),lines)
    article <- apply(lines, 1, function(x)
      paste(article[x[1]:x[2]], collapse = " "))
    article <-  stringr::str_extract(article, "<page>(.*?)</page>")

    # meta
    id <- stringr::str_extract(article, "<ns>(.*?)</id>")
    id <- stringr::str_extract(id, "<id>(.*?)</id>")
    id <- removeXML(id)
    id <- paste0("ID",id)
    title <- stringr::str_extract(article, "<title>(.*?)</title>")
#    title <- removeHTML(removeXML(title), dec=FALSE, hex=FALSE, entity=FALSE)
    date <- stringr::str_extract(article, "\\{\\{(.*?)[Dd]ate(.*?)\\}\\}")
    date <- stringr::str_extract(date, "[^:alpha:][Dd]ate[\\|=](.*?)[\\}|]")
    date <- stringr::str_extract(date, "[:alpha:]*[ ]*[0-9]?[0-9], [1-2][0-9]{3}")
    date <- as.Date(date, format = "%B %d, %Y")

    mData <- data.frame(id, date, title, stringsAsFactors = FALSE)
    meta <- rbind(meta, mData)

    # text:
    text_new <- stringr::str_extract(article,"<text(.*?)==\\s?(Sources|Sister links)\\s?==")
#    text_new <- removeHTML(removeXML(text_new), dec=FALSE, hex=FALSE, entity=FALSE)
    text_new <- gsub(pattern = "\\{\\{(.*?)[Dd]ate[\\|=](.*?)\\}\\}",
      replacement = "", text_new, perl = TRUE)
    text_new <- gsub(pattern = "==\\s?(Sources|Sister links)\\s?==", replacement = "",
      text_new, perl = TRUE)
    text_new <- gsub(pattern = "\\{\\{haveyoursay\\}\\}", replacement = "",
      text_new, perl = TRUE)
    text_new <- gsub(pattern = "(\\{\\{|\\[\\[)([^(||\\}|\\]]*)(\\}\\}|\\]\\])",
      replacement = "\\2 ", text_new, perl = TRUE)
    text_new <- gsub(
      pattern = "(\\{\\{|\\[\\[)(:?w:?|)([^(\\||\\}|\\]]*)\\|([^(\\}|\\])]*)(\\}\\}|\\]\\])",
      replacement = "\\4 ", text_new, perl = TRUE)
    text_new <- gsub(pattern = "\\{\\{(.*?)\\}\\}", replacement = "", text_new, perl = TRUE)
    text_new <- gsub(pattern = "\\[\\[(.*?)\\]\\]", replacement = "", text_new, perl = TRUE)
    #text_new <- gsub(
    #  pattern = "(\\{\\{|\\[\\[)[[^(\\}|\\])]*(\\{|\\[)[^(\\}|\\])]*(\\}|\\])[^(\\}|\\])]*]*(\\}\\}|\\]\\])",
    #  replacement = "", text_new, perl = TRUE)
    text_new[is.na(text_new)] <- ""
    text_new <- trimws(text_new)
    names(text_new) <- id
    text <- as.list(c(text, text_new))
  }
  Sys.setlocale("LC_TIME", temp_time)
  res <- list("meta" = meta, "text" = text, "metamult" = metamult)
  class(res) <- "textmeta"
  summary(res)
}
