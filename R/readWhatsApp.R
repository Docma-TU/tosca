#' Read WhatsApp files
#'
#' Reads HTML-files from WhatsApp and separates the text and meta data.
#'
#' @param path Character: string with path where the data files are.
#' @param file Character: string with names of the HTML files.
#' @return \code{\link{textmeta}} object.
#' @author Jonas Rieger (<jonas.rieger@@tu-dortmund.de>)
#' @keywords manip
#'
#' @export readWhatsApp

readWhatsApp = function(path = getwd(), file = list.files(path = path,
  pattern = "*.html$", full.names = FALSE, recursive = TRUE)){
  
  text = NULL
  meta = NULL
  filenames = substr(file, 1, nchar(file)-5)
  readTime = Sys.time()
  
  for (i in seq_along(file)) {
    #message("read WhatsApp-Chat \"", filenames[i], "\" ...")
    article = readLines(con = paste(path, file[i], sep="/"), encoding = "UTF-8")
    lines = grep(pattern = "<div class=\"vW7d1((.){7})?\">", x = article)
    
    articlespan = article[min(lines):length(article)]
    obs = tail(
      unlist(strsplit(paste(articlespan, collapse = " "),
        "<div class=\"vW7d1((.){7})?\">")), n = -1)
    # jeweils erster Wert des Splits keine Beobachtung!
    
    mData = data.frame(id = paste(filenames[i], seq_along(obs), sep = "."), title = NA_character_,
      stringsAsFactors = FALSE)
    # IDs noch genauer setzen. Iwie den Namen des Chats einbringen,
    # siehe dafuer article[lines] vor obs
    newtext = character(length(obs))
    
    mData$systemActivity = grepl(pattern = "<div class=\"_3_7SH Zq3Mc tail\">", obs)
    mData$systemDate = grepl(pattern = "<div class=\"_3_7SH Zq3Mc\">", obs)
    mData$systemSecurity = grepl(pattern = "<div class=\"_3_7SH _14b5J Zq3Mc tail\">", obs)
    mData$systemInfo = grepl(pattern = "<div class=\"_3_7SH _3DFk6 _2pwyf( message-(in|out))?( tail)?\">", obs)
    mData$systemAccount = grepl(pattern = "<div class=\"_3_7SH _3T8jk Zq3Mc tail\">", obs)
    
    systemMessage = mData$systemActivity | mData$systemDate | mData$systemSecurity | mData$systemInfo | mData$systemAccount
    newtext[systemMessage] = removeXML(stringr::str_extract(obs[systemMessage],
      "<div class=\"_3_7SH( _14b5J)? Zq3Mc( tail)?\">(.*?)</div>"))
    
    mData$userMessage = !systemMessage
    mData$textMessage = grepl(pattern = "<div class=\"_3_7SH _3DFk6( message-(in|out))?( tail)?\">", obs)
    mData$imageMessage = grepl(pattern = "<div class=\"_3_7SH _3qMSo( message-(in|out))?( tail)?\">", obs)
    mData$audioMessage = grepl(pattern = "<div class=\"_3_7SH _1gqYh( message-(in|out))?( tail)?\">", obs) |
      grepl(pattern = "<div class=\"_3_7SH _17oKL( message-(in|out))?( tail)?\">", obs)
    mData$videoMessage = grepl(pattern = "<div class=\"_3_7SH _3In2e( message-(in|out))?( tail)?\">", obs)
    mData$stickerMessage = grepl(pattern = "<div class=\"_3_7SH _1rK-b( message-(in|out))?( tail)?\">", obs)
    mData$gifMessage = grepl(pattern = "<div class=\"_3_7SH _2hOiI( message-(in|out))?( tail)?\">", obs)
    header = stringr::str_extract(obs[mData$userMessage],
      "<div class=\"(.*?)copyable-text\" data-pre-plain-text=\"(.*?)\"><div")
    header = stringr::str_extract(header,
      "copyable-text\" data-pre-plain-text=\"(.*?)\"><div")
    mData$posix[mData$userMessage] = substr(header, 38, 54)
    mData$posix = trimws(gsub(pattern = "\\]", x = mData$posix, replacement = ""))
    mData$posix = as.POSIXct(mData$posix, format = "%H:%M, %d.%m.%Y", "CET")
    mData$date = as.Date(mData$posix)
    mData$author[mData$userMessage] = substr(header, 55, nchar(header) - 8)
    mData$author = trimws(gsub(pattern = "\\]", x = mData$author, replacement = ""))
    
    textandemojis = stringr::str_extract(obs[mData$userMessage],
      "<((span dir=\"ltr\" class=\")|(div class=\"_2x9bY ))selectable-text invisible-space copyable-text\">(.*?)</div>")
    
    # emojji detection and extraction
    newtext[mData$userMessage] = removeXML(.extractEmojis(textandemojis))
    
    mData$forward[mData$userMessage] = grepl(
      pattern = "<span data-icon=\"forward-indicator\" class=\"ySrGA\">",
      x = obs[mData$userMessage])
    mData$cited[mData$userMessage] = grepl(pattern = "<span dir=\"auto\" class=\"quoted-mention\">",
      x = obs[mData$userMessage])
    citedtmp = mData$cited
    citedtmp[is.na(citedtmp)] = FALSE
    if(any(citedtmp)){
      mData$citedAuthor[citedtmp] = removeXML(stringr::str_extract(obs[citedtmp],
        "<span dir=\"auto\" class=\"((_3Ye_R _1wjpf)|(_2a1Yw))\">(.*?)</div>"))
      mData$citedText[citedtmp] = removeXML(.extractEmojis2(stringr::str_extract(obs[citedtmp],
        "<span dir=\"auto\" class=\"quoted-mention\">(.*?)</div>")))
      # Zitat koennte Emojis enthalten?!
    }
    else{
      mData$citedAuthor = NA
      mData$citedText = NA
    }
    
    mData$time[mData$userMessage] = removeXML(stringr::str_extract(obs[mData$userMessage],
      "<span class=\"_3EFt_\">(.*?)</span>"))
    
    meta = rbind(meta, mData)
    
    names(newtext) = mData$id
    text = c(text, newtext)
  }
  res = textmeta(meta = meta, text = as.list(text))
  readTime = difftime(Sys.time(), readTime)
  cat("Time for Reading Data:", round(as.numeric(readTime), 2), attributes(readTime)$units,
    "\n----------------------------------------------------------------------\n")
  summary(res)
}

.extractEmojis = function(textandemojis){
  # emojji detection and extraction with "data-plain-text" Flag
  textandemojis = strsplit(textandemojis, "<img crossorigin")
  tmp = cumsum(lengths(textandemojis))
  indsplit = rbind(c(1, head(tmp, -1)+1), tmp)
  textandemojis = unlist(textandemojis)
  indemoji = grepl(x = textandemojis, pattern = "data-plain-text=\"(.*?)\">")
  tmp = stringr::str_extract(string = textandemojis[indemoji],
    pattern = "data-plain-text=\"(.*?)\">")
  tmp = stringr::str_extract(string = tmp, pattern = "data-plain-text=\"(.*?)\"")
  textandemojis[indemoji] = paste(substr(tmp, 18, nchar(tmp)-1),
    removeXML(paste("<", textandemojis[indemoji])))
  textandemojis = sapply(seq_len(ncol(indsplit)), function(x)
    paste(textandemojis[indsplit[1,x]:indsplit[2,x]], collapse = " "))
  return(textandemojis)
}

.extractEmojis2 = function(textandemojis){
  # emojji detection and extraction with "alt" Flag
  textandemojis = strsplit(textandemojis, "<img crossorigin")
  tmp = cumsum(lengths(textandemojis))
  indsplit = rbind(c(1, head(tmp, -1)+1), tmp)
  textandemojis = unlist(textandemojis)
  indemoji = grepl(x = textandemojis, pattern = "alt=\"(.*?)\">")
  tmp = stringr::str_extract(string = textandemojis[indemoji],
    pattern = "alt=\"(.*?)\">")
  tmp = stringr::str_extract(string = tmp, pattern = "alt=\"(.*?)\"")
  textandemojis[indemoji] = paste(substr(tmp, 6, nchar(tmp)-1),
    removeXML(paste("<", textandemojis[indemoji])))
  textandemojis = sapply(seq_len(ncol(indsplit)), function(x)
    paste(textandemojis[indsplit[1,x]:indsplit[2,x]], collapse = " "))
  return(textandemojis)
}
