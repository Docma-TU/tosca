#' Read WhatsApp files
#'
#' Reads HTML-files from WhatsApp and separates the text and meta data. The
#' functions \code{datemining} and \code{authormining} can be used to deduce
#' some missing values (concerning the author or date tag) from the data itself.
#'
#' @param path Character: string with path where the data files are.
#' If only \code{path} is given, \code{file} will be determined by searching
#' for html files with \code{\link[base]{list.files}} and recursion.
#' @param file Character: string with names of the HTML files.
#' @return \code{\link{textmeta}} object.
#' @author Jonas Rieger (<jonas.rieger@@tu-dortmund.de>)
#' @keywords manip
#'
#' @export readWhatsApp
readWhatsApp = function(path, file){
  if(missing(file)){
    if(missing(path)) path = getwd()
    if(any(grepl(pattern = "\\.html$", x = path, ignore.case = TRUE))){
      return(readWhatsApp.file(file = path))
    }else{
      return(readWhatsApp.file(file = list.files(path = path, pattern = "*.html$",
        full.names = TRUE, recursive = TRUE, ignore.case = TRUE)))
    }
  }
  if(missing(path)){
    return(readWhatsApp.file(file = file))
  }
  return(readWhatsApp.file(file = file.path(path, file)))
}

readWhatsApp.file = function(file){
  
  stopifnot(is.character(file), all(file.access(file, mode = 4) == 0))
  
  text = NULL
  meta = NULL
  filenames = gsub(x = gsub(x = file, pattern = "\\.html$", replacement = ""),
    pattern = ".*[/\\]+", replacement = "")
  readTime = Sys.time()
  
  for (i in seq_along(file)) {
    #message("read WhatsApp-Chat \"", filenames[i], "\" ...")
    article = readLines(con = file[i], encoding = "UTF-8")
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

#' @rdname readWhatsApp
#' @param object \code{\link{textmeta}} object, result from readWhatsapp.
#' @export datemining.WA
datemining.WA = function(object){
  
  stopifnot(is.textmeta(object),
    all(c("systemDate", "userMessage") %in% colnames(object$meta)))
  
  noDate = which(object$meta$userMessage & is.na(object$meta$date))
  
  for (i in seq_along(noDate)){
    tmp = which.max(object$meta$systemDate[seq(noDate[i], nrow(object$meta))]) #findet ersten TRUE
    tmp = tmp + noDate[i] - 1 #berechnet Index
    nextDate = object$text[[object$meta$id[tmp]]]
    
    tmp = which.max(object$meta$systemDate[seq(noDate[i], 1)]) #findet ersten TRUE rueckwaerts
    tmp = noDate[i] - tmp + 1 #berechnet Index
    prevDate = object$text[[object$meta$id[tmp]]]
    
    nextDate = as.Date(nextDate, format = "%d.%m.%Y")
    prevDate = as.Date(prevDate, format = "%d.%m.%Y")
    daydiff = as.numeric(difftime(nextDate, prevDate, "day"))
    if(!is.na(daydiff) && daydiff == 1){
      object$meta$date[noDate[i]] = prevDate
    }
  }
  
  newnoDate = noDate[is.na(object$meta$date[noDate])]
  
  for (i in seq_along(newnoDate)){
    tmp = which.max(!is.na(object$meta$date[seq(newnoDate[i], nrow(object$meta))])) #findet ersten TRUE
    ind1 = tmp + newnoDate[i] - 1 #berechnet Index
    nextDate = object$meta$date[ind1]
    
    tmp = which.max(!is.na(object$meta$date[seq(newnoDate[i], 1)])) #findet ersten TRUE rueckwaerts
    ind2 = newnoDate[i] - tmp + 1 #berechnet Index
    prevDate = object$meta$date[ind2]
    
    daydiff = as.numeric(difftime(nextDate, prevDate, "day"))
    if(!is.na(daydiff) && daydiff == 0){
      object$meta$date[newnoDate[i]] = prevDate
    }
    if(!any(object$meta$systemDate[ind2:newnoDate[i]])){
      object$meta$date[newnoDate[i]] = prevDate
    }
    if(!any(object$meta$systemDate[ind1:newnoDate[i]])){
      object$meta$date[newnoDate[i]] = nextDate
    }
  }
  
  return(object)
}

#' @rdname readWhatsApp
#' @export authormining.WA
authormining.WA = function(object){
  
  stopifnot(is.textmeta(object),
    all(c("author", "userMessage") %in% colnames(object$meta)))
  
  warning("\nadding author for user observations (replacing NAs by setting author to the most often author per ID-prefix - or to the ID-prefix itself, if no there is no known author at all): make sure that all user messages without author tag are sent from the main user in each chat!")
  noAuthor = which(object$meta$userMessage & is.na(object$meta$author))
  
  ids = gsub(pattern = "\\.[0-9]*", x = object$meta$id, replacement = "")
  
  object$meta$author[noAuthor] =
    sapply(gsub(pattern = "\\.[0-9]*", x = object$meta$id[noAuthor], replacement = ""),
      function(x) ifelse(!is.null(names(which.max(table(object$meta$author[ids == x])))),
        names(which.max(table(object$meta$author[ids == x]))), NA_character_))
  
  noAuthor = which(object$meta$userMessage & is.na(object$meta$author))
  
  object$meta$author[noAuthor] = gsub(pattern = "\\.[0-9]*",
    x = object$meta$id[noAuthor], replacement = "")
  return(object)
}
