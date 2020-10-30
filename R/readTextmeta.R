#' Read Corpora as CSV
#'
#' Reads CSV-files and seperates the text and meta data. The result is a
#' \code{\link{textmeta}} object.
#' 
#' @param df \code{data.frame} table which should be transformed to a textmeta object
#' @param path \code{character/data.frame} string with path where the data files
#' are OR parameter \code{df} for \code{readTextmeta.df}
#' @param file \code{character} string with names of the CSV files
#' @param cols \code{character} vector with columns which should be kept
#' @param dateFormat \code{character} string with the date format in the files
#' for \code{\link{as.Date}}
#' @param idCol \code{character} string with column name of the IDs
#' @param dateCol \code{character} string with column name of the Dates
#' @param titleCol \code{character} string with column name of the Titles
#' @param textCol \code{character} string with column name of the Texts
#' @param encoding character string with encoding specification of the files
#' @param xmlAction \code{logical} whether all columns of the CSV should be
#' handled with \code{\link{removeXML}}
#' @param duplicateAction \code{logical}
#' whether \code{\link{deleteAndRenameDuplicates}} should be applied to the
#' created \code{\link{textmeta}} object
#' @return \code{\link{textmeta}} object
#' @keywords manip
#' @export readTextmeta

readTextmeta <- function(path, file, cols, dateFormat = "%Y-%m-%d", idCol = "id",
                         dateCol = "date", titleCol = "title", textCol = "text", encoding = "UTF-8",
                         xmlAction = TRUE, duplicateAction = TRUE){
  if(is.data.frame(path)){
    if(missing(cols)) cols = colnames(path)
    return(readTextmeta.df(df = path, cols = cols, dateFormat = dateFormat, idCol = idCol,
                           dateCol = dateCol, titleCol = titleCol, textCol = textCol,
                           xmlAction = TRUE, duplicateAction = TRUE))
  }
  # setting missed parameters
  if(missing(path)) path <- getwd()
  if(missing(file)) file <- list.files(path = path, pattern = "*.csv$",
                                       full.names = FALSE, recursive = TRUE)
  if(missing(cols)) keepAllCols <- TRUE
  else keepAllCols <- FALSE
  
  # stop if parameters set wrong
  stopifnot(is.character(file), is.character(path), length(path) == 1,
            is.logical(xmlAction), is.logical(duplicateAction), is.character(dateFormat),
            is.character(idCol), is.character(dateCol), is.character(titleCol),
            is.character(textCol), length(dateFormat) == 1, length(idCol) == 1,
            length(dateCol) == 1, length(titleCol) == 1, length(textCol) == 1,
            length(xmlAction) == 1, length(duplicateAction) == 1)
  
  # initialize
  text <- NULL
  meta <- NULL
  
  # go along all files
  for(i in seq(along = file)){
    message(paste0(i, "/", length(file), ": ", file[i]))
    
    # read in a lone file
    lonefile <- read.csv(file = file.path(path, file[i]), fileEncoding = encoding)
    
    # if keepAllCols: nothing to do
    # but: delete last two chars from colnames for the case that lonefile
    # is not a data.frame and cols is not specified yet. 
    if(keepAllCols) cols <- substr(colnames(lonefile), 1, nchar(colnames(lonefile))-2)
    else lonefile <- lonefile[, cols]
    if(!is.data.frame(lonefile)){
      lonefile <- data.frame(lonefile)
      colnames(lonefile) <- cols
    }
    
    # set important meta information to NA if not given in file
    if(!(idCol %in% colnames(lonefile))){
      message(paste0("NOTE: No ID-column \"", idCol, "\" in file, set to ascending numbers"))
      lonefile[, idCol] <- paste0("ID-", 1:nrow(lonefile) +
                                    ifelse(is.null(meta), 0, nrow(meta)))
    }
    if(!(dateCol %in% colnames(lonefile))){
      message(paste0("NOTE: No date-column \"", dateCol, "\" in file, set to NA"))
      lonefile[, dateCol] <- NA
    }
    if(!(titleCol %in% colnames(lonefile))){
      message(paste0("NOTE: No title-column \"", titleCol, "\" in file, set to NA"))
      lonefile[, titleCol] <- NA
    }
    if(!(textCol %in% colnames(lonefile))){
      message(paste0("NOTE: No text-column \"", textCol, "\" in file, set to NA"))
      lonefile[, textCol] <- NA
    }
    
    # remove XML tags
    if(xmlAction){
      for(j in seq(ncol(lonefile))){
        lonefile[, j] <- removeXML(lonefile[, j])
      }
    }
    
    # format date and rename id, date and title columns to standard
    lonefile[, dateCol] <- as.Date(lonefile[, dateCol], format = dateFormat)
    lonefile[, idCol] <- as.character(lonefile[, idCol])
    lonefile[, titleCol] <- as.character(lonefile[, titleCol])
    colnames(lonefile)[colnames(lonefile) == dateCol] <- "date"
    colnames(lonefile)[colnames(lonefile) == idCol] <- "id"
    colnames(lonefile)[colnames(lonefile) == titleCol] <- "title"
    
    # get text from file and name with id
    newText <- lonefile[, textCol]
    names(newText) <- lonefile$id
    
    # merge meta with existing meta information from other files
    lonefile <- data.frame(lonefile[, !(textCol == colnames(lonefile))],
                           stringsAsFactors = FALSE)
    meta <- rbind(meta, lonefile)
    
    # merge text with existing texts from other files
    text <- as.list(c(text, newText))
  }
  
  # create textmeta
  res <- list("meta" = meta, "text" = text, "metamult" = NULL)
  class(res) <- "textmeta"
  
  # remove duplicates
  if(duplicateAction) res <- deleteAndRenameDuplicates(res)
  
  # print summary to console
  summary(res)
}

#' @rdname readTextmeta
#' @export 
readTextmeta.df = function(df, cols = colnames(df), dateFormat = "%Y-%m-%d", idCol = "id",
                           dateCol = "date", titleCol = "title", textCol = "text",
                           xmlAction = TRUE, duplicateAction = TRUE){
  # stop if parameters set wrong
  stopifnot(is.data.frame(df), is.logical(xmlAction),
            is.logical(duplicateAction), is.character(dateFormat),
            is.character(idCol), is.character(dateCol), is.character(titleCol),
            is.character(textCol), length(dateFormat) == 1, length(idCol) == 1,
            length(dateCol) == 1, length(titleCol) == 1, length(textCol) == 1,
            length(xmlAction) == 1, length(duplicateAction) == 1)
  
  lonefile = df[,cols]
  if(!is.data.frame(lonefile)){
    lonefile <- data.frame(lonefile)
    colnames(lonefile) <- cols
  }
  
  if(!(idCol %in% colnames(lonefile))){
    message(paste0("NOTE: No ID-column \"", idCol, "\" in file, set to ascending numbers"))
    lonefile[, idCol] <- paste0("ID-", 1:nrow(lonefile))
  }
  if(!(dateCol %in% colnames(lonefile))){
    message(paste0("NOTE: No date-column \"", dateCol, "\" in file, set to NA"))
    lonefile[, dateCol] <- NA
  }
  if(!(titleCol %in% colnames(lonefile))){
    message(paste0("NOTE: No title-column \"", titleCol, "\" in file, set to NA"))
    lonefile[, titleCol] <- NA
  }
  if(!(textCol %in% colnames(lonefile))){
    message(paste0("NOTE: No text-column \"", textCol, "\" in file, set to NA"))
    lonefile[, textCol] <- NA
  }
  
  # remove XML tags
  if(xmlAction){
    for(j in seq(ncol(lonefile))){
      lonefile[, j] <- removeXML(lonefile[, j])
    }
  }
  
  # format date and rename id, date and title columns to standard
  lonefile[, dateCol] <- as.Date(lonefile[, dateCol], format = dateFormat)
  lonefile[, idCol] <- as.character(lonefile[, idCol])
  lonefile[, titleCol] <- as.character(lonefile[, titleCol])
  colnames(lonefile)[colnames(lonefile) == dateCol] <- "date"
  colnames(lonefile)[colnames(lonefile) == idCol] <- "id"
  colnames(lonefile)[colnames(lonefile) == titleCol] <- "title"
  
  # get text from file and name with id
  text <- as.list(lonefile[, textCol])
  names(text) <- lonefile$id
  
  res <- list("meta" = data.frame(lonefile[, !(textCol == colnames(lonefile))],
                                  stringsAsFactors = FALSE),
              "text" = text, "metamult" = NULL)
  class(res) <- "textmeta"
  
  # remove duplicates
  if(duplicateAction) res <- deleteAndRenameDuplicates(res)
  
  # print summary to console
  summary(res)
  
}
