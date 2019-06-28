#' Creating List of Duplicates
#'
#' Creates a List of different types of Duplicates in a textmeta-object.
#'
#' This function helps to identify different types of Duplicates and gives the
#' ability to exclude these for further Analysis (e.g. LDA).
#'
#' @param object A textmeta-object.
#' @param paragraph Logical: Should be set to \code{TRUE} if the article is a
#' list of character strings, representing the paragraphs.
#' @return Named List:
#' \item{uniqueTexts}{ Character vector of IDs so that each text occurs once - if a text occurs twice or more often in the corpus, the ID of the first text regarding the list-order is returned}
#' \item{notDuplicatedTexts}{ Character vector of IDs of texts which are represented only once in the whole corpus}
#' \item{idFakeDups}{ List of character vectors: IDs of texts which originally has the same ID but belongs to different texts grouped by their original ID}
#' \item{idRealDups}{ List of character vectors: IDs of texts which originally has the same ID and text but different meta information grouped by their original ID}
#' \item{allTextDups}{ List of character vectors: IDs of texts which occur twice or more often grouped by text equality}
#' \item{textMetaDups}{ List of character vectors: IDs of texts which occur twice or more often and have the same meta information grouped by text and meta equality}
#'
#' @keywords manip
#' @examples
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' A="A fake duplicate",
#' B="So Long, and Thanks for All the Fish",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#'
#' corpus <- textmeta(meta=data.frame(id=c("A", "A", "B", "B", "C", "C"),
#' title=c("Fishing", "Fake duplicate", "Don't panic!", "towel day", "Sir Ronald", "Sir Ronald"),
#' date=c("1885-01-02", "1885-01-03", "1979-03-04", "1979-03-05", "1951-05-06", "1951-05-06"),
#' stringsAsFactors=FALSE), text=texts)
#'
#' duplicates <- deleteAndRenameDuplicates(object=corpus)
#' duplist(object=duplicates, paragraph = FALSE)
#' @export duplist

# Es existieren keine doppelten IDs durch deleteAndRenameDuplicates in read!
# Ausgabe soll Liste sein mit
# 1. uniqueTexts: Jeder vorkommende Text genau einmal
#    a) notDuplicatedTexts: Texte, fuer die keine identischen IDs oder Texte
#                       im Korpus vorkommen
# 2. idFakeDups: Texte mit gleicher ID und unterschiedlichem Text ("Reste" aus 3.)
# 3. idRealDups: Texte mit gleicher ID und gleichem Text (Meta differs)
# 4. (DIE EIGENTLICHE AUFGABE DIESER FUNKTION) Texte, deren Text gleich ist
#    allTextDups: mit allen Text-Duplikaten
#    a) textOnlyDups: mit unterschiedlichen Meta-Daten (insbesondere existiert kein
#                     Duplikat mit identischen Meta-Informationen)
#    b) textMetaDups: mit exakt gleichen Meta-Daten
#    c) textOthersDups: mit Duplikaten, die in allTextDups vorkommen, aber nicht
#                       in textMetaDups oder textOnlyDups.

duplist <- function(object, paragraph = FALSE){
  stopifnot(is.textmeta(object), is.logical(paragraph), length(paragraph) == 1)
  # help-function to create lists of IDs:
  foo_makeList <- function(dupType, to_replace){
    if (length(dupType) < 1) return(list())
    sorted_dups <- sort(names(object$text[dupType]))
    temp <- grep(to_replace, sorted_dups)
    dups_names <- gsub(pattern = to_replace, replacement = "", sorted_dups[temp])
    temp <- cbind(temp, c(temp[-1], length(sorted_dups) + 1) - 1)
    dupType <- apply(temp, 1, function(x) sorted_dups[x[1]:x[2]])
    if (is.matrix(dupType)){
      dupType <- as.list(as.data.frame(dupType, stringsAsFactors = FALSE))
    }
    if (!is.list(dupType)){
      dupType <- as.list(dupType)
    }
    names(dupType) <- dups_names
    return(dupType)
  }

  # 2. idFakeDups 3. idRealDups
  message("ID-Fake-Dups... ", appendLF = FALSE)
  idFakeDups <- foo_makeList(dupType = grep("_IDFakeDup", names(object$text)),
    to_replace = "_IDFakeDup1")
  message("next step\nID-Real-Dups... ", appendLF = FALSE)
  idRealDups <- foo_makeList(dupType = grep("_IDRealDup", names(object$text)),
    to_replace = "_IDRealDup1")
  message("next step\nunique (and not-duplicated) texts... ", appendLF = FALSE)

  # 1. uniqueTexts a) allUniqueTexts:
  if (paragraph){
    textvek <- unlist(lapply(object$text, paste, collapse = " "))
  }
  else textvek <- unlist(object$text)
  text_same <- duplicated(textvek)
  text_same_fromLast <- duplicated(textvek, fromLast = TRUE)
  # 1) uniqueTexts:
  if (any(!text_same)) uniqueTexts <- names(object$text)[!text_same]
  else uniqueTexts <- character(0)
  # 1a) notDuplicatedTexts:
  ind = text_same | text_same_fromLast
  if (any(!ind)) allUniqueTexts <- names(object$text)[!ind]
  else allUniqueTexts <- character(0)

  message("next step\nsame texts... ", appendLF = FALSE)
  # 4. Same text, but different IDs:
  if (any(ind)){
    ind <- which(ind)
    # allTextDups:
    allTextDups_names <- names(object$text)[ind]
    allTextDups <- lapply(na.omit(unique(textvek[ind])),
      function(x) allTextDups_names[textvek[ind] == x])

    # b) textMetaDups:
    doNotTestID <- colnames(object$meta)[colnames(object$meta) != "id"]
    ids <- unlist(allTextDups)
    metaind <- match(ids, object$meta$id)
    splitind <- rep(seq_len(length(allTextDups)), times = lengths(allTextDups))
    splitted <- apply(object$meta[metaind, doNotTestID], 1, paste, collapse = " ")
    names(splitted) <- ids
    splitted <- split(splitted, splitind)
    textMetaDups <- lapply(splitted, function(y) lapply(unique(y), function(x) names(y)[x == y]))

    # reduziere die textMetaDups-Liste:
    textMetaDups <- do.call(c, textMetaDups)
    textMetaDups <- unname(textMetaDups[lengths(textMetaDups) > 1])
  }
  else {
    allTextDups <- list()
    textMetaDups <- list()
  }
  message("success")
  res <- list(uniqueTexts = uniqueTexts, notDuplicatedTexts = allUniqueTexts,
    idFakeDups = idFakeDups, idRealDups = idRealDups,
    allTextDups = allTextDups, textMetaDups = textMetaDups)
  class(res) <- "duplist"
  summary(res)
  return(res)
}

#' @rdname duplist
#' @param x An R Object.
#' @export
is.duplist <- function(x){
  if(class(x) != "duplist"){
    message("Object is not of class \"duplist\".")
    return(FALSE)
  }
  if(!is.list(x)){
    message("Object is not a list.")
    return(FALSE)
  }
  if(!all(c("uniqueTexts", "notDuplicatedTexts", "idFakeDups", "idRealDups",
    "allTextDups", "textMetaDups") %in% names(x))){
    message("Listnames incorrect.")
    return(FALSE)
  }
  if(!all(is.character(x$uniqueTexts), is.character(x$notDuplicatedTexts),
    is.list(x$idFakeDups), is.list(x$idRealDups), is.list(x$allTextDups),
    is.list(x$textMetaDups))){
    message("Structure of List incorrect.")
    return(FALSE)
  }
  return(TRUE)
}

#' @rdname duplist
#' @param ... Further arguments for print and summary. Not implemented.
#' @export
print.duplist <- function(x, ...){
  if(!is.duplist(x)){
    print.default(x)
  }
  else{
    cat("duplist, list of (lists of) IDs with names:
 \"uniqueTexts\", \"notDuplicatedTexts\", \"idFakeDups\", \"idRealDups\",
 \"allTextDups\", \"textMetaDups\".\n")
    invisible(x)
  }
}

#' @rdname duplist
#' @export
summary.duplist <- function(object, ...){
  stopifnot(is.duplist(object))
  print(object)
  cat(length(object$uniqueTexts), "unique texts\n")
  cat(length(object$notDuplicatedTexts), "not-duplicated texts\n")
  cat(sum(lengths(object$idFakeDups)), "texts with",
    length(object$idFakeDups), "different Fake-Dup IDs (ID equals, text differs)\n")
  cat(sum(lengths(object$idRealDups)), "texts with",
    length(object$idRealDups), "different Real-Dup IDs (ID and text equals, meta differs)\n")
  cat(sum(lengths(object$allTextDups)), "text-duplicates with",
    length(object$allTextDups), "different texts\n")
  cat(sum(lengths(object$textMetaDups)), "(text and meta)-duplicates with",
    length(object$textMetaDups), "different (text and meta) combinations, excluding ID\n")
  invisible(object)
}
