#' Deletes and Renames Articles with the same ID
#'
#' Deletes articles with the same ID and same text. Renames the ID of articles
#' with the same ID but different text-component (_IDFakeDup, _IDRealDup).
#'
#' @param object A \code{textmeta} object as a result of a read-function.
#' @param renameRemaining Logical: Should all articles for which a counterpart with the same
#' id exists, but which do not have the same text and - in addition - which matches (an)other
#' article(s) in the text field be named a "fake duplicate" or not.
#' @details Summary: Different types of duplicates:
#'  "complete duplicates" = same ID, same information in text, same information in meta
#'  "real duplicates" = same ID, same information in text, different information in meta
#'  "fake duplicates" = same ID, different information in text
#' @return A filtered \code{textmeta} object with updated IDs.
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
#' duplicates$meta$id
#' 
#' texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' A="Give a Man a Fish, and You Feed Him for a Day.
#' Teach a Man To Fish, and You Feed Him for a Lifetime",
#' A="A fake duplicate",
#' B="So Long, and Thanks for All the Fish",
#' B="So Long, and Thanks for All the Fish",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.",
#' C="A very able manipulative mathematician, Fisher enjoys a real mastery
#' in evaluating complicated multiple integrals.")
#' 
#' corpus <- textmeta(meta=data.frame(id=c("A", "A", "A", "B", "B", "C", "C"),
#' title=c("Fishing", "Fishing2", "Fake duplicate", "Don't panic!", "towel day",
#' "Sir Ronald", "Sir Ronald"),
#' date=c("1885-01-02", "1885-01-02", "1885-01-03", "1979-03-04", "1979-03-05",
#' "1951-05-06", "1951-05-06"),
#' stringsAsFactors=FALSE), text=texts)
#' 
#' duplicates <- deleteAndRenameDuplicates(object=corpus)
#' duplicates2 <- deleteAndRenameDuplicates(object=corpus, renameRemaining = FALSE)
#' 
#' @export deleteAndRenameDuplicates
#'

# 1. Artikel-IDs, deren IDs gleich, der Text aber unterschiedlich ist
#    hier fallen auch all diejenigen Eintraege drunter, fuer die es eine identische
#    ID gibt, die ggf aber auch als RealDup bereits deklariert wurde, weil es diese
#    ID auch mit identischem text gibt.
#     -> hier _IDFakeDup1 ... _IDFakeDupn anhaengen
#    (hier keine Option fuer gleiche Meta-Daten, da Text unterschiedlich,
#     also sind gleiche Meta-Daten nicht zu erwarten)
# 2. Artikel mit identischer ID und Text (aber unterschiedlichen Meta-Daten)
#     -> hier _IDRealDup1 ... _IDRealDupn anheangen
# 3. Artikel mit komplett identischen ID, Text, Meta werden geloescht!!


deleteAndRenameDuplicates <- function(object, renameRemaining = TRUE){
  stopifnot(is.textmeta(object), is.logical(renameRemaining), length(renameRemaining) == 1,
            (is.null(object$meta) || all(names(object$text) == object$meta$id)))
  id = NULL
  i = NULL
  
  if (is.null(object$meta)){ #if do.meta == FALSE:
    ind <- which(duplicated(names(object$text)) | duplicated(names(object$text), fromLast = TRUE))
    if (length(ind) < 1) return(object)
    
    dat = data.table(i = seq_len(length(object$text))[ind],
                     text = sapply(lapply(object$text, unlist), paste0, collapse = " ")[ind])
    to_del = duplicated(dat[,-1])
    if (sum(to_del) > 0){
      message(paste("delete \"complete duplicates\":", sum(to_del)), appendLF = FALSE)
      object$text <- object$text[-dat[to_del,i]]
      message("  next step")
    }
    
    # Rename if text differs:
    to_rename = duplicated(names(object$text)) | duplicated(names(object$text), fromLast = TRUE)
    if (sum(to_rename) > 0){
      message(paste("rename \"fake duplicates\":", length(to_rename)), appendLF = FALSE)
      dat = data.table(i = which(to_rename), id = names(object$text)[to_rename])
      dat[, id := paste0(id, "_IDFakeDup", seq_len(.N)), by = id]
      names(object$text)[dat[, i]] = dat[, id]
      message("  next step")
    }
    message("success")
    return(object)
  }
  ind = duplicated(names(object$text)) | duplicated(names(object$text), fromLast = TRUE)
  if(any(ind)){
    # Ansonsten existieren text und meta:
    # 3. Artikel mit komplett identischen ID, Text, Meta werden geloescht:
    dat = as.data.table(cbind(seq_len(length(object$text))[ind],
                              sapply(lapply(object$text, unlist), paste0, collapse = " ")[ind],
                              object$meta[ind,]))
    to_del = duplicated(dat[,-1])
    if (sum(to_del) > 0){
      message(paste("delete \"complete duplicates\":", sum(to_del)), appendLF = FALSE)
      object$text <- object$text[-unlist(dat[to_del,1])]
      object$meta <- object$meta[-unlist(dat[to_del,1]),]
      message("  next step")
    }
    ind = duplicated(names(object$text)) | duplicated(names(object$text), fromLast = TRUE)
    dat = data.table(i = seq_len(length(object$text))[ind],
                     text = sapply(lapply(object$text, unlist), paste0, collapse = " ")[ind],
                     id = object$meta$id[ind])
    # 2. Artikel mit identischer ID und Text (aber unterschiedlichen Meta-Daten):
    to_rename = duplicated(dat[, 2:3]) | duplicated(dat[, 2:3], fromLast = TRUE)
    if (sum(to_rename) > 0){
      message(paste("rename \"real duplicates\":", sum(to_rename)), appendLF = FALSE)
      dat[to_rename, id := paste0(id, "_IDRealDup", seq_len(.N)), by = c("text", "id")]
      names(object$text)[dat[to_rename, i]] = dat[to_rename, id]
      object$meta$id[dat[to_rename, i]] = dat[to_rename, id]
      message("  next step")
      dat = dat[!to_rename,]
    }
    # 1. Artikel-IDs, deren IDs gleich, der Text aber unterschiedlich ist:
    to_rename = duplicated(dat[, id]) | duplicated(dat[, id], fromLast = TRUE)
    if (sum(to_rename) > 0){
      message(paste("rename \"fake duplicates\":", sum(to_rename)), appendLF = FALSE)
      dat[to_rename, id := paste0(id, "_IDFakeDup", seq_len(.N)), by = id]
      names(object$text)[dat[to_rename, i]] = dat[to_rename, id]
      object$meta$id[dat[to_rename, i]] = dat[to_rename, id]
      dat = dat[!to_rename,]
    }
    if (renameRemaining && nrow(dat) > 0){
      message(paste("\nrename remaining \"fake duplicates\":", nrow(dat)), appendLF = FALSE)
      names(object$text)[dat[,i]] = paste0(dat[, id], "_IDFakeDup1")
      object$meta$id[dat[,i]] = paste0(dat[, id], "_IDFakeDup1")
    }
  }
  message("  success")
  return(object)
}
