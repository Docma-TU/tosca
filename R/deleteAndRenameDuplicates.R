#' Deletes and Renames Articles with the same ID
#'
#' Deletes articles with the same ID and same text. Renames the ID of articles
#' with the same ID but different text-component (_IDFakeDup, _IDRealDup).
#'
#' @param object A \code{textmeta} object as a result of a read-function.
#' @param paragraph Logical: Should be set to \code{TRUE} if the article is a
#' list of character strings, representing the paragraphs.
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
#' @export deleteAndRenameDuplicates
#'

# 1. Artikel-IDs, deren IDs gleich, der Text aber unterschiedlich ist
#     -> hier _IDFakeDup1 ... _IDFakeDupn anhaengen
#    (hier keine Option fuer gleiche Meta-Daten, da Text unterschiedlich,
#     also sind gleiche Meta-Daten nicht zu erwarten)
# 2. Artikel mit identischer ID und Text (aber unterschiedlichen Meta-Daten)
#     -> hier _IDRealDup1 ... _IDRealDupn anheangen
# 3. Artikel mit komplett identischen ID, Text, Meta werden geloescht!!


deleteAndRenameDuplicates <- function(object, paragraph = FALSE){
  stopifnot(is.textmeta(object), is.logical(paragraph), length(paragraph) == 1)

  if (is.null(object$meta)){ #if do.meta == FALSE:
    ind <- which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                             fromLast = TRUE))
    if (length(ind) < 1) return(object)
    if (paragraph == TRUE){
      textvek <- unlist(lapply(object$text[ind], paste, collapse = " "))
    }
    else textvek <- unlist(object$text[ind])
    # Delete duplicates of ID !and! text:
    to_del <- ind[duplicated(textvek)]
    if (length(to_del) > 0){
      message(paste("delete \"complete duplicates\":", length(to_del)), appendLF = FALSE)
      object$text <- object$text[-to_del]
      message("  next step")
    }
    # Rename if text differs:
    to_rename <- which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                                   fromLast = TRUE))
    if (length(to_rename) > 0){
      ind_loop = logical(length(names(object$text)))
      ind_loop[to_rename] = TRUE
      message(paste("rename \"fake duplicates\":", length(to_rename)), appendLF = FALSE)
      for (i in na.omit(unique(names(object$text)[to_rename]))){
        to_rename_loop <- (names(object$text) == i) & ind_loop
        to_rename_loop[is.na(to_rename_loop)] <- FALSE
        names(object$text)[to_rename_loop] <- paste0(names(object$text)[to_rename_loop],
                                                     "_IDFakeDup", 1:sum(to_rename_loop))
      }
      message("  next step")
    }
    message("success")
    return(object)
  }
  # Ansonsten existieren text und meta:
  # 3. Artikel mit komplett identischen ID, Text, Meta werden geloescht:
  ind <- which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                           fromLast = TRUE))
  if (length(ind) < 1){
    message("success")
    return(object)
  }
  if (paragraph == TRUE){
    textvek <- unlist(lapply(object$text[ind], paste, collapse = " "))
  }
  else textvek <- unlist(object$text[ind])
  to_del <- ind[duplicated(object$meta[ind,]) & duplicated(textvek)]
  if (length(to_del) > 0){
    message(paste("delete \"complete duplicates\":", length(to_del)), appendLF = FALSE)
    object$text <- object$text[-to_del]
    object$meta <- object$meta[-to_del,]
    #object$metamult <- object$metamult[-to_del]
    message("  next step")
    ind <- which(duplicated(names(object$text)) | duplicated(names(object$text),
                                                             fromLast = TRUE))
  }

  # 2. Artikel mit identischer ID und Text (aber unterschiedlichen Meta-Daten):
  if (length(ind) < 1){
    message("success")
    return(object)
  }
  if (paragraph == TRUE){
    textvek <- unlist(lapply(object$text[ind], paste, collapse = " "))
  }
  else textvek <- unlist(object$text[ind])
  text_same <- duplicated(textvek) | duplicated(textvek, fromLast = TRUE)
  to_rename <- ind[text_same]
  if (length(to_rename) > 0){
    ind_loop = logical(length(names(object$text)))
    ind_loop[to_rename] = TRUE
    message(paste("rename \"real duplicates\":", length(to_rename)), appendLF = FALSE)
    for (i in na.omit(unique(names(object$text)[to_rename]))){
      to_rename_loop <- names(object$text) == i & ind_loop
      to_rename_loop[is.na(to_rename_loop)] <- FALSE
      new_ids <- paste0(names(object$text)[to_rename_loop], "_IDRealDup",
                        1:sum(to_rename_loop))
      names(object$text)[to_rename_loop] <- new_ids
      object$meta$id[to_rename_loop] <- new_ids
      #if (!is.null(object$metamult))
      #  names(object$metamult)[to_rename_loop] <- new_ids
    }
    message("  next step")
  }

  # 1. Artikel-IDs, deren IDs gleich, der Text aber unterschiedlich ist:
  to_rename <- ind[!text_same]
  if (length(to_rename) < 1){
    message("success")
    return(object)
  }
  ind_loop = logical(length(names(object$text)))
  ind_loop[to_rename] = TRUE
  message(paste("rename remaining \"fake duplicates\":", length(to_rename)), appendLF = FALSE)
  for (i in na.omit(unique(names(object$text)[to_rename]))){
    to_rename_loop <- names(object$text) == i & ind_loop
    to_rename_loop[is.na(to_rename_loop)] <- FALSE
    new_ids <- paste0(names(object$text)[to_rename_loop], "_IDFakeDup",
                      1:sum(to_rename_loop))
    names(object$text)[to_rename_loop] <- new_ids
    object$meta$id[to_rename_loop] <- new_ids
    #if (!is.null(object$metamult))
    #  names(object$metamult)[to_rename_loop] <- new_ids
  }
  message("  success")
  return(object)
}
