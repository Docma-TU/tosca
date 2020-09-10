#' Read Pages from Wikipedia
#'
#' Downloads pages from Wikipedia and extracts some meta information
#' with functions from the package \code{\link{WikipediR}}. Creates a
#' \code{\link{textmeta}} object including the requested pages.
#'
#'
#' @param category \code{character} articles of which category should be
#' downloaded, see \code{\link{pages_in_category}}, argument \code{categories}
#' @param subcategories \code{logical} (default: \code{TRUE}) should
#' subcategories be downloaded as well
#' @param language \code{character} (default: \code{"en"}),
#' see \code{\link{pages_in_category}}
#' @param project \code{character} (default: \code{"wikipedia"}),
#' see \code{\link{pages_in_category}}
#' @return \code{\link{textmeta}} object
#' @keywords manip
#' @examples
#' \dontrun{corpus <- readWiki(category="Person_(Studentenbewegung)",
#' subcategories = FALSE, language = "de", project = "wikipedia")}
#' @export readWiki
#'

readWiki <- function(category, subcategories = TRUE,
  language = "en", project = "wikipedia"){

  stopifnot(
    is.character(category), length(category) == 1,
    is.logical(subcategories), length(subcategories) == 1,
    is.character(language), length(language) == 1,
    is.character(project), length(project) == 1)

  level1pages <-
    WikipediR::pages_in_category(
      language = language, project = project, type = "page", limit = 500,
      properties = c("id", "title", "timestamp"),
      categories = category)$query$categorymembers
  subs <- NULL
  level2pages <- NULL
  if (subcategories){
    subs <-
      WikipediR::pages_in_category(
        language = language, project = project, type = "subcat", limit = 500,
        properties = "title", categories = category)$query$categorymembers
    subs <- gsub("Category:", "", sapply(subs, function(x) x$title))
    level2pages <-
      do.call(c,
        lapply(subs, function(x)
          WikipediR::pages_in_category(
            language = language, project = project, type = "page", limit = 500,
            properties = c("id", "title", "timestamp"),
            categories = x)$query$categorymembers))
  }
  pages = c(level1pages, level2pages)

  message("downloading ", length(pages), " articles in the category \"", category,
    "\" and ", length(subs), " subcategories...")

  id <- sapply(pages, function(x) x$pageid)
  title <- sapply(pages, function(x) x$title)
  date <- as.Date(sapply(pages, function(x) x$timestamp))
  categoryCall <- category
  touched <-
    as.Date(
      sapply(pages,
        function(x) WikipediR::page_info(language = language, project = project,
          page = x$title)$query$pages[[1]]$touched))

  meta <- data.frame(id = as.character(id), date = date, title = title,
    categoryCall = categoryCall, touched = touched, stringsAsFactors = FALSE)

  text <- lapply(meta$title,
    function(x) WikipediR::page_content(language = language, project = project,
      page_name = x)$parse$text$`*`)
  names(text) <- meta$id
  return(textmeta(meta = meta, text = text))
}
