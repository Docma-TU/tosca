context("read Wikipedia Pages")

test_that("readWiki", {

  try(Wiki1 <- readWiki("Journalism by continent"))
  try(Wiki2 <- readWiki("Reporting specialties"))
  try(Wiki3 <- readWiki("Newswriting", subcategories = FALSE))
  
  if(!exists("Wiki1") | !exists("Wiki2") | !exists("Wiki3")){
    message("Problems with internet recource in readWiki")
  }
  
  if(exists("Wiki1")){
    expect_true(is.textmeta(Wiki1))
    expect_equal(length(Wiki1$text), nrow(Wiki1$meta))
    expect_equal(names(Wiki1$meta), c("id", "date", "title", "categoryCall", "touched"))
  }
  if(exists("Wiki2")){
    expect_true(is.textmeta(Wiki2))
    expect_equal(length(Wiki2$text), nrow(Wiki2$meta))
  }
  if(exists("Wiki3")){
    expect_true(is.textmeta(Wiki3))
    expect_equal(length(Wiki3$text), nrow(Wiki3$meta))
  }
  
  if(exists("Wiki1") & exists("Wiki2") & exists("Wiki3")){
    expect_equal(names(Wiki1$meta), names(Wiki2$meta))
    expect_equal(names(Wiki2$meta), names(Wiki3$meta))
  }

})
