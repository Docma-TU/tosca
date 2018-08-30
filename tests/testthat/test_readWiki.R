context("read Wikipedia Pages")

test_that("readWiki", {
  a = readWiki("Journalism by continent")
  b = readWiki("Reporting specialties")
  c = readWiki("Newswriting", subcategories = FALSE)
  
  expect_true(all(is.textmeta(a), is.textmeta(b), is.textmeta(c)))
  expect_equal(length(a$text), nrow(a$meta))
  expect_equal(length(b$text), nrow(b$meta))
  expect_equal(length(c$text), nrow(c$meta))
  expect_equal(names(a$meta), c("id", "date", "title", "categoryCall", "touched"))
  expect_equal(names(a$meta), names(b$meta))
  expect_equal(names(b$meta), names(c$meta))
})
