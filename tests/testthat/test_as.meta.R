context("transform a corpus object to textmeta")

test_that("as.textmeta.corpus", {
  meta <- data.frame(id = 1:3, additionalVariable = matrix(5, ncol = 4, nrow = 3))
  expect_true(isMeta(as.meta(meta, dateFormat = "a")))
  
  meta <- data.frame(id = 1:3, title = "title", date = "2019-06-11")
  expect_false(isMeta(as.meta(meta)))
  expect_true(isMeta(as.meta(meta, dateFormat = "%Y-%m-%d")))
  meta <- meta[, -1]
  expect_true(isMeta(as.meta(meta, dateFormat = "%Y-%m-%d")))
  
  expect_equal(colnames(as.meta(meta))[1:3], c("id", "date", "title"))
  
})
