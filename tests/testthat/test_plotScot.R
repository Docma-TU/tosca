context("plotting (Sub-)Corpora over Time")

test_that("plotScot", {
  load("data/tm.RData")
  set.seed(123)
  id = sample(tm$meta$id, 0.1*nrow(tm$meta))
  
  tab = plotScot(tm, id = id)
  expect_equal(tab$counts, c(2, 8, 9, 11, 11, 3, 7, 5, 6, 11, 16, 11, 11, 13, 12,
                             15, 16, 15, 10, 9, 12, 9, 10, 15, 14, 12, 7, 4, 12,
                             17, 21, 17, 10, 16, 9, 17, 12, 10, 3, 8, 7, 6, 6, 5,
                             6, 6, 6, 8, 10, 5, 1, 11, 4, 6, 6, 3, 0, 1, 1, 1, 0,
                             2, 1, 0, 0, 1, 1, 0, 3, 1, 1, 1, 5, 4, 0, 1, 2, 3, 2,
                             0, 2, 4, 1, 1, 1, 0, 1, 1, 4, 0, 0, 3, 3, 1, 2, 0, 1,
                             1, 1, 0, 1, 3, 1, 1, 0, 3, 2, 4, 7, 3, 3, 0, 2, 6, 3,
                             6, 4, 2, 4, 3, 6, 2, 4, 3, 5, 1, 3, 3, 1, 2, 1, 4, 4,
                             4, 4, 4))
  expect_equal(plotScot(tm, id = id, curves = "both", smooth = 0.1), tab)
  expect_equal(plotScot(tm, id = id, curves = "smooth"), tab)
  expect_equal(plotScot(tm, id = id, unit = "year")$counts,
               c(51, 141, 142, 121, 74, 11, 23, 15, 17, 34, 43, 27))
  expect_equal(plotScot(tm, id = id, unit = "year", rel = TRUE)$proportion,
    c(0.103238866396761, 0.107963246554364, 0.096271186440678, 0.1, 0.0933165195460277,
      0.064327485380117, 0.14375, 0.1171875, 0.125, 0.0988372093023256,
      0.0945054945054945, 0.0685279187817259))
  expect_equal(plotScot(tm, id = id, unit = "year", type = "words", rel = TRUE,
    mark = FALSE)$proportion,
    c(0.114820846905537, 0.0827275006267235, 0.137620837808808, 0.238684798051553,
      0.169597277368123, 0.121883656509695, 0, 0, 0, 0.591156462585034,
      0.0648909039387929, 0))
  expect_error(plotScot(object = tm$meta))
})
