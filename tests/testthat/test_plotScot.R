context("plotting (Sub-)Corpora over Time")

test_that("plotScot", {
  load("data/tm.RData")
  set.seed(123)
  id = sample(tm$meta$id, 0.1*nrow(tm$meta))
  
  tab = plotScot(tm, id = id)
  expect_equal(tab$counts, c(2, 6, 9, 7, 7, 7, 5, 10, 7, 13, 16, 9, 11, 12, 10,
                             16, 12, 17, 10, 12, 8, 8, 13, 9, 14, 11, 12, 9, 14,
                             15, 13, 13, 9, 14, 5, 19, 11, 9, 10, 9, 5, 11, 7, 7,
                             9, 8, 8, 3, 9, 5, 4, 5, 6, 4, 6, 4, 1, 2, 0, 0, 1, 1,
                             1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 4, 1, 0, 1, 0, 4, 2,
                             2, 1, 0, 2, 1, 1, 1, 0, 2, 1, 3, 1, 0, 1, 1, 0, 0, 5,
                             1, 0, 0, 1, 2, 4, 4, 2, 3, 4, 0, 6, 2, 3, 3, 3, 3, 4,
                             3, 3, 1, 3, 5, 2, 8, 4, 6, 4, 3, 6, 4, 10, 9, 3, 5, 2, 5, 4))
  expect_equal(plotScot(tm, id = id, curves = "both", smooth = 0.1), tab)
  expect_equal(plotScot(tm, id = id, curves = "smooth"), tab)
  expect_equal(plotScot(tm, id = id, unit = "year")$counts, c(43, 143, 138, 122, 74, 12,
                                                              14, 16, 12, 37, 46, 48))
  expect_equal(plotScot(tm, id = id, unit = "year", rel = TRUE)$proportion,
    c(0.0870445344129555, 0.109494640122511, 0.0935593220338983, 0.100826446280992,
      0.0933165195460277, 0.0701754385964912, 0.0875, 0.125, 0.0882352941176471,
      0.107558139534884, 0.101098901098901, 0.121827411167513))
  expect_equal(plotScot(tm, id = id, unit = "year", type = "words", rel = TRUE,
    mark = FALSE)$proportion, c(0, 0.0674354474805716, 0.1078141783029, 0.0497259995940735,
                                0.0493477027793534, 0.606648199445983, 0, 0.474860335195531,
                                0, 0.0639455782312925, 0.0289033720600737, 0.634905951279679))
  expect_error(plotScot(object = tm$meta))
})
