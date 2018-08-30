context("Plotting Frequency Analysis")

test_that("plotFreq", {
  load("data/tm.RData")
  set.seed(123)
  id = sample(tm$meta$id, 0.1*nrow(tm$meta))
  
  tab = plotFreq(tm, id = id, wordlist = c("tu", "dortmund"))
  expect_equal(tab$tu, c(2, 8, 9, 11, 11, 3, 7, 5, 6, 11, 16, 11, 11, 13, 12,
    14, 14, 15, 10, 9, 12, 9, 10, 15, 14, 12, 7, 4, 12, 17, 20, 17, 10, 16, 9,
    17, 12, 10, 3, 8, 7, 6, 6, 5, 6, 6, 6, 8, 10, 5, 1, 10, 4, 5, 6, 3, 0, 1, 1,
    1, 0, 2, 1, 0, 0, 1, 1, 0, 3, 1, 1, 1, 5, 4, 0, 1, 2, 3, 2, 0, 2, 4, 1, 1,
    1, 0, 1, 1, 4, 0, 0, 3, 3, 1, 2, 0, 1, 1, 1, 0, 1, 3, 1, 1, 0, 3, 2, 4, 7,
    3, 3, 0, 2, 6, 3, 6, 4, 2, 4, 3, 6, 2, 3, 3, 5, 1, 3, 3, 1, 2, 1, 4, 4, 4,
    4, 4))
  expect_equal(tab$dortmund, c(2, 8, 9, 11, 11, 3, 7, 5, 6, 11, 16, 11, 11, 13,
    12, 14, 14, 15, 10, 9, 12, 9, 10, 15, 14, 12, 7, 4, 12, 17, 20, 17, 10, 16,
    9, 17, 12, 10, 3, 8, 7, 6, 6, 5, 6, 6, 6, 8, 10, 5, 1, 11, 4, 5, 6, 3, 0, 1,
    1, 1, 0, 2, 1, 0, 0, 1, 1, 0, 3, 1, 1, 1, 5, 4, 0, 1, 2, 3, 2, 0, 2, 4, 1,
    1, 1, 0, 1, 1, 4, 0, 0, 3, 3, 1, 2, 0, 1, 1, 1, 0, 1, 3, 1, 1, 0, 3, 2, 4,
    7, 3, 3, 0, 2, 6, 3, 6, 4, 2, 4, 3, 6, 2, 4, 3, 5, 1, 3, 3, 1, 2, 1, 4, 4,
    4, 4, 4))
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    curves = "both", smooth = 0.1), tab)
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    curves = "smooth"), tab)
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    unit = "year")$tu, c(51, 138, 141, 121, 72, 11, 23, 15, 17, 34, 42, 27))
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    unit = "year", rel = TRUE)$tu_rel, c(1, 0.992805755395683, 1, 1,
      0.986301369863014, 1, 1, 1, 1, 1, 0.976744186046512, 1))
  expect_equal(plotFreq(tm, id = id, wordlist = list(c("tu", "dortmund")),
    link = "or", unit = "year", type = "words", mark = FALSE)$tu.dortmund,
    c(1809, 4780, 5473, 5485, 3223, 594, 1425, 780, 1026, 2686, 2615, 2090))
  expect_equal(plotFreq(tm, id = id, wordlist = list(c("tu", "dortmund")),
    link = "or", unit = "year", type = "words", mark = FALSE,
    rel = TRUE)$tu.dortmund_rel,
    c(0.196395613939855, 0.197667686709122, 0.19914853358562, 0.203381660424932,
      0.202883041671912, 0.184586699813549, 0.194539249146758,
      0.197119029567854, 0.196363636363636, 0.2, 0.197343596709682,
      0.197523863528967))
  expect_error(plotFreq(object = tm$meta))
  expect_error(plotFreq(tm))
})
