context("Plotting Frequency Analysis")

test_that("plotFreq", {
  load("data/tm.RData")
  set.seed(123)
  id = sample(tm$meta$id, 0.1*nrow(tm$meta))
  
  tab = plotFreq(tm, id = id, wordlist = c("tu", "dortmund"))
  expect_equal(tab$tu, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,
                         0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))
  expect_equal(tab$dortmund, tab$tu)
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    curves = "both", smooth = 0.1), tab)
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    curves = "smooth"), tab)
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    unit = "year")$tu, c(1, 2, 3, 2, 1, 1, 0, 0, 0, 3, 1))
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    unit = "year", rel = TRUE)$tu_rel, c(1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1))
  expect_equal(plotFreq(tm, id = id, wordlist = list(c("tu", "dortmund")),
    link = "or", unit = "year", type = "words", mark = FALSE)$tu.dortmund,
    c(31, 69, 219, 229, 62, 31, 0, 0, 0, 355, 43))
  expect_equal(plotFreq(tm, id = id, wordlist = list(c("tu", "dortmund")),
    link = "or", unit = "year", type = "words", mark = FALSE,
    rel = TRUE)$tu.dortmund_rel,
    c(0.219858156028369, 0.209090909090909, 0.213658536585366, 0.194727891156463,
      0.207357859531773, 0.234848484848485, 0, 0, 0, 0.204257767548907, 0.187772925764192))
  expect_error(plotFreq(object = tm$meta))
  expect_error(plotFreq(tm))
})
