context("Plotting Frequency Analysis")

test_that("plotFreq", {
  load("data/tm.RData")
  set.seed(123)
  id = sample(tm$meta$id, 0.1*nrow(tm$meta))
  
  tab = plotFreq(tm, id = id, wordlist = c("tu", "dortmund"))
  expect_equal(tab$tu, c(1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         1, 0, 0, 0, 1, 0, 2))
  expect_equal(tab$dortmund, tab$tu)
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    curves = "both", smooth = 0.1), tab)
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    curves = "smooth"), tab)
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    unit = "year")$tu, c(3, 3, 1, 1, 1, 0, 3, 0, 1, 1, 3))
  expect_equal(plotFreq(tm, id = id, wordlist = c("tu", "dortmund"),
    unit = "year", rel = TRUE)$tu_rel, c(1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1))
  expect_equal(plotFreq(tm, id = id, wordlist = list(c("tu", "dortmund")),
    link = "or", unit = "year", type = "words", mark = FALSE)$tu.dortmund,
    c(55, 165, 49, 20, 114, 0, 183, 0, 45, 17, 389))
  expect_equal(plotFreq(tm, id = id, wordlist = list(c("tu", "dortmund")),
    link = "or", unit = "year", type = "words", mark = FALSE,
    rel = TRUE)$tu.dortmund_rel,
    c(0.204460966542751, 0.205479452054795, 0.2, 0.229885057471264, 0.17351598173516,
      0, 0.19572192513369, 0, 0.23936170212766, 0.166666666666667, 0.188926663428849))
  expect_error(plotFreq(object = tm$meta))
  expect_error(plotFreq(tm))
})
