context("merge textmeta objects")

test_that("mergeTextmeta", {
  abc <- textmeta(text = list(a = "abcd", b = c("abcd", "de"), y = "xyz"))
  abc2 <- textmeta(
    text = list(a = "abcd", b = c("abcd", "de"), y = "xyz"),
    meta = data.frame(id = c("a", "b", "y"),
      date = as.Date(c("2017-01-01", "2017-01-01", "2017-01-01")),
      title = c("abc", "bed", "yxy"), stringsAsFactors = FALSE))
  m1 <- mergeTextmeta(list(abc, abc))
  m2 <- mergeTextmeta(list(abc, abc2))
  m3 <- mergeTextmeta(list(abc2, abc))
  m4 <- mergeTextmeta(list(abc2, abc2))
  m5 <- mergeTextmeta(list(abc, abc, abc2, abc, abc, abc2, abc2))
  expect_true(is.textmeta(m5))
  expect_equal(length(m5$text), 4*length(abc$text)+3*length(abc2$text))
  expect_true(
    all(is.textmeta(m1), is.textmeta(m2), is.textmeta(m3), is.textmeta(m4)))
  expect_error(mergeTextmeta(abc, abc))
  expect_equal(mergeTextmeta(list(abc, abc2), all = FALSE), m1)
})
