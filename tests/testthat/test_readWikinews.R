context("read Wikipedia Pages")

test_that("readWikinews", {
  corp = readWikinews(path = "data/Wikinews")
  corp1 = readWikinews(path = "data/Wikinews", file = "Wikinews1.xml")
  corp2 = readWikinews(path = "data/Wikinews", file = "Wikinews2.xml")

  expect_true(is.textmeta(corp))
  expect_true(is.textmeta(corp1))
  expect_true(is.textmeta(corp2))
  expect_equal(mergeTextmeta(list(corp1, corp2)), corp)

  expect_equal(length(corp$text), nrow(corp$meta))
  expect_equal(length(corp$text), 4)
  expect_equal(length(corp1$text), nrow(corp1$meta))
  expect_equal(length(corp1$text), 3)
  expect_equal(length(corp2$text), nrow(corp2$meta))
  expect_equal(length(corp2$text), 1)

  expect_equal(corp$meta$date, as.Date(c("2018-03-30", "2018-04-04", "2018-04-07", "2018-03-31")))
  expect_equal(corp$meta$id, names(corp$text))
  expect_equal(corp$meta$id, c("ID2838764", "ID2839675", "ID2839732", "ID2839233"))
  expect_equal(corp1$meta$date, as.Date(c("2018-03-30", "2018-04-04", "2018-04-07")))
  expect_equal(corp1$meta$id, names(corp1$text))
  expect_equal(corp1$meta$id, c("ID2838764", "ID2839675", "ID2839732"))
  expect_equal(corp2$meta$date, as.Date("2018-03-31"))
  expect_equal(corp2$meta$id, names(corp2$text))
  expect_equal(corp2$meta$id, "ID2839233")
})
