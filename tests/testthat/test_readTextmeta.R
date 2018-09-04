context("read CSV files")

test_that("readTextmeta", {
  tm <- readTextmeta(path = file.path(getwd(),"data"), file = "readTextmeta.csv",
    dateCol = "date_gmt", textCol = "content")
  expect_true(is.textmeta(tm))
  expect_equal(length(tm$text), 3)
  expect_equal(nrow(tm$meta), 3)
  expect_equal(names(tm$text), c("ABC", "IDK100", "IWaS"))
  expect_false(any(sapply(tm$text, is.na)))
  expect_equal(tm$meta$id, names(tm$text))

  tm <- readTextmeta(path = file.path(getwd(),"data"), file = "readTextmeta.csv",
    cols = character())
  expect_true(is.textmeta(tm))
  expect_equal(length(tm$text), 3)
  expect_equal(ncol(tm$meta), 3)
  expect_equal(nrow(tm$meta), 3)
  expect_true(all(sapply(tm$text, is.na)))
  expect_true(all(is.na(tm$meta$date)))
  expect_true(all(is.na(tm$meta$title)))
  expect_equal(tm$meta$id, paste("ID", 1:3, sep = "-"))
  expect_equal(tm$meta$id, names(tm$text))

  tm <- readTextmeta(path = file.path(getwd(),"data"), file = "readTextmeta.csv",
    cols = "id")
  expect_true(is.textmeta(tm))
  expect_equal(length(tm$text), 3)
  expect_equal(ncol(tm$meta), 3)
  expect_equal(nrow(tm$meta), 3)
  expect_true(all(sapply(tm$text, is.na)))
  expect_true(all(is.na(tm$meta$date)))
  expect_true(all(is.na(tm$meta$title)))
  expect_equal(tm$meta$id, names(tm$text))
  expect_equal(names(tm$text), c("ABC", "IDK100", "IWaS"))
})
