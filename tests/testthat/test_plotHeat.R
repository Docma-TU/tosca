context("plot Topics as Heatmap including Clustering")

test_that("plotHeat", {
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(123)
  x1 <- matrix(sample(c(rep(0, 20), 1:20), 10000, replace = TRUE), 10, 1000)
  ldaID <- paste("ID", 1:200)
  x2 <- list(document_sums = x1)

  text <- matrix(sample(paste("word", 1:100), 10000, replace = TRUE), 200, 50)
  text <- lapply(apply(text, 1, list), unlist)
  names(text) <- paste("ID", 1:200)

  words <- makeWordlist(text)$words
  LDAdoc <- LDAprep(text, words)
  lda <- LDAgen(documents = LDAdoc, K = 3L, vocab = words,
    num.iterations = 20L, burnin = 70L, seed = 123)

  meta1 <- as.Date(sample(1:730, 1200, replace = TRUE), origin = "1990-10-03")
  names(meta1) <- paste("ID", 1:1200)
  meta <- data.frame(id = paste("ID", 1:1200), date = meta1,
    title = as.character(NA), stringsAsFactors = FALSE)

  obj <- textmeta(text = text, meta = meta)

  res1 <- plotHeat(object = obj, ldaresult = lda, ldaID = ldaID, file = file.path(tempdir(),"abc.pdf"))
  expect_equal(dim(res1), c(3, 4))
  res2 <- plotHeat(object = obj, ldaresult = lda, ldaID = ldaID, unit = "month",
    file = file.path(tempdir(),"abc.pdf"))
  expect_true(all(res2$date == seq(min(res2$date), max(res2$date), "month")))
  res3 <- plotHeat(object = obj, ldaresult = lda, ldaID = ldaID, file = file.path(tempdir(),"abc.pdf"),
    norm = TRUE)
  expect_equal(dim(res3), c(3, 4))
  res4 <- plotHeat(object = textmeta(meta = obj$meta), ldaresult = lda,
    ldaID = ldaID, file = file.path(tempdir(),"abc.pdf"))
  expect_equal(dim(res4), c(3, 4))
  expect_true(all(res4$date == seq(min(res4$date), max(res4$date), "year")))
  expect_true(all(res3$date == seq(min(res3$date), max(res3$date), "year")))
  expect_true(all(res1$date == seq(min(res1$date), max(res1$date), "year")))
})
