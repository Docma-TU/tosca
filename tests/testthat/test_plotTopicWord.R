context("plot Words in Topics relative to Words")

test_that("plotTopicWord", {
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

  res1 <- plotTopicWord(object = obj, docs = LDAdoc, ldaresult = lda,
    ldaID = ldaID)
  expect_true(all(res1$date == seq(min(res1$date), max(res1$date), "month")))
  res2 <- plotTopicWord(object = obj, docs = LDAdoc, ldaresult = lda,
    ldaID = ldaID, unit = "week")
  expect_true(all(res2$date == seq(min(res2$date), max(res2$date), "week")))
  res3 <- plotTopicWord(object = obj, docs = LDAdoc, ldaresult = lda,
    ldaID = ldaID, pages = TRUE)
  expect_equal(res1, res3)
  res4 <- plotTopicWord(object = obj, docs = LDAdoc, ldaresult = lda,
    ldaID = ldaID,
    mark = FALSE, curves = "both", legend = "none", natozero = FALSE)
  expect_equal(res1, res4)
  res5 <- plotTopicWord(object = obj, docs = LDAdoc, ldaresult = lda,
    ldaID = ldaID, rel = TRUE, link = "or")
  expect_true(all(res5$date == res1$date), all(colnames(res1) == colnames(res5)),
    all(res5[, -1] <= 1))
  res6 <- plotTopicWord(object = obj, docs = LDAdoc, ldaresult = lda,
    ldaID = ldaID, file = file.path(tempdir(),"abc.pdf"))
  expect_equal(res1, res6)
  res7 <- plotTopicWord(object = obj, docs = LDAdoc, ldaresult = lda,
    ldaID = ldaID, curves = "smooth")
  expect_equal(res1, res7)
})
