context("plot Words in Subcorpora")

test_that("plotWordSub", {
  set.seed(123)
  x1 <- matrix(sample(c(rep(0, 20), 1:20), 10000, replace = TRUE), 10, 1000)
  ldaID <- paste("ID", 1:200)
  x2 <- list(document_sums = x1)

  text <- matrix(sample(paste("word", 1:100), 10000, replace = TRUE), 200, 50)
  text <- lapply(apply(text, 1, list), unlist)
  names(text) <- paste("ID", 1:200)

  words <- makeWordlist(text)$words
  LDAdoc <- LDAprep(text, words)
  lda <- LDAgen(documents = LDAdoc, K = 8L, vocab = words,
    num.iterations = 20L, burnin = 70L, seed = 123)

  meta1 <- as.Date(sample(1:730, 1200, replace = TRUE), origin = "1990-10-03")
  names(meta1) <- paste("ID", 1:1200)
  meta <- data.frame(id = paste("ID", 1:1200), date = meta1,
    title = as.character(NA), stringsAsFactors = FALSE)

  obj <- textmeta(text = text, meta = meta)
  search <- paste("word", 12:15)

  reslim1 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, limit = 0.51)
  reslim2 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, limit = 0.51, alloc = "unique")
  expect_equal(reslim1, reslim2)
  resalloc1 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, limit = 0.4, alloc = "best", rel = FALSE)
  resalloc2 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, limit = 0.4, alloc = "unique", rel = FALSE)
  resalloc3 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, limit = 0.4, rel = FALSE)
  resalloc4 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, limit = 0.9, alloc = "best", rel = FALSE)
  expect_true(all(resalloc1 >= resalloc2))
  expect_true(all(resalloc3 >= resalloc2))
  expect_equal(resalloc1, resalloc4)
  restype1 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, alloc = "best", rel = FALSE, type = "words")
  expect_true(all(restype1[,-1] == resalloc1[,-1]*50))
  restype2 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, alloc = "best", type = "words")
  expect_true(all(restype2[,-1] <= 1))
  res1 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID, search = search)
  expect_true(all(res1$date == seq(min(res1$date), max(res1$date), "month")))
  res2 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    unit = "week", search = search)
  expect_true(all(res2$date == seq(min(res2$date), max(res2$date), "week")))
  res3 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, select = 1:4)
  expect_true(ncol(res3) == 5)
  expect_error(plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    search = search, select = 1:12))
  res4 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    mark = FALSE, curves = "both", legend = "none", natozero = FALSE, search = search)
  res4neu <- res4
  res4neu[is.na(res4neu)] <- 0
  expect_equal(res1, res4neu)
  res5 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    rel = TRUE, search = search)
  expect_true(all(res5$date == res1$date), all(colnames(res1) == colnames(res5)),
    all(res5[, -1] <= 1))
  res6 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    file = file.path(tempdir(),"abc.pdf"), search = search)
  expect_equal(res1, res6)
  res7 <- plotWordSub(object = obj, ldaresult = lda, ldaID = ldaID,
    curves = "smooth", search = search)
  expect_equal(res1, res7)
})
