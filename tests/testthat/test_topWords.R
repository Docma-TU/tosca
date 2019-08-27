context("topWords")

test_that("topWords", {
  
  load("data/test-k3i20b70s24601alpha0.33eta0.33.RData")
  
  topics <- result$topics
  
  expect_equal(topWords(topics = topics),
    lda::top.topic.words(topics = topics, num.words = 1, by.score = TRUE))
  expect_equal(topWords(topics = topics, numWords = 20, byScore = FALSE),
    lda::top.topic.words(topics = topics))
  expect_error(topWords(topics = topics, epsilon = 0))
  
  tw <- topWords(topics = topics, values = TRUE)
  expect_true(is.list(tw))
  expect_true(all(names(tw) == c("word", "val")))
  
  imp <- importance(topics = topics)
  expect_equal(diag(imp[1:3, tw$word]), tw$val)
  
})
