context("topicCoherence")

test_that("topicCoherence", {

docs <- lapply(1:100, function(x)rbind(sample(0L:99L, 50, replace=TRUE), 1L))
docs <- c(docs, lapply(1:100, function(x)rbind(sample(50L:149L, 50, replace=TRUE), 1L)))

suppressWarnings(RNGversion("3.5.0"))
set.seed(24601)
ldaresult <- LDAgen(documents=docs, K = 5L, vocab=as.character(0:149), num.iterations = 20L, burnin = 70L, seed=24602, num.words = 10L, LDA = TRUE, count=TRUE)

expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=10, by.score=TRUE),5), c(-41.23167, -40.07306, -37.49477, -47.19478, -34.73816))
expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=5, by.score=TRUE),5), c(-8.82447,  -9.10893,  -8.18058, -11.22174,  -8.41985))
expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=5, by.score=FALSE),5), c(-8.82644,  -9.08993, -12.66089, -10.72207,  -7.63619))

})
