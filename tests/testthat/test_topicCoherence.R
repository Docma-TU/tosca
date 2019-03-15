context("topicCoherence")

test_that("topicCoherence", {

suppressWarnings(RNGversion("3.5.0"))
set.seed(24601)
  
docs <- lapply(1:100, function(x)rbind(sample(0L:99L, 50, replace=TRUE), 1L))
docs <- c(docs, lapply(1:100, function(x)rbind(sample(50L:149L, 50, replace=TRUE), 1L)))

ldaresult <- LDAgen(documents=docs, K = 5L, vocab=as.character(0:149), num.iterations = 20L, burnin = 70L, seed=24602, num.words = 10L, LDA = TRUE, count=TRUE)

expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=10, by.score=TRUE),4), round(c(-41.74625, -38.26620, -34.25354, -35.56694, -47.67926),4))
expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=5, by.score=TRUE),4), round(c(-9.279865,  -7.622130,  -7.338766,  -6.881622, -11.796214),4))
expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=5, by.score=FALSE),4), round(c(-10.550004,  -7.724558,  -9.466624, -10.080576, -11.122454),4))
expect_error(topicCoherence(ldaresult=ldaresult, documents=docs[[-1]], num.words=5, by.score=FALSE))
expect_error(topicCoherence(ldaresult=ldaresult, documents=docs, num.words="5", by.score=FALSE))

})
