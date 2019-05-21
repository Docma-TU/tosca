context("topicCoherence")

test_that("topicCoherence", {

suppressWarnings(RNGversion("3.5.0"))
set.seed(24601)
  
docs <- lapply(1:100, function(x)rbind(sample(0L:99L, 50, replace=TRUE), 1L))
docs <- c(docs, lapply(1:100, function(x)rbind(sample(50L:149L, 50, replace=TRUE), 1L)))

ldaresult <- LDAgen(documents=docs, K = 5L, vocab=as.character(0:149), num.iterations = 20L, burnin = 70L, seed=24602, num.words = 10L, LDA = TRUE, count=TRUE)

expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=10, by.score=TRUE),4), round(c(-50.83504, -38.68486, -40.28166, -38.29243, -54.07439),4))
expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=5, by.score=TRUE),4), round(c(-10.540172,  -7.743481,  -8.714336,  -8.168923, -12.150991),4))
expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=5, by.score=FALSE),4), round(c(-9.627170,  -7.888853,  -9.392538,  -8.579004, -10.401106),4))
expect_equal(round(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=5, by.score=TRUE, sym.coherence = TRUE),4), round(c(-9.571439,  -7.671703,  -7.471123,  -8.402583, -11.201438),4))
expect_error(topicCoherence(ldaresult=ldaresult, documents=docs[[-1]], num.words=5, by.score=FALSE))
expect_error(topicCoherence(ldaresult=ldaresult, documents=docs, num.words="5", by.score=FALSE))
expect_warning(topicCoherence(ldaresult=ldaresult, documents=docs, num.words=150, by.score=FALSE))

})
