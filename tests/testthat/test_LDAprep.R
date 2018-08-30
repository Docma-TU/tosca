context("LDAprep prepare data for LDA")

test_that("LDAprep", {

  text <- list(A=c("lorem","ipsum","dolor"),
    B=c("ut","ut","ut","enim","ad","minim"),
    C=c("lorem","ipsum","dolor","dolor","dolor","dolor","sit"))

  A <- matrix(c(1,1,3,1,4,1),2,3)
  B1 <- matrix(c(0,1,2,1,5,1,7,3),2,4)
  B2 <- matrix(c(0,1,2,1,5,1,7,1,7,1,7,1),2,6)
  C1 <- matrix(c(1,4,3,1,4,1,6,1),2,4)
  C2 <- matrix(c(1,1,1,1,1,1,1,1,3,1,4,1,6,1),2,7)

  expect_equal(LDAprep(text=c(D="",text, E=NULL), vocab=sort(unique(unlist(text)))),list(A=A, B=B2,C=C2))
})
