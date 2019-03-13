context("intruderWords")

test_that("intruderWords", {

    ## library(dirmult)
    ## set.seed(24601)
    ## beta <- rbind(rdirichlet(n=10, alpha=rep(0.6,200)))
    ## colnames(beta) <- paste("Wort", 1:200)

    ## set.seed(24601)

    ## iW <- intruderWords(beta, byScore = TRUE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=NULL)
    ## 1 2 3
    ## 6
    ## h
    ## 1
    ## x
    ## q

    ## iWo <- intruderWords(beta, byScore = TRUE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=iW)
    ## 4 2
    ## 1
    ## 3
    ## 2
    ## 4
    ## 2 1

    ## iWs <- intruderWords(beta, byScore = FALSE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=NULL)
    ## L123
    ## 6
    ## h
    ## 1
    ## x
    ## q

    ## save(beta, iW, iWo, iWs, file="data/intruderWords.RData")

    load("data/intruderWords.RData")
  suppressWarnings(RNGversion("3.5.0"))
    set.seed(24601)

    iW2 <- intruderWords(beta, byScore = TRUE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=NULL, test=TRUE, testinput=c("1 2 3", "6", "h", "1", "x", "q"))

    iWo2 <- intruderWords(beta, byScore = TRUE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=iW, test=TRUE, testinput=c("4 2", "1", "3", "2", "4", "2 1"))

    iWs2 <- intruderWords(beta, byScore = FALSE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=NULL, test=TRUE, testinput=c("L123", "6", "h", "1", "x", "q"))

    expect_equal(iW, iW2)
    expect_equal(iWo, iWo2)
    expect_equal(iWs, iWs2)

    iWs3 <- intruderWords(beta, byScore = FALSE, numTopwords = 10L, numIntruder = 1L:2L, numOutwords = 8L, noTopic=TRUE, printSolution = FALSE, oldResult=NULL, test=TRUE, testinput=c("0", "0", "0", "1", "1", "1", "1", "1", "1", "1"))
    print(iWs3)
    summary(iWs3)
})
