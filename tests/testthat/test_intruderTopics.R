context("intruderWords")

test_that("intruderWords", {

    ## library(dirmult)
    ## set.seed(24601)
    ## beta <- rbind(rdirichlet(n=50, alpha=rep(0.6,200)))
    ## colnames(beta) <- paste("Wort", 1:200)

    ## text <- as.list(paste("This is text", 1:100, ". It's a great text."))
    ## names(text) <- paste("T", 1:100)
    ## theta <- rbind(matrix(sample(c(1:50, rep(0,50)),800, replace=TRUE),8,100),matrix(0,2,100))


    ## set.seed(24601)
    ## iT <- intruderTopics(text, beta, theta, id=paste("T", 1:100), numIntruder=1:2, numOuttopics=4, byScore=TRUE, minWords=1L, minOuttopics=1L, stopTopics=2, printSolution=FALSE, oldResult=NULL)
    ## h
    ## 1
    ## 5
    ## 2 1
    ## g
    ## q

    ## iTo <- intruderTopics(text, beta, theta, id=paste("T", 1:100), numIntruder=1:2, numOuttopics=4, byScore=TRUE, minWords=1L, minOuttopics=1L, stopTopics=2, printSolution=FALSE, oldResult=iT)
    ## 1
    ## 3
    ## q

    ## iTs <- intruderTopics(text, beta, theta, id=paste("T", 1:100), numIntruder=1:2, numOuttopics=4, byScore=FALSE, minWords=1L, minOuttopics=1L, stopTopics=2, printSolution=FALSE, oldResult=NULL)
    ## 1
    ## 2 1
    ## q



    ## save(beta, theta, text, iT, iTo, iTs, file="data/intruderTopics.RData", compress="bzip2")

    load("data/intruderTopics.RData")

    set.seed(24601)
    iT2 <- intruderTopics(text, beta, theta, id=paste("T", 1:100), numIntruder=1:2, numOuttopics=4, byScore=TRUE, minWords=1L, minOuttopics=1L, stopTopics=2, printSolution=FALSE, oldResult=NULL, test=TRUE, testinput=c("h", "1", "5", "2 1", "g", "q"))

    iTo2 <- intruderTopics(text, beta=matrix(1:4,2,2), theta=matrix(1:4,2,2), id=1:5, numIntruder=1:2, numOuttopics=99, byScore=FALSE, minWords=100L, minOuttopics=100L, stopTopics=200, printSolution=FALSE, oldResult=iT, test=TRUE, testinput=c("1", "3", "q"))

    iTs2 <- intruderTopics(text, beta, theta, id=paste("T", 1:100), numIntruder=1:2, numOuttopics=4, byScore=FALSE, minWords=1L, minOuttopics=1L, stopTopics=2, printSolution=FALSE, oldResult=NULL, test=TRUE, testinput=c("1", "2 1", "q"))


    expect_equal(iT, iT2)
    expect_equal(iTo, iTo2)
    expect_equal(iTs, iTs2)

    iTs2 <- intruderTopics(text, beta, theta, id=paste("T", 1:100), numIntruder=1:2, numOuttopics=4, byScore=FALSE, minWords=1L, minOuttopics=1L, stopTopics=2, printSolution=FALSE, oldResult=NULL, test=TRUE, testinput=c("1", "2 1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "q"))
    print(iTs2)
    summary(iTs2)
})
