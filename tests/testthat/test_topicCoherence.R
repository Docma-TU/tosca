context("topicCoherence")

test_that("topicCoherence", {

docs <- lapply(1:100, function(x)rbind(sample(0L:99L, 50, replace=TRUE), 1L))
docs <- c(docs, lapply(1:100, function(x)rbind(sample(50L:149L, 50, replace=TRUE), 1L)))

ldaresult <- LDAgen(documents=docs, K = 5L, vocab=as.character(0:149), num.iterations = 20L, burnin = 70L, seed=24602, num.words = 10L, LDA = TRUE, count=TRUE)

topicCoherence(ldaresult=ldaresult, documents=docs, num.words=10, by.score=TRUE)

})

documents <- docs
num.words <- 10
by.score <- TRUE
  ttw <- lda::top.topic.words(ldaresult$topics, num.words = num.words, by.score = by.score)
  wordtopic <- mapply(function(x,y)rbind(x[1,]+1, y+1), documents, ldaresult$assignments)
  topicCoherence <- numeric(nrow(ldaresult$topics))
  
  for(i in 1:nrow(ldaresult$topics)){
    wordid <- match(ttw[,i], colnames(ldaresult$topics))
    
    D <- NULL
    for(j in 1:nrow(ttw)) D <- cbind(D, sapply(wordtopic, function(x)wordid[j] %in% x[1,]))
    Dsum <- colSums(D)
    
    grid <- cbind(rep(2:num.words, 1:(num.words-1)), unlist(sapply(1:(num.words-1), function(x)1:x)))
    
    tc <- apply(grid, 1, function(x) (sum(D[,x[1]]*D[,x[2]]) +1) / Dsum[x[2]] )
    topicCoherence[i] <- sum(log(tc))