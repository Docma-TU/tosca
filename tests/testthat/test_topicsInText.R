context("topicsInText")

test_that("topicsInText", {

text <- list("ID1"=rbind(c(0,4,1,3), rep(1,4)), "ID2"=rbind(c(1,4,3,2,4,1,5), rep(1,7)))
ldaID <- c("ID1", "ID2")
vocab <- c("das", "frisst", "gurkensalat", "keinen", "pferd", "dortmund", "schalke", "mensa", "statistik", "wandern", "textmining")
ldaresult <- list(assignments= list(c(0,1,2,2), c(2,2,0,2,0,2,2)))
ldaresult$topics <- split(unlist(lapply(text, function(x)x[1,])), unlist(ldaresult$assignments))
ldaresult$topics <- sapply(ldaresult$topics, factor, levels=1:length(vocab))
ldaresult$topics <- t(sapply(ldaresult$topics, table))
colnames(ldaresult$topics) <- vocab
meta <- data.frame(id=c("ID1", "ID2", "ID3", "ID4"), date=as.Date(c("2016-01-01","2000-03-24","2017-04-23","1999-12-24")), title=c("title 1", "title 2", "title 3", "title 4"), page=2:5, category=c("A","B","A","B"))
originaltext <- list(ID1=c("Das Pferd frisst keinen Gurkensalat."), ID2=c("Das Pferd frisst keinen Gurkensalat! Das Pferd frisst 200 Aepfel in Dortmund."))

T1 <- topicsInText(text=text, ldaID=ldaID, id="ID1", ldaresult=ldaresult, label=NULL, vocab=vocab, wordOrder=c("both", "alphabetical", "topics", ""), colors=NULL, fixColors=FALSE, meta=NULL, originaltext=NULL, unclearTopicAssignment=TRUE, htmlreturn=TRUE)

T2 <- topicsInText(text=text, ldaID=ldaID, id="ID2", ldaresult=ldaresult, label=NULL, vocab=vocab, wordOrder=c("both", "alphabetical", "topics", ""), colors=NULL, fixColors=FALSE, meta=NULL, originaltext=originaltext, unclearTopicAssignment=TRUE, htmlreturn=TRUE)

T3 <- topicsInText(text=text, ldaID=ldaID, id="ID1", ldaresult=ldaresult, label=c("T1", "T2", "T3"), vocab=vocab, wordOrder=c("both", "alphabetical", "topics", ""), colors=NULL, fixColors=FALSE, meta=NULL, originaltext=NULL, unclearTopicAssignment=TRUE, htmlreturn=TRUE)

T4 <- topicsInText(text=text, ldaID=ldaID, id="ID1", ldaresult=ldaresult, label=c("T1", "T2", "T3"), vocab=vocab, wordOrder="alphabetical", colors=NULL, fixColors=FALSE, meta=NULL, originaltext=NULL, unclearTopicAssignment=TRUE, htmlreturn=TRUE)

T5 <- topicsInText(text=text, ldaID=ldaID, id="ID1", ldaresult=ldaresult, label=c("T1", "T2", "T3"), vocab=vocab, wordOrder="topics", colors=NULL, fixColors=FALSE, meta=NULL, originaltext=NULL, unclearTopicAssignment=FALSE, htmlreturn=TRUE)

T6 <- topicsInText(text=text, ldaID=ldaID, id="ID1", ldaresult=ldaresult, label=c("T1", "T2", "T3"), vocab=vocab, wordOrder="", colors="red", fixColors=FALSE, meta=meta, originaltext=originaltext, unclearTopicAssignment=TRUE, htmlreturn=TRUE)

T7 <- topicsInText(text=text, ldaID=ldaID, id="ID1", ldaresult=ldaresult, label=c("T1", "T2", "T3"), vocab=vocab, wordOrder="", colors=c("red", "blue", "green"), fixColors=FALSE, meta=meta, originaltext=originaltext, unclearTopicAssignment=TRUE, htmlreturn=TRUE)

T8 <- topicsInText(text=text, colors=c(2), ldaID=ldaID, id="ID1", ldaresult=ldaresult, label=c("T1", "T2", "T3"), vocab=vocab, wordOrder="", fixColors=TRUE, meta=meta, originaltext=originaltext, unclearTopicAssignment=TRUE, htmlreturn=TRUE)
## write.csv(c(T1, T2, T3, T4, T5, T6, T7, T8), file="data/topicsInText.csv", fileEncoding="UTF-8")

Tall <- read.csv("data/topicsInText.csv", stringsAsFactor=FALSE, encoding="UTF-8")

expect_equal(Tall[1:16,2], T1)
expect_equal(Tall[17:50,2], T2)
expect_equal(Tall[51:66,2], T3)
expect_equal(Tall[67:77,2], T4)
expect_equal(Tall[78:88,2], T5)
expect_equal(Tall[89:107,2], unname(T6))
expect_equal(Tall[108:126,2], unname(T7))
expect_equal(Tall[127:145,2], unname(T8))
})
