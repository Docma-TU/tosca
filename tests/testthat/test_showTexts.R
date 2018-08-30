context("showTexts")

test_that("showTexts", {

    meta <- data.frame(id=as.character(1:3), date=as.Date(c("1960-01-01","1987-06-25","2014-08-06")), title=c("Title 1", "Title 2", "Title 3"), page=c(24,60,1), stringsAsFactors=FALSE)
    text <- list("1"="Text 1", "2"="Text 2", "3"="Text 3")
    object <- textmeta(meta=meta, text=text)
    M <- cbind(meta[,c(1,2,3)],text=unlist(text))
    M <- data.frame(apply(M,2,as.character), stringsAsFactors = FALSE)
    M2 <- M[c(1,3,2),]
    M3 <- M[3:1,]
    rownames(M) <- rownames(M2) <- rownames(M3)<- 1:3
    m <- showTexts(object,id=as.character(1:3))
    expect_equal(M,m)
    m <- showTexts(object,id=matrix(as.character(c(1:3,1,3,2,3:1)),3,3))
    expect_equal(list("1"=M, "2"=M2, "3"=M3),m)
})
