context("showMeta")

test_that("showMeta", {

    meta <- data.frame(id=as.character(1:3), date=as.Date(c("1960-01-01","1987-06-25","2014-08-06")), title=c("Title 1", "Title 2", "Title 3"), page=c(24,60,1), stringsAsFactors=FALSE)

    expect_equal(showMeta(meta=meta, id = as.character(c(1,3)), cols = colnames(meta)), meta[c(1,3),])
    expect_equal(showMeta(meta=meta, cols = colnames(meta)), meta)
    idmatrix <- matrix(as.character(c(1,3,2,2)),ncol=2)
    expect_equal(showMeta(meta=meta, id = idmatrix, cols = colnames(meta)), list("1"=meta[c(1,3),], "2"=meta[2,]), file=tempdir())
    colnames(idmatrix) <- c("ID1", "ID2")
    expect_equal(showMeta(meta=meta, id = idmatrix, cols = colnames(meta)), list("ID1"=meta[c(1,3),], "ID2"=meta[2,]))
})
