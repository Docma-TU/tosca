context("textmeta")

test_that("textmeta", {

    text <- list("Lorem1 ipsum2 dolor3 sit4 amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. ", "  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. ", "")

    meta <- data.frame(id=as.character(1:3), date=as.Date(c(NA,"1987-06-25","2014-08-06")), title=c("Title 1", "Title 2", "Title 3"), page=c(24,60,1), stringsAsFactors=FALSE)

    metamult <- list(metamult1=c("1"= "Apfel", "1"= "Apple", "1"= "Eple", "2"="Norsk", "2"="Engelsk", "2"="Tysk"), metamult2=c("1"=1, "1"=22, "2"=0, "3"=4, "3"=0))

    ## tm1 <- textmeta(meta = NULL, text = NULL, metamult = NULL)
    ## tm2 <- textmeta(meta = meta, text = text, metamult = metamult)
    ## save(tm1,tm2, file="data/textmeta.RData")

    load("data/textmeta.RData")

    expect_equal(tm1, textmeta(meta = NULL, text = NULL, metamult = NULL))
    expect_equal(tm2, textmeta(meta = meta, text = text, metamult = metamult))
    expect_equal(print.textmeta(tm1), tm1)
    expect_equal(print.textmeta(tm2), tm2)
    expect_equal(print(tm1), print.textmeta(tm1))
    expect_equal(print(tm2), print.textmeta(tm2))
    expect_equal(summary(tm1), summary.textmeta(tm1))
    expect_equal(summary(tm2), summary.textmeta(tm2))
    expect_equal(summary(tm1), tm1)
    expect_equal(summary(tm2), tm2)
    expect_error(textmeta(meta=data.frame(id="a", date="2000-01-01", title=5)))
    meta2 <- meta
    meta2$date <- as.Date(c("1987-05-25", "1987-06-25", "1987-07-25"))
    expect_equal(plot.textmeta(textmeta(meta = meta2, text = text), unit="month"), data.frame(date=as.Date(c("1987-05-01", "1987-06-01", "1987-07-01")), counts=1))
    expect_equal(summary(tm2, metavariables="title"), tm2)

})

