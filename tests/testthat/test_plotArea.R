context("plotArea")

test_that("plotArea", {
  suppressWarnings(RNGversion("3.5.0"))
    set.seed(24601)
    ldaresult <- list(document_sums=matrix(sample(c(rep(0,20),1:20),10000, replace=TRUE),10,1000))
    ldaid <- paste("ID", 11:1010)
    meta1 <- data.frame(id=paste("ID", 1:1020), date=as.Date(sample(1:730, 1020, replace=TRUE), origin="1990-10-03"))
    ldaresult2 <- list(document_sums=matrix(c(60,50,50,50,200,1,100,1,20,80,45,55),2,6))
    ldaid2 <- paste("ID", 1:6)
    meta2 <- data.frame(id=paste("ID", 1:6), date=as.Date(c(1,32,64,96,128,160), origin="1990-10-03"))

    ## sP1 <- plotArea(ldaresult=ldaresult, ldaID=ldaid, select=NULL, tnames=NULL, threshold=NULL, meta=meta1, unit="month", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.08)
    ## sP2 <- plotArea(ldaresult=ldaresult, ldaID=ldaid, select=c(2,4,7), tnames=NULL, threshold=NULL, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.2)
    ## sP3 <- plotArea(ldaresult=ldaresult, ldaID=ldaid, select=NULL, tnames=NULL, threshold=0.14, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.1)
    ## sP4 <- plotArea(ldaresult=ldaresult2, ldaID=ldaid2, select=NULL, tnames=NULL, threshold=NULL, meta=meta2, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.08)
    ## save(sP1, sP2, sP3, sP4, file="data/plotArea.RData")


    load("data/plotArea.RData")

    expect_equal(sP1, plotArea(ldaresult=ldaresult, ldaID=ldaid, select=NULL, tnames=NULL, threshold=NULL, meta=meta1, unit="month", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.08))
    expect_equal(sP2, plotArea(ldaresult=ldaresult, ldaID=ldaid, select=c(2,4,7), tnames=NULL, threshold=NULL, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.2))
    expect_equal(sP2, plotArea(ldaresult=ldaresult, ldaID=ldaid, select=c("T2", "T4", "T7"), tnames=paste0("T",as.character(1:10)), threshold=NULL, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0))
    expect_equal(sP3, plotArea(ldaresult=ldaresult, ldaID=ldaid, select=NULL, tnames=NULL, threshold=0.14, meta=meta1, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0))
    expect_equal(sP4, plotArea(ldaresult=ldaresult2, ldaID=ldaid2, select=NULL, tnames=NULL, threshold=NULL, meta=meta2, unit="month", xunit="month", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.08))
    expect_error(plotArea(ldaresult=ldaresult, ldaID=paste("ID", 1011:2010), select=NULL, tnames=NULL, threshold=NULL, meta=meta1, unit="month", xunit="year", color=NULL, sort=TRUE, legend="topleft", legendLimit=0, peak=0.2))

    })

