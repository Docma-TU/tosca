context("clusterTopics")

test_that("clusterTopics", {

load("data/test-k3i20b70s24601alpha0.33eta0.33.RData")

## cT <- clusterTopics(ldaresult=result, method = "average", width=30, height=15)
## cT2 <- clusterTopics(ldaresult=result, method = "single", width=30, height=15)
## save(cT,cT2,file="data/clusterTopics.RData")

load("data/clusterTopics.RData")

expect_equal(cT, clusterTopics(ldaresult=result, file=file.path(tempdir(),"abc.pdf"), method = "average", width=30, height=15))
expect_equal(cT2, clusterTopics(ldaresult=result, file=file.path(tempdir(),"abc.pdf"), method = "single", width=30, height=15))
})
