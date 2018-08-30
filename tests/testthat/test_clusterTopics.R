context("clusterTopics")

test_that("clusterTopics", {

load("data/test-k3i20b70s24601alpha0.33eta0.33.RData")

## cT <- clusterTopics(topics=result$topics, file="test.pdf", method = "average", width=30, height=15)
## cT2 <- clusterTopics(topics=result$topics, file="test.pdf", method = "single", width=30, height=15)
## save(cT,cT2,file="data/clusterTopics.RData")

load("data/clusterTopics.RData")

expect_equal(cT, clusterTopics(ldaresult=result, file=paste0(tempdir(),"/abc.pdf"), method = "average", width=30, height=15))
expect_equal(cT2, clusterTopics(ldaresult=result, file=paste0(tempdir(),"/abc.pdf"), method = "single", width=30, height=15))
})
