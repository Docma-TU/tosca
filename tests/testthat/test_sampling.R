context("sampling")

test_that("sampling", {

  suppressWarnings(RNGversion("3.5.0"))
  set.seed(24601)

  id <- paste0("ID", 1:100)
  corporaID <- list(sample(id, 30), sample(id, 10), sample(id, 70))
  label <- sample(as.logical(0:1), 15, replace=TRUE)
  names(label) <- c(sample(id, 10), sample(corporaID[[2]], 5))
  m <- 10
  expect_equal(c("ID3", "ID40", "ID78", "ID6", "ID9", "ID97", "ID92", "ID23", "ID87", "ID55"), sampling(id, corporaID, label, m))
  expect_equal(c("ID62", "ID96", "ID75", "ID56", "ID53", "ID27", "ID92", "ID6", "ID66", "ID30"), sampling(id, corporaID, label, m, randomize = FALSE, exact = TRUE))
  expect_equal(c("ID23", "ID40", "ID29", "ID46", "ID80", "ID61", "ID12", "ID75", "ID17", "ID35"), sampling(id, corporaID, label, m, randomize = TRUE, exact = FALSE))
  expect_equal(c("ID77", "ID11", "ID61", "ID78", "ID9", "ID93", "ID29", "ID8", "ID66", "ID86"), sampling(id, corporaID, label, m, randomize = TRUE, exact = TRUE))
  expect_equal(c("ID66", "ID68", "ID7",  "ID92", "ID72", "ID15", "ID23", "ID30", "ID97", "ID61"), sampling(id, corporaID, m=m, randomize = TRUE, exact = TRUE))
  
  expect_warning(sampling(id, corporaID, label, m=100, randomize = FALSE, exact = FALSE))
  expect_message(sampling(id, corporaID, label, m=80, randomize = FALSE, exact = FALSE))
  expect_error(sampling(id, corporaID, label, m="80", randomize = FALSE, exact = FALSE))
  expect_error(sampling(id, corporaID, label, m="80", randomize = 5, exact = FALSE))
  expect_error(sampling(id, corporaID, label, m="80", randomize = 5, exact = "FALSE"))
  expect_error(sampling(id=paste0("ID", 1:10), corporaID, label, m="80", randomize = 5, exact = "FALSE"))
})
