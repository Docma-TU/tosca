context("transform a corpus object to textmeta")

test_that("as.textmeta.corpus", {
  texts <- c("Give a Man a Fish, and You Feed Him for a Day.
 Teach a Man To Fish, and You Feed Him for a Lifetime",
    "So Long, and Thanks for All the Fish",
    "A very able manipulative mathematician, Fisher enjoys a real mastery
    in evaluating complicated multiple integrals.")
  
  corp <- quanteda::corpus(x = texts)
  obj <- as.textmeta.corpus(corp, addMetadata = FALSE)
  expect_true(is.textmeta(obj))
  
  quanteda::docvars(corp, "title") <- c("Fishing", "Don't panic!", "Sir Ronald")
  quanteda::docvars(corp, "date") <- c("1885-01-02", "1979-03-04", "1951-05-06")
  quanteda::docvars(corp, "id") <- c("A", "B", "C")
  quanteda::docvars(corp, "additionalVariable") <- 1:3
  
  obj <- as.textmeta.corpus(corp)
  expect_true(is.textmeta(obj))
})
