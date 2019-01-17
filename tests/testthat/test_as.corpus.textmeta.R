context("transform a textmeta object to corpus")

test_that("as.corpus.textmeta", {
  texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
 Teach a Man To Fish, and You Feed Him for a Lifetime",
    B="So Long, and Thanks for All the Fish",
    C="A very able manipulative mathematician, Fisher enjoys a real mastery
    in evaluating complicated multiple integrals.")
  
  obj <- textmeta(meta=data.frame(id=c("A", "B", "C", "D"),
    title=c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
    date=c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
    additionalVariable=1:4, stringsAsFactors=FALSE), text=texts)
  
  corp <- as.corpus.textmeta(obj)
  expect_true(quanteda::is.corpus(corp))
})
