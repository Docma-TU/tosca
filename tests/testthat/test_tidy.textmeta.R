context("tidy text component of textmeta object")

test_that("tidy.textmeta", {
  texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
   Teach a Man To Fish, and You Feed Him for a Lifetime",
    B="So Long, and Thanks for All the Fish",
    C="A very able manipulative mathematician, Fisher enjoys a real mastery
   in evaluating complicated multiple integrals.")
  
  obj <- textmeta(meta=data.frame(id=c("A", "B", "C", "D"),
    title=c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
    date=c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
    additionalVariable=1:4, stringsAsFactors=FALSE), text=texts)
  
  tidy_text <- tidy.textmeta(obj)
  expect_true(is.textmeta_tidy(tidy_text))
  expect_equal(nrow(tidy_text$meta), 4)
  expect_equal(nrow(tidy_text$text), 3)
  
  obj <- cleanTexts(obj)
  tidy_text1 <- tidy.textmeta(obj)
  expect_true(is.textmeta_tidy(tidy_text1))
  expect_equal(nrow(tidy_text1$meta), 3)
  expect_equal(nrow(tidy_text1$text), 24)
  
  
  texts <- list(A="Give a Man a Fish, and You Feed Him for a Day.
   Teach a Man To Fish, and You Feed Him for a Lifetime",
    B="So Long, and Thanks for All the Fish",
    C="A very able manipulative mathematician, Fisher enjoys a real mastery
    in evaluating complicated multiple integrals.", D=NULL)
  obj <- textmeta(meta=data.frame(id=c("A", "B", "C", "D"),
    title=c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
    date=c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
    additionalVariable=1:4, stringsAsFactors=FALSE), text=texts)
  tidy_text3 <- tidy.textmeta(obj)
  expect_true(is.textmeta_tidy(tidy_text3))
  expect_equal(tidy_text1$meta, tidy_text3$meta)
  expect_equal(tidy_text$meta[1:3,], tidy_text3$meta)
  
  tidy_text2 <- print.textmeta_tidy(tidy_text3)
  expect_true(is.textmeta_tidy(tidy_text2))
})
