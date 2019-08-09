context("showTopicsInText")

test_that("showTopicsInText", {
    texts <- list(
        A = "Give a Man a Fish, and You Feed Him for a Day.
      Teach a Man To Fish, and You Feed Him for a Lifetime",
        B = "So Long, and Thanks for All the Fish",
        C = "A very able manipulative mathematician, Fisher enjoys a real mastery
      in evaluating complicated multiple integrals.")
    
    corpus <- textmeta(
        meta = data.frame(
            id = c("A", "B", "C", "D"),
            title = c("Fishing", "Don't panic!", "Sir Ronald", "Berlin"),
            date = c("1885-01-02", "1979-03-04", "1951-05-06", "1967-06-02"),
            additionalVariable = 1:4,
            stringsAsFactors = FALSE),
        text = texts)
    
    raw <- corpus
    
    corpus <- cleanTexts(corpus)
    wordlist <- makeWordlist(corpus$text)
    ldaPrep <- LDAprep(text=corpus$text, vocab=wordlist$words)
    
    LDA <- LDAgen(documents = ldaPrep, K = 3L, vocab = wordlist$words, num.words = 3)
    
    # default:
    res <- showTopicsInText(obj = raw, ldaresult = LDA, documents = ldaPrep)
    
    expect_true(is.list(res))
    expect_true(all(sapply(res, is.data.frame)))
    expect_equal(length(res), 3)
    expect_equal(names(res), names(texts))
    expect_true(all(sapply(res, function(x) colnames(x) == c("TEXT", "WORD", "TOPIC"))))
    expect_true(all(sapply(res, function(x){
      all(is.character(x$TEXT), is.character(x$WORD), is.character(x$TOPIC))
    })))
    
    # id:
    id = "B"
    res2 <- showTopicsInText(obj = raw, ldaresult = LDA, documents = ldaPrep, id = id)
    expect_true(is.list(res2))
    expect_true(all(sapply(res2, is.data.frame)))
    expect_equal(length(res2), 1)
    expect_equal(names(res2), id)
    expect_equal(res$B, res2$B)
    expect_true(all(sapply(res2, function(x) colnames(x) == c("TEXT", "WORD", "TOPIC"))))
    expect_true(all(sapply(res2, function(x){
      all(is.character(x$TEXT), is.character(x$WORD), is.character(x$TOPIC))
    })))
    
    # expect errors:
    expect_error(showTopicsInText(ldaresult = LDA, documents = ldaPrep[[-1]]))
    expect_error(showTopicsInText(ldaresult = LDA, documents = ldaPrep, id = "Z"))
    expect_error(showTopicsInText(ldaresult = LDA))
    expect_error(showTopicsInText(documents = ldaPrep))
    expect_error(showTopicsInText(ldaresult = 1, documents = ldaPrep))
    expect_error(showTopicsInText(ldaresult = LDA, documents = ldaPrep, vocab = "ABCDE"))
    expect_error(showTopicsInText(ldaresult = LDA, documents = ldaPrep, tnames = c("Z", "K")))
    
    expect_error(showTopicsInText(obj = raw, ldaresult = LDA, documents = ldaPrep[[-1]]))
    expect_error(showTopicsInText(obj = raw, ldaresult = LDA, documents = ldaPrep, id = "Z"))
    expect_error(showTopicsInText(obj = raw, ldaresult = LDA))
    expect_error(showTopicsInText(obj = raw, documents = ldaPrep))
    expect_error(showTopicsInText(obj = raw, ldaresult = 1, documents = ldaPrep))
    expect_error(showTopicsInText(obj = raw, ldaresult = LDA, documents = ldaPrep, vocab = "ABCDE"))
    expect_error(showTopicsInText(obj = raw, ldaresult = LDA, documents = ldaPrep, tnames = c("Z", "K")))
    
    expect_error(showTopicsInText(obj = raw[1], ldaresult = LDA, documents = ldaPrep))
    
})
