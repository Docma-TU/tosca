context("showTextTopic")

test_that("showTextTopic", {
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
    
    corpus <- cleanTexts(corpus)
    wordlist <- makeWordlist(corpus$text)
    ldaPrep <- LDAprep(text=corpus$text, vocab=wordlist$words)
    
    LDA <- LDAgen(documents = ldaPrep, K = 3L, vocab = wordlist$words, num.words = 3)
    
    # default:
    res <- showTextTopics(ldaresult = LDA, documents = ldaPrep)
    
    expect_true(is.list(res))
    expect_true(all(sapply(res, is.matrix)))
    expect_true(all(sapply(res, is.numeric)))
    
    expect_equal(length(res), 3)
    expect_equal(names(res), names(texts))
    expect_true(all(drop(LDA$topic_sums) == Reduce("+", lapply(res, function(x) x[-1,1]))))
    expect_true(all(lengths(corpus$text) == sapply(res, function(x) x[1,1])))
    
    # id:
    id = "B"
    res <- showTextTopics(ldaresult = LDA, documents = ldaPrep, id = id,
        tnames = LETTERS[4:6])
    expect_true(is.list(res))
    expect_true(all(sapply(res, is.matrix)))
    expect_true(all(sapply(res, is.numeric)))
    
    expect_equal(length(res), 1)
    expect_equal(names(res), "B")
    expect_true(all(drop(LDA$topic_sums) >= Reduce("+", lapply(res, function(x) x[-1,1]))))
    expect_true(all(lengths(corpus$text)[match(id, names(ldaPrep))] ==
            sapply(res, function(x) x[1,1])))
    
    # tnames:
    expect_equal(rownames(res[[1]])[-1], LETTERS[4:6])
    
    # vocab:
    res <- showTextTopics(ldaresult = LDA, documents = ldaPrep, vocab = "fish")
    expect_true(is.list(res))
    expect_true(all(sapply(res, is.matrix)))
    expect_true(all(sapply(res, is.numeric)))
    
    expect_true(all(res$C[1,] == c(1,1)))
    
    # expect errors:
    expect_error(showTextTopics(ldaresult = LDA, documents = ldaPrep[[-1]]))
    expect_error(showTextTopics(ldaresult = LDA, documents = ldaPrep, id = "Z"))
    expect_error(showTextTopics(ldaresult = LDA))
    expect_error(showTextTopics(documents = ldaPrep))
    expect_error(showTextTopics(ldaresult = 1, documents = ldaPrep))
    expect_error(showTextTopics(ldaresult = LDA, documents = ldaPrep, vocab = "ABCDE"))
    expect_error(showTextTopics(ldaresult = LDA, documents = ldaPrep, tnames = c("Z", "K")))
})
