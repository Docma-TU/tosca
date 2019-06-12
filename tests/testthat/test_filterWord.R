context("filterWord creates subcorpus based on keyword search")

test_that("filterWord", {
  text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aa", "aab", "bc"))
  wordlist1 <- c("aa", "bc")
  wordlist2 <- c("aa")

  expect_error(filterWord(text = counts, out = "text"))

  counts <- c(2,1)
  res <- filterWord(text = text, search=wordlist1, out = "bin")
  expect_equal(res, rep(TRUE, 4))

  res <- filterWord(text = text, search = list(data.frame(pattern = wordlist1,
    word = FALSE, count = counts, stringsAsFactors = FALSE)), out = "bin")
  expect_equal(res, c(TRUE, FALSE, FALSE, TRUE))

  res <- filterWord(text = text, search=wordlist1, out = "count")
  test <- matrix(c(2,0,1,2,1,1,1,1), nrow=4)
  colnames(test) <- c("aa","bc")
  expect_equal(res, test)

  res <- filterWord(text = text, search = wordlist2, out="count")
  test <- matrix(c(2,0,1,2), nrow=4)
  colnames(test) <- "aa"
  expect_equal(res, test)

  res <- filterWord(text = text, search = data.frame(pattern = wordlist1,
    count = counts, word = FALSE, stringsAsFactors = FALSE), out = "text")
  expect_equal(res, list("abaabcaa", c("aa", "aab", "bc")))

  res <- filterWord(text = text, search = list(data.frame(pattern = wordlist1[1],
    count = counts[1], word = FALSE, stringsAsFactors = FALSE),
    data.frame(pattern = wordlist1[2], count = counts[2], word = FALSE,
      stringsAsFactors = FALSE)), out = "text")
  expect_equal(res, text)

  res <- filterWord(text = text, search = data.frame(pattern = wordlist2, count = 2, word = FALSE), out = "text")
  expect_equal(res, list("abaabcaa", c("aa", "aab", "bc")))

  res <- filterWord(text = text, search = data.frame(pattern=wordlist2, count=3, word=FALSE), out = "text")
  expect_equal(res, list())

  ## ignore.case
  expect_equal(filterWord(text=list("a"), search="A", out="text"), list())
  expect_equal(filterWord(text="a", search="A", out="text"), list())
  expect_equal(filterWord(text="a", search="A", out="text", ignore.case=TRUE), list("a"))
  expect_equal(filterWord(text=list("a","A"), search="A", out="text", ignore.case=TRUE), list("a", "a"))
  expect_equal(filterWord(text=c("a","A"), search="a", out="text", ignore.case=TRUE), list("a", "a"))

  test <- matrix(c(1,1), nrow=2)
  colnames(test) <- c("a_case")
  test2 <- matrix(c(1,0), nrow=2)
  colnames(test2) <- c("a")
  expect_equal(filterWord(text=c("a","A"), search="a", out="count", ignore.case=TRUE), test)
  expect_equal(filterWord(text=c("a","A"), search="a", out="count", ignore.case=FALSE), test2)

  ## counts
  counts <- list(c(2,1), 3)
  text2 <- list("abc", c("a","a","b"), c("c","c","c"))
  res <- filterWord(text = text2, search=list(data.frame(pattern=c("a","b"), count=c(2,1), word=c(FALSE, FALSE)), data.frame(pattern="c", count=3, word=FALSE)), out="bin")
  expect_equal(res, c(FALSE, TRUE, TRUE))
  res2 <- filterWord(textmeta(text = text2), search=list(data.frame(pattern=c("a","b"), count=c(2,1), word=c(FALSE, FALSE)), data.frame(pattern="c", count=3, word=FALSE)), out="bin")
  expect_equal(res, res2)

  ## words
  text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aa", "aab", "bc"))
  search1 <- data.frame(pattern=c("aa", "bc"), word=c(FALSE, TRUE), count=1)
  expect_equal(filterWord(text=text, search=search1, out="bin"), c(FALSE, FALSE, FALSE, TRUE))

  text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aab", "bc"))
  search1 <- data.frame(pattern=c("aa", "bc"), word=c(TRUE, TRUE), count=1)
  expect_equal(filterWord(text=text, search=search1, out="bin"), c(FALSE, FALSE, FALSE, FALSE))

  text <- list("abaabcaa", "aab")
  search1 <- data.frame(pattern=c("aab"), word=c(TRUE), count=1)
  expect_equal(filterWord(text=text, search=search1, out="bin"), c(FALSE, TRUE))

  text <- list("abaabcaa", "ababc", c("aba", "aabc"), c("aa", "aab", "bc"))
  search1 <- data.frame(pattern=c("aab", "a", "bc", "ab"), word=c(TRUE, FALSE, TRUE, TRUE), count=1)
  tr <- matrix(c(0,0,0,1,5,2,4,4,0,0,0,1,0,0,0,0), nrow=4)
  colnames(tr) <- c("aab_w", "a", "bc_w", "ab_w")
  expect_equal(filterWord(text=text, search=search1, out="count"),tr)

  search1 <- data.frame(pattern=c("aab", "a", "bc", "ab"), word=c("word", "pattern", "left", "right"), count=1)
  tr <- matrix(c(0,0,0,1,5,2,4,4,0,1,1,1,1,1,1,0), nrow=4)
  colnames(tr) <- c("aab_w", "a", "bc_l", "ab_r")
  expect_equal(filterWord(text=text, search=search1, out="count"),tr)

  search1 <- data.frame(pattern=c("aab", "a", "bc", "ab"), word=c("word", "pattern", "pattern", "pattern"), count=1)
  tr <- matrix(c(0,0,0,1,5,2,4,4,1,1,1,1,2,2,2,1), nrow=4)
  colnames(tr) <- c("aab_w", "a", "bc", "ab")
  expect_equal(filterWord(text=text, search=search1, out="count"),tr)

  names(text) <- LETTERS[1:4]
  meta <- data.frame(id = LETTERS[1:10], date = as.Date(NA),
    title = as.character(NA), stringsAsFactors = FALSE)
  meta <- meta[sample(1:10), ]
  tm = textmeta(meta = meta, text = text)

  comp <- filterWord(object = tm, search = search1)
  expect_equal(length(comp$text), nrow(comp$meta))
  expect_true(is.textmeta(comp))
  expect_true(names(comp$text) == "D")
  expect_true(comp$meta$id == "D")

  text <- list("abaabcaa", NA, NULL, c("aa", "aab", "bc"))
  expect_equal(filterWord(text=text, search="a", out="text"),list("abaabcaa", c("aa", "aab", "bc")))

})
