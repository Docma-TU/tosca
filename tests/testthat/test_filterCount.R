context("filterCount")

test_that("filterCount", {

  counts <- c(sample(0:5, 48, replace = TRUE), 55, 104, 566)
  bounds <- replicate(51, paste(sample(c(".", " ", ";", "/", "-", "(", ")",
    "[", "]", "{", "}", "?", "!", "&", ":"), sample(1:4, 1)), collapse = ""))
  text <- as.list(mapply(function(x, y) paste(rep("a", x), collapse = y), counts, bounds))
  expect_equal(filterCount(text = text, out = "count"), counts)

  counts <- c(sample(0:5, 48, replace = TRUE), 55, 104, 566)
  bounds <- replicate(51, paste(sample(c(".", " ", ";", "/", "-", "(", ")",
    "[", "]", "{", "}", "?", "!", "&", ":"), sample(1:4, 1)), collapse = ""))
  text <- as.list(mapply(function(x, y) paste(rep("a", x), collapse = y), counts, bounds))
  expect_equal(filterCount(object = textmeta(text = text), out = "count"), counts)
  expect_true(is.textmeta(filterCount(textmeta(text = text))))
  expect_error(filterCount(object = text))

  counts <- c(sample(0:5, 48, replace = TRUE), 55, 104, 566)
  bounds <- replicate(51, paste(sample(c(".", " ", ";", "/", "-", "(", ")",
    "[", "]", "{", "}", "?", "!", "&", ":"), sample(1:4, 1)), collapse = ""))
  text <- as.list(mapply(function(x, y) paste(rep("a", x), collapse = y), counts, bounds))
  count <- sample(1:12, 1)
  expect_equal(filterCount(text = text, count = count, out = "bin"), counts >= count)

  counts <- c(sample(0:5, 48, replace = TRUE), 55, 104, 566)
  bounds <- replicate(51, paste(sample(c(".", " ", ";", "/", "-", "(", ")",
    "[", "]", "{", "}", "?", "!", "&", ":"), sample(1:4, 1)), collapse = ""))
  text <- as.list(mapply(function(x, y) paste(rep("a", x), collapse = y), counts, bounds))
  names(text) <- paste0("ID", 1:51)
  count <- sample(1:12, 1)
  expect_equal(names(filterCount(text = text, count = count)),
    names(text)[counts >= count])

})
