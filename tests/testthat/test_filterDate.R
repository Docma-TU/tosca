context("filterDate")

test_that("filterDate", {
    text <- as.list(paste("text", 1:200))
    names(text) <- paste("id", 101:300)
    set.seed(24601)
    meta <- data.frame(
      id = paste("id", 1:400),
      date = as.Date(1:400, origin="1990-10-03"),
      title = as.character(NA),
      stringsAsFactors = FALSE)

    expect_equal(text, filterDate(text = text, meta = meta,
      s.date = min(meta$date), e.date = max(meta$date)))
    expect_equal(text[17:22], filterDate(text = text, meta = meta,
      s.date = as.Date("1991-01-28"),  e.date = as.Date("1991-02-02")))
    expect_equal(text, filterDate(text = text, meta = meta,
      s.date = as.Date("1960-01-28"),  e.date = as.Date("1999-02-02")))
    expect_true(is.textmeta(filterDate(textmeta(text = text, meta = meta))))
})
