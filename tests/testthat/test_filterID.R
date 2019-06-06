context("filterDate")

test_that("filterDate", {
    text <- as.list(paste("text", 1:200))
    names(text) <- paste("id", 101:300)

    meta <- data.frame(
      id = paste("id", 1:400),
      date = as.Date(1:400, origin="1990-10-03"),
      title = as.character(NA),
      stringsAsFactors = FALSE)
    
    expect_equal(text, filterID(text, meta$id))
    expect_equal(2, length(filterID(text, paste("id", 101:102))))
    expect_equal(0, length(filterID(text, "id")))
    expect_error(length(filterID(text, 15)))
    
    expect_equal(filterID(text, c(meta$id, "QUATSCH")), filterID(text, meta$id))
    
    tm <- textmeta(text = text, meta = meta)
    
    expect_equal(tm, filterID(tm, meta$id, filtermeta = FALSE))
    
    expect_equal(
      filterID(tm, c(meta$id, "QUATSCH"), filtermeta = FALSE),
      filterID(tm, meta$id, filtermeta = FALSE))
    
    expect_equal(
      filterID(tm, c(meta$id, "QUATSCH")),
      filterID(tm, meta$id))
    
    z <- filterID(tm, meta$id)
    expect_equal(text, z$text)
    expect_equal(names(text), z$meta$id)
    
    
})
