context("cleanTexts: Remove ... tokenization")

test_that("cleanTexts", {

    text <- list("Lorem1 ipsum2 dolor3 sit4 amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. ",
                 "  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. ", "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed dass waehrend der die das eiusmod tempor incidunt ut labore et dolore magna aliqua. ", "", "")

    text2 <- list("Lorem1 ipsum2 dolor3 sit4 amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. ",
                 c("  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. ", "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed dass waehrend der die das eiusmod tempor incidunt ut labore et dolore magna aliqua. ", ""), "")

    mc1 <- list(c(
"lorem",      "ipsum",      "dolor",      "sit",        "amet",
"consectetur","adipisici",  "elit",       "sed",        "eiusmod",
"tempor",     "incidunt",   "ut",         "labore",     "et",
"dolore",     "magna",      "aliqua"),
c(
"ut",           "enim",         "ad",           "minim",
"veniam",       "quis",         "nostrud",      "exercitation",
"ullamco",      "laboris",      "nisi",         "ut",
"aliquid",      "ex",           "ea",           "commodi",
"consequat",    "quis",         "aute",         "iure",
"reprehenderit","voluptate",    "velit",        "esse",
"cillum",       "dolore",       "eu",           "fugiat",
"nulla",        "pariatur"),
c(
"lorem",      "ipsum",      "dolor",      "sit",        "amet",
"consectetur","adipisici",  "elit",       "sed",        "eiusmod",
"tempor",     "incidunt",   "ut",         "labore",     "et",
"dolore",     "magna",      "aliqua"))


mc2 <- list(list(c(
"lorem",      "ipsum",      "dolor",      "sit",        "amet",
"consectetur","adipisici",  "elit",       "sed",        "eiusmod",
"tempor",     "incidunt",   "ut",         "labore",     "et",
"dolore",     "magna",      "aliqua")),
list(c(
"ut",           "enim",         "ad",           "minim",
"veniam",       "quis",         "nostrud",      "exercitation",
"ullamco",      "laboris",      "nisi",         "ut",
"aliquid",      "ex",           "ea",           "commodi",
"consequat",    "quis",         "aute",         "iure",
"reprehenderit","voluptate",    "velit",        "esse",
"cillum",       "dolore",       "eu",           "fugiat",
"nulla",        "pariatur"),
c(
"lorem",      "ipsum",      "dolor",      "sit",        "amet",
"consectetur","adipisici",  "elit",       "sed",        "eiusmod",
"tempor",     "incidunt",   "ut",         "labore",     "et",
"dolore",     "magna",      "aliqua")))

expect_equal(cleanTexts(text = text, sw = "de", paragraph=FALSE), mc1)
expect_equal(cleanTexts(text = text2, sw = "de", paragraph=TRUE), mc2)

names(text2) <- LETTERS[1:3]
names(mc2) <- LETTERS[1:2]
tm <- textmeta(meta = data.frame(id = LETTERS[1:3], title = as.character(NA),
  date = as.Date(NA), stringsAsFactors = FALSE), text = text2)
cleared <- cleanTexts(object = tm, sw = "de", paragraph = TRUE)
expect_true(is.textmeta(cleared))
expect_equal(cleared$text, mc2)
})
