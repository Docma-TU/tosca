context("makeWordlist")

test_that("makeWordlist", {

text <- list(c(
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

wt <- c(ad=1,     adipisici=2,        aliqua=2,       aliquid=1,          amet=2,
         aute=1,        cillum=1,       commodi=1,   consectetur=2,     consequat=1,
        dolor=2,        dolore=3,            ea=1,       eiusmod=2,          elit=2,
         enim=1,          esse=1,            et=2,            eu=1,            ex=1,
 exercitation=1,        fugiat=1,      incidunt=2,         ipsum=2,          iure=1,
       labore=2,       laboris=1,         lorem=2,         magna=2,         minim=1,
         nisi=1,       nostrud=1,         nulla=1,      pariatur=1,          quis=2,
reprehenderit=1,           sed=2,           sit=2,        tempor=2,       ullamco=1,
           ut=4,         velit=1,        veniam=1,     voluptate=1)

expect_equal(makeWordlist(text), list(words=sort(unique(unlist(text))), wordtable=wt))
})
