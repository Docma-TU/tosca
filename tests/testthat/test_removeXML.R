context("remove XML tags and change umlauts style")

test_that("removeXML", {
#Sys.setlocale('LC_ALL','C')

text <- c("aba<vcs>ab</vcs>caa", "ab&dgv;abc", "&auml;&Auml;&ouml;&Ouml;&uuml;&Uuml;&szlig;", "aa", "aab", "bc")

tmp <- c("e4","c4","f6","d6","fc","dc","df")
tmp <- paste0(sapply(tmp, function(x)eval(parse(text = paste0("'\\u", x, "'")))), collapse = "")
text2 <- c("aba<vcs>ab</vcs>caa", "ab&dgv;abc", tmp, "aa", "aab", "bc")

expect_equal(removeXML(x=text2), c("aba ab caa","ab&dgv;abc","\UE4\UC4\UF6\UD6\UFC\UDC\UDF","aa","aab","bc"))
expect_equal(removeHTML(x=text, symbolList = 1, dec=FALSE, hex=FALSE, delete = FALSE), text2)

umlauts <- c("aba ab caa","ab&dgv;abc","\UE4\UC4\UF6\UD6\UFC\UDC\UDF","aa","aab","bc")
exp1 <- c("aba ab caa", "ab&dgv;abc", "aeAeoeOeueUess", "aa", "aab", "bc")
expect_equal(removeUmlauts(x=umlauts), exp1)


x <- c("&#x00f8;&#248;&oslash;")

expect_equal(removeHTML(x=x, symbolList = 1, dec=TRUE, hex=FALSE, entity=FALSE, delete = FALSE), "&#x00f8;\UF8&oslash;")
expect_equal(removeHTML(x=x, symbolList = 1, dec=FALSE, hex=TRUE, entity=FALSE, delete = TRUE), "\UF8")
expect_equal(removeHTML(x=x, symbolList = 1, dec=FALSE, hex=FALSE, entity=TRUE, delete = TRUE), "\UF8")

x <- c("&#x00f8;&#x0f8;&#xf8;&#248;&#0248;&#00248;")

expect_equal(removeHTML(x=x, symbolList = 1, dec=TRUE, hex=FALSE, entity=FALSE, delete = TRUE), "\UF8\UF8\UF8")
expect_equal(removeHTML(x=x, symbolList = 1, dec=FALSE, hex=TRUE, entity=FALSE, delete = TRUE), "\UF8\UF8\UF8")

ISOtest <- sort(unique(as.vector(ISO8859())))[-1]
ISOtest2 <- sapply(ISOtest, function(x)eval(parse(text = paste0("'\\u", x, "'"))))
ISOtestSymbols <- toupper(paste0(as.hexmode(c(32:64,91:96,123:126,160:191,215,247, 818, 8194:8222, 8254, 8291, 8364, 8417, 8470)))) # Vector of Symbols
ISOtestSymbols <- ISOtest %in% gsub(pattern="^0*", replacement="", ISOtestSymbols)
ISOtest2Symbols <- ISOtest2
ISOtest2Symbols[ISOtestSymbols] <- ""

ISOtestDec <- paste0("&#", strtoi(ISOtest, base=16L), ";")
ISOtestHex <- paste0("&#x", ISOtest, ";")
ISOtestEnt <- namedEntity()[match(ISOtest, namedEntity()[,2]),1]
EntNA <- is.na(ISOtestEnt)

expect_equal(removeHTML(x=ISOtestDec, symbolList = c(1:11,13:16), dec=TRUE, hex=FALSE, entity=FALSE, symbols=TRUE), unname(ISOtest2))
expect_equal(removeHTML(x=ISOtestHex, symbolList = c(1:11,13:16), dec=FALSE, hex=TRUE, entity=FALSE, symbols=TRUE), unname(ISOtest2))
expect_equal(removeHTML(x=ISOtestEnt, symbolList = c(1:11,13:16), dec=FALSE, hex=FALSE, entity=TRUE, symbols=TRUE)[!EntNA], unname(ISOtest2)[!EntNA])

expect_equal(removeHTML(x=ISOtestDec, symbolList = c(1:11,13:16), dec=TRUE, hex=FALSE, entity=FALSE), unname(ISOtest2Symbols))

## lists

testlist <- list(ID1=c("a", "b"), ID2=NULL, ID3=c("<gh>", "&auml; \UE4"))

expect_equal(removeXML(testlist), list(ID1=c("a", "b"), ID3=c("", "&auml; \UE4")))
expect_equal(removeHTML(testlist), list(ID1=c("a", "b"), ID3=c("<gh>", "\UE4 \UE4")))
expect_equal(removeUmlauts(testlist), list(ID1=c("a", "b"), ID3=c("<gh>", "&auml; ae")))

})


## text <- "&Auml;&Ouml;"
## Encoding(text)
## text <- gsub(pattern="&Auml;", replacement="\u00C4", x=text)
## Encoding(text)
## text <- gsub(pattern="&Ouml;", replacement="\u00D6", x=text)
## text
## text=="\u00C4\u00D6"

## Encoding(text) <- "UTF-8"
## text
## text=="\u00C4\u00D6"
## text <- gsub(pattern="&Auml;", replacement="\u00C4", x=text, useBytes=TRUE)
## text <- gsub(pattern="&Ouml;", replacement="\u00D6", x=text, useBytes=TRUE)


