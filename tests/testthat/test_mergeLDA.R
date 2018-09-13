context("mergeLDA")

test_that("mergeLDA", {

text <- list(A= "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quet dolore magna aliqua. Ut enim ad minim veniamet dolore magna aliqua. Ut enim ad minim veniamis nostrud exercitation ullamco laboris nisi ut aliquid ex ea commodi consequat. Quis aute iure reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
             B= "Lorem ipsum dolor sit amet, consecteturofficia deserunt mollit officia deserunt mollit  adipisici eet dolore magna aliqua. Ut enim ad minim veniamet dolore magna aliqua. Ut enim ad minim veniamlit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud",
             C="in voet dolore magna aliqua. Ut enim ad minim veniamluptate velit esse cillum dolore eu officia deserunt mollit officia deserunt mollit fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
             D="Lorem ipsum dolor siofficia deserunt mollit officia deserunt mollit t amet, consectetur adipisici elit, sed eiusmod temporet dolore magna aliqua. Ut enim ad minim veniam incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud",
             E="Lorem ipsum dolor sit amet,et dolore magna aliqua. Ut enim ad minim veniam consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. mollit anim id est laborum.",
             F= "sunt in culpa qui officia deserunt mollit anim id est laborum.",
             G= "Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minimet dolore magna aliqua. Ut enim ad minim veniamet dolore magna aliqua. Ut enim ad minim veniamet dolore magna aliqua. Ut enim ad minim veniam veniam, quis nostrud Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrudLorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud",
             H="Lorem ipsum dolor sit amet, consectetur adipisici elit, sed eiusmod tempor incidunt ut labore et dolore magna  cupiditat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.",
             I="iure reprehenderit iure reprehenderit in voluptate velit esse cillumofficia deserunt mollit officia deserunt mollit officia deserunt mollit  dolore eu fugiat nulla pariatur. Excepteur sint obcaecat cupiditat non proident, suntin culpa qui officiaiure reprehenderit  in culpa qui officiaiure reprehenderit  deserunt mollit anim id est laborum.")

text2 <- cleanTexts(text = text)
wordlist <- makeWordlist(text2)
LDAdoc <- LDAprep(text2, wordlist$words)
lda1 <- LDAgen(documents=LDAdoc, K = 3L, vocab=wordlist$words, num.iterations = 20L, burnin = 70L, seed=124601, num.words = 10L, LDA = TRUE)

text2 <- cleanTexts(text = text[c("B", "C", "F", "H")])
wordlist <- makeWordlist(text2)
LDAdoc <- LDAprep(text2, wordlist$words)
lda2 <- LDAgen(documents=LDAdoc, K = 3L, vocab=wordlist$words, num.iterations = 20L, burnin = 70L, seed=124601, num.words = 10L, LDA = TRUE)


## mL1 <- mergeLDA(x=list(lda1=lda1, lda2=lda2))
## mL2 <- mergeLDA(x=list(lda1, lda2))
## save(mL1, mL2, file="data/mergeLDA.RData")

load("data/mergeLDA.RData")
expect_equal(mL1, mergeLDA(x=list(lda1=lda1, lda2=lda2)))
expect_equal(mL2, mergeLDA(x=list(lda1, lda2)))
})
