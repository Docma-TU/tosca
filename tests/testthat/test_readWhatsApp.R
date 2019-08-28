context("read WhatsApp Chats")

test_that("readWhatsApp", {
  corp = readWhatsApp(path = file.path("data", "WhatsApp"))
  corp1 = readWhatsApp(path = file.path("data", "WhatsApp"), file = "WhatsApp1.html")
  corp2 = readWhatsApp(path = file.path("data", "WhatsApp"), file = "WhatsApp2.html")
  corp3 = readWhatsApp(path = file.path("data", "WhatsApp"), file = "WhatsApp3.html")
  
  expect_error(readWhatsApp(path = file.path("data", "WhatsApp"), file = "WhatsApp4.html"))
  expect_equal(
    readWhatsApp(path = file.path("data", "WhatsApp", "WhatsApp1.html")),
    readWhatsApp(file = file.path("data", "WhatsApp", "WhatsApp1.html")))
  expect_equal(corp, readWhatsApp())
  
  expect_true(is.textmeta(corp))
  expect_true(is.textmeta(corp1))
  expect_true(is.textmeta(corp2))
  expect_equal(mergeTextmeta(list(corp1, corp2, corp3)), corp)
  
  expect_equal(length(corp$text), nrow(corp$meta))
  expect_equal(length(corp$text), 12)
  expect_equal(length(corp1$text), nrow(corp1$meta))
  expect_equal(length(corp1$text), 4)
  expect_equal(length(corp2$text), nrow(corp2$meta))
  expect_equal(length(corp2$text), 4)
  expect_equal(length(corp2$text), nrow(corp3$meta))
  expect_equal(length(corp2$text), 4)
  
  ## WAmining
  corpdate = datemining.WA(corp)
  expect_true(corpdate$meta$date[3] == "2018-11-29")
  corpdate$meta$date[3] = NA_character_
  expect_equal(corp, corpdate)
  
  expect_warning(corpauthor <- authormining.WA(corp))
  expect_true(corpauthor$meta$author[3] == "Ressource")
  expect_true(all(corpauthor$meta$author[10:12] == "WhatsApp3"))
  corpauthor$meta$author[c(3, 10:12)] = NA_character_
  expect_equal(corp, corpauthor)
})
