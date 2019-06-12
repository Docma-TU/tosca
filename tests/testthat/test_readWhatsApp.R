context("read WhatsApp Chats")

test_that("readWhatsApp", {
  corp = readWhatsApp(path = file.path("data", "WhatsApp"))
  corp1 = readWhatsApp(path = file.path("data", "WhatsApp"), file = "WhatsApp1.html")
  corp2 = readWhatsApp(path = file.path("data", "WhatsApp"), file = "WhatsApp2.html")
  
  expect_error(readWhatsApp(path = file.path("data", "WhatsApp"), file = "WhatsApp3.html"))
  expect_equal(
    readWhatsApp(path = file.path("data", "WhatsApp", "WhatsApp1.html")),
    readWhatsApp(file = file.path("data", "WhatsApp", "WhatsApp1.html")))
  expect_equal(corp, readWhatsApp())
  
  expect_true(is.textmeta(corp))
  expect_true(is.textmeta(corp1))
  expect_true(is.textmeta(corp2))
  expect_equal(mergeTextmeta(list(corp1, corp2)), corp)
  
  expect_equal(length(corp$text), nrow(corp$meta))
  expect_equal(length(corp$text), 6)
  expect_equal(length(corp1$text), nrow(corp1$meta))
  expect_equal(length(corp1$text), 2)
  expect_equal(length(corp2$text), nrow(corp2$meta))
  expect_equal(length(corp2$text), 4)
})
