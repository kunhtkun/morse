test_that("morse() encode a string to Morse code words", {
  expect_equal(morse("abc", sep = "/"), ".-/-.../-.-.")
  expect_equal(morse("Abc", sep = "/"), ".-/-.../-.-.")
  expect_equal(morse("=-?", sep = "/"), "-...-/-....-/..--..")
  expect_equal(
    morse("(abc,def?)", sep = "/"),
    "-.--./.-/-.../-.-./--..--/-.././..-./..--../-.--.-"
  )
})

test_that("morse() handles special cases sensibly", {
  expect_equal(morse("a^b", sep = "/"), ".-/^/-...")
  expect_error(morse(c("abc", "ddd")))
  expect_equal(morse(character()), character())
})

test_that("unmorse() decode strings encoded with morse()", {
  cases <- list("abc", "def", "a!#Abb")
  for (case in cases) {
    expect_equal(unmorse(morse(case)), tolower(case))
  }
})
