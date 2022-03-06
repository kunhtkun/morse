#
# General functions to work with codes.
#

#' Make a hash table for code.
#'
#' @param code Mapping from characters to code words. Each element is a vector
#'             of two strings.
#' @param from Index of the character in the element of \code{code}.
#' @param to Index of the code word in the element of \code{code}.
#'
#' @return The hash table for code.
#'
#' @examples
#' morse:::make_code_table(list(c("a", "1"), c("b", "2")))
#' morse:::make_code_table(list(c("a", "1"), c("b", "2")), from = 2, to = 1)
make_code_table <- function(code, from = 1, to = 2) {
  stopifnot(is.list(code))
  table <- new.env(
    hash = TRUE,
    parent = emptyenv(),
    size = length(code)
  )
  for (entry in code) {
    table[[entry[from]]] <- entry[to]
  }
  table
}

#' Make an encode function.
#'
#' @param table Hash table for looking up code words.
#' @param pre A function to pre-process the input.
#'
#' @return A function that takes a string and return the encoded string.
#' @export
#'
#' @examples
#' table <- morse:::make_code_table(list(c("a", "1"), c("b", "2")))
#' encode <- make_encode(table)
#' encode("ab")
make_encode <- function(table, pre = identity) {
  lookup <- function(char) {
    word <- table[[char]]
    ifelse(is.null(word), char, word)
  }

  function(string, sep = "/") {
    stopifnot(is.character(string), length(string) <= 1)
    string <- pre(string)
    if (length(string) < 1) {
      character()
    } else {
      chars <- strsplit(string, split = "")[[1]]
      words <- sapply(chars, lookup)
      paste0(words, collapse = sep)
    }
  }
}

#' Make a decode function.
#'
#' @param table Hash table for looking up characters for code words.
#' @param pre A function to pre-process the input.
#'
#' @return A function that takes an encoded string and return the original string.
#' @export
#'
#' @examples
#' table <- morse:::make_code_table(list(c("a", "1"), c("b", "2")), from = 2, to = 1)
#' decode <- make_decode(table)
#' decode("1/2", sep = "/")
make_decode <- function(table, pre = identity) {
  lookup <- function(word) {
    char <- table[[word]]
    ifelse(is.null(char), word, char)
  }

  function(string, sep = "/") {
    stopifnot(is.character(string), length(string) <= 1)
    if (length(string) < 1) {
      character()
    } else {
      words <- strsplit(string, split = sep)[[1]]
      chars <- sapply(words, lookup)
      paste0(chars, collapse = "")
    }
  }
}

#
# Morse code
#

morse_code <- list(
  c("a", ".-"),
  c("b", "-..."),
  c("c", "-.-."),
  c("d", "-.."),
  c("e", "."),
  c("f", "..-."),
  c("g", "--."),
  c("h", "...."),
  c("i", ".."),
  c("j", ".---"),
  c("k", "-.-"),
  c("l", ".-.."),
  c("m", "--"),
  c("n", "-."),
  c("o", "---"),
  c("p", ".--."),
  c("q", "--.-"),
  c("r", ".-."),
  c("s", "..."),
  c("t", "-"),
  c("u", "..-"),
  c("v", "...-"),
  c("w", ".--"),
  c("x", "-..-"),
  c("y", "-.--"),
  c("z", "--.."),
  c("0", "-----"),
  c("1", ".----"),
  c("2", "..---"),
  c("3", "...--"),
  c("4", "....-"),
  c("5", "....."),
  c("6", "-...."),
  c("7", "--..."),
  c("8", "---.."),
  c("9", "----."),
  c(".", ".-.-.-"),
  c(",", "--..--"),
  c("?", "..--.."),
  c("'", ".----."),
  c("!", "-.-.--"),
  c("/", "-..-."),
  c("(", "-.--."),
  c(")", "-.--.-"),
  c("&", ".-..."),
  c(":", "---..."),
  c(";", "-.-.-."),
  c("=", "-...-"),
  c("+", ".-.-."),
  c("-", "-....-"),
  c("_", "..--.-"),
  c("\"", ".-..-."),
  c("$", "...-..-"),
  c("@", ".--.-.")
)

#' Encode a string with Morse code.
#'
#' @param string A character vector of length 1 to encode.
#' @param sep Separator between code words.
#'
#' @return Morse encoded string.
#' @export
#'
#' @examples
#' morse("a1!aa")
#' morse("a^^a")
morse <- local({
  table <- make_code_table(morse_code, from = 1, to = 2)
  make_encode(table, pre = tolower)
})

#' Decode a string with Morse code.
#'
#' @param string A character vector of length 1 to decode.
#' @param sep Separator between code words.
#'
#' @return Decoded string.
#' @export
#'
#' @examples
#' unmorse(".-/./.-", sep = "/")
#' unmorse(".-/a/.", sep = "/")
#' unmorse(morse("a^^a"))
unmorse <- local({
  table <- make_code_table(morse_code, from = 2, to = 1)
  make_decode(table, pre = tolower)
})
