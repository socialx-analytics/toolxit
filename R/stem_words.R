#' Stem Words in a Text Vector
#'
#' This function takes a vector of text and a dictionary dataframe to stem words according to the dictionary provided.
#' Each word in the text vector is replaced with its stem form based on the dictionary mapping.
#'
#' @param text_vector A character vector containing the text to be stemmed.
#' @param dictionary A dataframe that contains the mapping of words to their stemmed forms with columns named 'original' and 'stem'.
#' @return A character vector with words replaced by their stemmed forms according to the dictionary provided.
#' @export
#' @examples
#' \dontrun{
#' # Example text vector and dictionary
#' text <- c("running", "jumps", "easily", "faster")
#' dict <- data.frame(
#'   original = c("running", "jumps", "easily", "faster"),
#'   stem = c("run", "jump", "easy", "fast"),
#'   stringsAsFactors = FALSE
#' )
#' # Stem words using the dictionary
#' stemmed_text <- stem_words(text, dict)
#' }
stem_words <- function(text_vector, dictionary) {
  # Ensure that the text_vector is a character vector
  if (!is.character(text_vector)) {
    stop("text_vector must be a character vector.")
  }

  # Validate that the dictionary is a dataframe with the required columns
  if (!is.data.frame(dictionary) || !all(c("original", "stem") %in% names(dictionary))) {
    stop("dictionary must be a data frame with 'original' and 'stem' columns.")
  }

  # Use the 'hash' package to create a hash map for efficient word lookup
  hash_dict <- hash::hash(keys = dictionary$original, values = dictionary$stem)

  # Define a function to replace a word with its stem from the hash map
  find_stem <- function(word) {
    if (hash::has.key(word, hash_dict)) {
      return(hash_dict[[word]]) # Return the stemmed version if it exists
    }
    return(word) # Otherwise, return the word unchanged
  }

  # Define a function to clean up any extra spaces
  clean_spaces <- function(text) {
    return(stringi::stri_trim_both(stringi::stri_replace_all_regex(text, "\\s+", " ")))
  }

  # Split the text into individual words using stringi's boundary split
  words <- unlist(stringi::stri_split_boundaries(text_vector, type = "word"))

  # Apply the stemming function to each word
  stems <- sapply(words, find_stem)

  # Rejoin the stemmed words into a single text string, then clean up spaces
  stemmed_text <- clean_spaces(stringi::stri_join(stems, collapse = " "))

  # Return the processed text
  return(stemmed_text)
}
