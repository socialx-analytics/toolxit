#' Split Text Into Elements
#'
#' This function splits text into smaller elements based on the specified granularity: paragraphs, sentences, or words.
#'
#' @param text A character string containing the text to be split.
#' @param by A character string indicating the granularity of the split: 'paragraph', 'sentence', or 'word'.
#' @return A character vector with the text split into the specified elements.
#' @examples
#' text_example <- "This is a sentence. And another one! Here's the third one?\n\nNew paragraph."
#'
#' # Split by sentences
#' split_text(text_example, by = "sentence")
#'
#' # Split by paragraphs
#' split_text(text_example, by = "paragraph")
#'
#' # Split by words
#' split_text(text_example, by = "word")
#'
#' @export
#' @note Splitting by 'sentence' may not be perfect for complex text structures with abbreviations, numbers, etc.
#' For more robust sentence tokenization, consider using a natural language processing library.
split_text <- function(text, by = "paragraph") {
  # Check that 'text' is a valid non-null character string
  if (!is.character(text) || is.null(text)) {
    stop("The 'text' argument must be a non-null string.")
  }

  # Select the appropriate pattern for splitting based on the 'by' parameter
  pattern <- switch(by,
    "paragraph" = "\n\n+", # Pattern for splitting by paragraphs
    "sentence" = "(?<=[.!?])\\s+", # Pattern for splitting by sentences
    "word" = "\\s+", # Pattern for splitting by words
    stop("Invalid 'by' argument: must be 'paragraph', 'sentence', or 'word'")
  )

  # Use the pattern to split the text
  splits <- strsplit(text, pattern, perl = TRUE)

  # Define a helper function to clean up whitespace
  squish <- function(x) {
    gsub("\\s+", " ", trimws(x)) # Trim and reduce multiple spaces to single
  }

  # Clean each element from the splits to remove extra whitespaces
  splits <- lapply(splits, squish)

  # Convert the list of splits to a single character vector
  splits <- unlist(splits)

  # Filter out any empty strings that may have resulted from the split
  splits <- splits[splits != ""]

  # Return the processed split text
  return(splits)
}
