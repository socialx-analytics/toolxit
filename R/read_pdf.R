#' Read PDF File into Text in R
#'
#' Reads the content of a PDF file and converts it to text. It can handle both single and multi-page documents and allows selection of specific pages.
#'
#' @param file_path The path to the PDF file.
#' @param pages A vector of pages to read in (default is NULL, indicating all pages).
#' @param text_format Logical; if TRUE, returns the text in a single string, if FALSE, returns a list with each page's content as an element.
#' @return If text_format is TRUE, a single string containing the text of the specified pages; if FALSE, a list with each element representing a page's text.
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'document.pdf' is in the current working directory and has at least 3 pages
#' text <- read_pdf("document.pdf", pages = 1:3, text_format = TRUE)
#' print(text)
#' }
read_pdf <- function(file_path, pages = NULL, text_format = TRUE) {
  # Verify that the file exists at the provided path
  if (!file.exists(file_path)) {
    stop("The file does not exist at the specified path.")
  }

  # Use the 'pdftools' package to read the PDF content
  text <- pdftools::pdf_text(file_path)

  # Subset to the requested pages if 'pages' parameter is provided
  if (!is.null(pages)) {
    # Check that 'pages' parameter is valid
    if (!is.numeric(pages) || any(pages < 1)) {
      stop("Pages parameter should be NULL or a vector of positive integers.")
    }
    # Ensure requested pages are within the document's range
    if (max(pages) > length(text)) {
      stop("One or more requested pages exceed the number of pages in the document.")
    }
    text <- text[pages] # Subset the text to the requested pages
  }

  # If text_format is TRUE, convert the text to a single string
  if (text_format) {
    text <- paste(text, collapse = " ") # Combine text from all pages
    text <- gsub("-\n", "", text) # Remove hyphenation at the end of lines
    text <- gsub("\n", " ", text) # Replace newlines with spaces
    text <- gsub("\\s+", " ", text) # Remove extra spaces
  }

  # Return the text content in the specified format
  return(text)
}
